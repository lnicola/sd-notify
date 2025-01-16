#![deny(missing_docs)]

//! Lightweight crate for interacting with `systemd`.
//!
//! This crate can be used to send service readiness or state change notifications
//! to `systemd` or compatible service managers. It doesn't offer the full
//! functionality of `libsystemd`, but is pure-Rust and has no dependencies.
//!
//! For bindings to the native `libsystemd` library, see the [`systemd`][systemd]
//! crate. For a more complete Rust reimplementation of `libsystemd` API, see the
//! [`libsystemd`][libsystemd] crate.
//!
//! [systemd]: https://crates.io/crates/systemd
//! [libsystemd]: https://crates.io/crates/libsystemd
//!
//! # Example
//!
//! ```no_run
//! # use sd_notify::NotifyState;
//! #
//! let _ = sd_notify::notify(&[NotifyState::Ready]);
//! ```

use std::convert::TryFrom;
use std::env;
use std::fmt::{self, Display, Formatter, Write};
use std::fs;
use std::ffi::OsStr;
use std::io::{self, ErrorKind};
use std::mem::MaybeUninit;
#[cfg(feature = "fdstore")]
use std::os::fd::BorrowedFd;
use std::os::unix::io::RawFd;
use std::os::unix::net::UnixDatagram;
use std::process;
use std::str::FromStr;
use std::time::Duration;

use libc::CLOCK_MONOTONIC;

/// Daemon notification for the service manager.
#[derive(Clone, Debug)]
pub enum NotifyState<'a> {
    /// Service startup is finished.
    Ready,
    /// Service is reloading its configuration.
    ///
    /// On systemd v253 and newer, this message MUST be followed by a
    /// [`NotifyState::MonotonicUsec`] notification, or the reload will fail
    /// and the service will be terminated.
    Reloading,
    /// Service is stopping.
    Stopping,
    /// Free-form status message for the service manager.
    Status(&'a str),
    /// Service has failed with an `errno`-style error code, e.g. `2` for `ENOENT`.
    Errno(u32),
    /// Service has failed with a D-Bus-style error code, e.g. `org.freedesktop.DBus.Error.TimedOut`.
    BusError(&'a str),
    /// Main process ID (PID) of the service, in case it wasn't started directly by the service manager.
    MainPid(u32),
    /// Tells the service manager to update the watchdog timestamp.
    Watchdog,
    /// Tells the service manager to trigger a watchdog failure.
    WatchdogTrigger,
    /// Resets the configured watchdog value.
    WatchdogUsec(u32),
    /// Tells the service manager to extend the service timeout.
    ExtendTimeoutUsec(u32),
    /// Tells the service manager to store attached file descriptors.
    #[cfg(feature = "fdstore")]
    FdStore,
    /// Tells the service manager to remove stored file descriptors.
    #[cfg(feature = "fdstore")]
    FdStoreRemove,
    /// Tells the service manager to use this name for the attached file descriptor.
    #[cfg(feature = "fdstore")]
    FdName(&'a str),
    /// Notify systemd of the current monotonic time in microseconds.
    /// You can construct this value by calling [`NotifyState::monotonic_usec_now()`].
    MonotonicUsec(i128),
    /// Custom state.
    Custom(&'a str),
}

impl Display for NotifyState<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            NotifyState::Ready => write!(f, "READY=1"),
            NotifyState::Reloading => write!(f, "RELOADING=1"),
            NotifyState::Stopping => write!(f, "STOPPING=1"),
            NotifyState::Status(msg) => write!(f, "STATUS={}", msg),
            NotifyState::Errno(err) => write!(f, "ERRNO={}", err),
            NotifyState::BusError(addr) => write!(f, "BUSERROR={}", addr),
            NotifyState::MainPid(pid) => write!(f, "MAINPID={}", pid),
            NotifyState::Watchdog => write!(f, "WATCHDOG=1"),
            NotifyState::WatchdogTrigger => write!(f, "WATCHDOG=trigger"),
            NotifyState::WatchdogUsec(usec) => write!(f, "WATCHDOG_USEC={}", usec),
            NotifyState::ExtendTimeoutUsec(usec) => write!(f, "EXTEND_TIMEOUT_USEC={}", usec),
            #[cfg(feature = "fdstore")]
            NotifyState::FdStore => write!(f, "FDSTORE=1"),
            #[cfg(feature = "fdstore")]
            NotifyState::FdStoreRemove => write!(f, "FDSTOREREMOVE=1"),
            #[cfg(feature = "fdstore")]
            NotifyState::FdName(name) => write!(f, "FDNAME={}", name),
            NotifyState::MonotonicUsec(usec) => write!(f, "MONOTONIC_USEC={}", usec),
            NotifyState::Custom(state) => write!(f, "{}", state),
        }
    }
}

impl NotifyState<'_> {
    /// Create a new [`NotifyState::MonotonicUsec`] using the current system monotonic time.
    ///
    /// # Example
    ///
    /// ```
    /// # use sd_notify::NotifyState;
    /// #
    /// let _ = NotifyState::monotonic_usec_now().expect("Failed to read monotonic time");
    /// ```
    pub fn monotonic_usec_now() -> io::Result<Self> {
        monotonic_time_usec().map(NotifyState::MonotonicUsec)
    }
}

/// Checks whether the system has been booted by `systemd`.
///
/// This is implemented by verifying that a `/run/systemd/system` directory exists.
///
/// # Example
///
/// ```no_run
/// let _ = sd_notify::booted();
/// ```
pub fn booted() -> io::Result<bool> {
    let m = fs::symlink_metadata("/run/systemd/system")?;
    Ok(m.is_dir())
}

// Constants for env variable names so that we don't typo them.
const LISTEN_FDNAMES: &str = "LISTEN_FDNAMES";
const LISTEN_FDS: &str = "LISTEN_FDS";
const LISTEN_PID: &str = "LISTEN_PID";
const NOTIFY_SOCKET: &str = "NOTIFY_SOCKET";
const WATCHDOG_PID: &str = "WATCHDOG_PID";
const WATCHDOG_USEC: &str = "WATCHDOG_USEC";

/// Sends the service manager a list of state changes.
///
/// See [`sd_notify(3)`][sd_notify] for details.
///
/// [sd_notify]: https://www.freedesktop.org/software/systemd/man/sd_notify.html
///
/// # Limitations
///
/// The implementation of this function is somewhat naive: it doesn't support
/// sending notifications on behalf of other processes, doesn't send credentials,
/// and does not increase the send buffer size. It's still useful, though, in
/// usual situations.
///
/// If you wish to send file descriptors, use the `notify_with_fds` function.
///
/// # Example
///
/// ```no_run
/// # use sd_notify::NotifyState;
/// #
/// let _ = sd_notify::notify(&[NotifyState::Ready]);
/// ```
pub fn notify(state: &[NotifyState]) -> io::Result<()> {
    let Some(socket_path) = env::var_os(NOTIFY_SOCKET) else {
        return Ok(());
    };
    let mut msg = String::new();
    for s in state {
        writeln!(msg, "{}", s).unwrap();
    }
    let sock = connected_unix_datagram(&socket_path)?;
    let len = sock.send(msg.as_bytes())?;
    if len != msg.len() {
        Err(io::Error::new(ErrorKind::WriteZero, "incomplete write"))
    } else {
        Ok(())
    }
}

/// Sends the service manager a list of state changes.
///
/// This does the same as the [`notify`] and unsets the `NOTIFY_SOCKET`
/// environment variable afterwards, so that child processes no longer inherit
/// the variable.
///
/// # Safety
///
/// Since this function calls [`std::env::remove_var`], it has the same safety
/// preconditions. See its safety documentation for more details. It can only
/// be safely called before threads are spawned, in particular before any
/// `tokio` runtime initialization or `#[tokio::main]`.
pub unsafe fn notify_and_unset_env(state: &[NotifyState]) -> io::Result<()> {
    let result = notify(state);
    unsafe {
        env::remove_var(NOTIFY_SOCKET);
    }
    result
}

/// Sends the service manager a list of state changes with file descriptors.
///
/// The notification mechanism involves sending a datagram to a Unix domain socket.
/// See [`sd_pid_notify_with_fds(3)`][sd_pid_notify_with_fds] for details.
///
/// [sd_pid_notify_with_fds]: https://www.freedesktop.org/software/systemd/man/sd_notify.html
///
/// # Limitations
///
/// The implementation of this function is somewhat naive: it doesn't support
/// sending notifications on behalf of other processes, doesn't send credentials,
/// and does not increase the send buffer size. It's still useful, though, in
/// usual situations.
///
/// # Example
///
/// ```no_run
/// # use sd_notify::NotifyState;
/// # use std::os::fd::BorrowedFd;
/// #
/// # let fd = unsafe { BorrowedFd::borrow_raw(0) };
/// #
/// let _ = sd_notify::notify_with_fds(&[NotifyState::FdStore], &[fd]);
/// ```
#[cfg(feature = "fdstore")]
pub fn notify_with_fds(
    state: &[NotifyState],
    fds: &[BorrowedFd<'_>],
) -> io::Result<()> {
    use sendfd::SendWithFd;

    let Some(socket_path) = env::var_os(NOTIFY_SOCKET) else {
        return Ok(());
    };
    let mut msg = String::new();
    for s in state {
        writeln!(msg, "{}", s).unwrap();
    }
    let sock = connected_unix_datagram(&socket_path)?;
    let len = sock.send_with_fd(msg.as_bytes(), borrowed_fd_slice(fds))?;
    if len != msg.len() {
        Err(io::Error::new(ErrorKind::WriteZero, "incomplete write"))
    } else {
        Ok(())
    }
}

/// Sends the service manager a list of state changes with file descriptors.
///
/// This does the same as the [`notify_with_fds`] and unsets the
/// `NOTIFY_SOCKET` environment variable afterwards, so that child processes no
/// longer inherit the variable.
///
/// # Safety
///
/// Since this function calls [`std::env::remove_var`], it has the same safety
/// preconditions. See its safety documentation for more details. It can only
/// be safely called before threads are spawned, in particular before any
/// `tokio` runtime initialization or `#[tokio::main]`.
#[cfg(feature = "fdstore")]
pub unsafe fn notify_with_fds_and_unset_env(
    state: &[NotifyState],
    fds: &[BorrowedFd<'_>],
) -> io::Result<()> {
    let result = notify_with_fds(state, fds);
    unsafe {
        env::remove_var(NOTIFY_SOCKET);
    }
    result
}

#[cfg(feature = "fdstore")]
fn borrowed_fd_slice<'a>(s: &'a [BorrowedFd<'_>]) -> &'a [RawFd] {
    use std::slice;

    // SAFETY: BorrowedFd is #[repr(transparent)] over RawFd (memory safety)
    // and implements AsRawFd (lifetime safety).
    // Required only because sendfd does not have i/o safety traits.
    unsafe { slice::from_raw_parts(s.as_ptr() as *const RawFd, s.len()) }
}

fn connected_unix_datagram(path: &OsStr) -> io::Result<UnixDatagram> {
    let result = UnixDatagram::unbound()?;
    result.connect(path)?;
    Ok(result)
}

/// Checks for file descriptors passed by the service manager for socket
/// activation.
///
/// The function returns an iterator over file descriptors, starting from
/// `SD_LISTEN_FDS_START`. The number of descriptors is obtained from the
/// `LISTEN_FDS` environment variable.
///
/// Before returning, the file descriptors are set as `O_CLOEXEC`.
///
/// See [`sd_listen_fds(3)`][sd_listen_fds] for details.
///
/// [sd_listen_fds]: https://www.freedesktop.org/software/systemd/man/sd_listen_fds.html
///
/// # Example
///
/// ```no_run
/// let socket = sd_notify::listen_fds().map(|mut fds| fds.next().expect("missing fd"));
/// ```
pub fn listen_fds() -> io::Result<impl ExactSizeIterator<Item = RawFd>> {
    let listen_pid = if let Ok(pid) = env::var(LISTEN_PID) {
        pid
    } else {
        return Ok(0..0);
    }
    .parse::<u32>()
    .map_err(|_| io::Error::new(ErrorKind::InvalidInput, "invalid LISTEN_PID"))?;

    if listen_pid != process::id() {
        return Ok(0..0);
    }

    let listen_fds = if let Ok(fds) = env::var(LISTEN_FDS) {
        fds
    } else {
        return Ok(0..0);
    }
    .parse::<u32>()
    .map_err(|_| io::Error::new(ErrorKind::InvalidInput, "invalid LISTEN_FDS"))?;

    let overflow = || io::Error::new(ErrorKind::InvalidInput, "fd count overflowed");

    const SD_LISTEN_FDS_START: u32 = 3;
    let last = SD_LISTEN_FDS_START
        .checked_add(listen_fds)
        .ok_or_else(overflow)?;

    for fd in SD_LISTEN_FDS_START..last {
        fd_cloexec(fd)?
    }

    let last = RawFd::try_from(last).map_err(|_| overflow())?;
    let listen_fds = SD_LISTEN_FDS_START as RawFd..last;
    Ok(listen_fds)
}

/// Checks for file descriptors passed by the service manager for socket
/// activation.
///
/// This does the same as the [`listen_fds`] and unsets the `LISTEN_FDS` and
/// `LISTEN_PID` environment variables afterwards, so that child processes no
/// longer inherit the variable.
///
/// # Safety
///
/// Since this function calls [`std::env::remove_var`], it has the same safety
/// preconditions. See its safety documentation for more details. It can only
/// be safely called before threads are spawned, in particular before any
/// `tokio` runtime initialization or `#[tokio::main]`.
pub unsafe fn listen_fds_and_unset_env() -> io::Result<impl ExactSizeIterator<Item = RawFd>> {
    let result = listen_fds();
    unsafe {
        env::remove_var(LISTEN_PID);
        env::remove_var(LISTEN_FDS);
    }
    result
}

/// Checks for file descriptors passed by the service manager for socket
/// activation.
///
/// The function returns an iterator over file descriptors, starting from
/// `SD_LISTEN_FDS_START`. The number of descriptors is obtained from the
/// `LISTEN_FDS` environment variable.
///
/// Before returning, the file descriptors are set as `O_CLOEXEC`.
///
/// See [`sd_listen_fds_with_names(3)`][sd_listen_fds_with_names] for details.
///
/// [sd_listen_fds_with_names]: https://www.freedesktop.org/software/systemd/man/sd_listen_fds.html
///
/// # Example
///
/// ```no_run
/// let socket = sd_notify::listen_fds().map(|mut fds| fds.next().expect("missing fd"));
/// ```
pub fn listen_fds_with_names() -> io::Result<impl ExactSizeIterator<Item = (RawFd, String)>> {
    let listen_fds = listen_fds()?;
    zip_fds_with_names(listen_fds, env::var(LISTEN_FDNAMES).ok())
}

/// Checks for file descriptors passed by the service manager for socket
/// activation.
///
/// This does the same as the [`listen_fds_with_names`] and unsets the
/// `LISTEN_FDNAMES`, `LISTEN_FDS` and `LISTEN_PID` environment variables
/// afterwards, so that child processes no longer inherit the variable.
///
/// # Safety
///
/// Since this function calls [`std::env::remove_var`], it has the same safety
/// preconditions. See its safety documentation for more details. It can only
/// be safely called before threads are spawned, in particular before any
/// `tokio` runtime initialization or `#[tokio::main]`.
pub unsafe fn listen_fds_with_names_and_unset_env() -> io::Result<impl ExactSizeIterator<Item = (RawFd, String)>> {
    let result = listen_fds_with_names();
    unsafe {
        env::remove_var(LISTEN_PID);
        env::remove_var(LISTEN_FDS);
        env::remove_var(LISTEN_FDNAMES);
    }
    result
}

/// Internal helper that is independent of listen_fds function, for testing purposes.
fn zip_fds_with_names(
    listen_fds: impl ExactSizeIterator<Item = RawFd>,
    listen_fdnames: Option<String>,
) -> io::Result<impl ExactSizeIterator<Item = (RawFd, String)>> {
    let listen_fdnames = if let Some(names) = listen_fdnames {
        // systemd shouldn't provide an empty fdname element. However if it does, the
        // sd_listen_fds_with_names function will return an empty string for that fd,
        // as in the following C example:
        //
        // void main() {
        //  char **names;
        //  setenv("LISTEN_FDNAMES", "x::z", 1);
        //  int n = sd_listen_fds_with_names(0, &names);
        //  assert(*names[1] == 0);
        // }
        names.split(':').map(|x| x.to_owned()).collect::<Vec<_>>()
    } else {
        let mut names = vec![];
        names.resize(listen_fds.len(), "unknown".to_string());
        names
    };

    if listen_fdnames.len() == listen_fds.len() {
        Ok(listen_fds.zip(listen_fdnames))
    } else {
        Err(io::Error::new(
            ErrorKind::InvalidInput,
            "invalid LISTEN_FDNAMES",
        ))
    }
}

fn fd_cloexec(fd: u32) -> io::Result<()> {
    let fd = RawFd::try_from(fd).map_err(|_| io::Error::from_raw_os_error(libc::EBADF))?;
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFD, 0) };
    if flags < 0 {
        return Err(io::Error::last_os_error());
    }
    let new_flags = flags | libc::FD_CLOEXEC;
    if new_flags != flags {
        let r = unsafe { libc::fcntl(fd, libc::F_SETFD, new_flags) };
        if r < 0 {
            return Err(io::Error::last_os_error());
        }
    }
    Ok(())
}

/// Asks the service manager for enabled watchdog.
///
/// See [`sd_watchdog_enabled(3)`][sd_watchdog_enabled] for details.
///
/// [sd_watchdog_enabled]: https://www.freedesktop.org/software/systemd/man/sd_watchdog_enabled.html
///
/// # Example
///
/// ```no_run
/// # use sd_notify;
/// #
/// if let Some(duration) = sd_notify::watchdog_enabled() {
///     // watchdog enabled with `duration`
/// } else {
///     // watchdog disabled
/// }
/// ```
pub fn watchdog_enabled() -> Option<Duration> {
    let s = env::var(WATCHDOG_USEC)
        .ok()
        .and_then(|s| u64::from_str(&s).ok());
    let p = env::var(WATCHDOG_PID)
        .ok()
        .and_then(|s| u32::from_str(&s).ok());

    match (s, p) {
        (Some(usec_val), Some(pid)) if pid == process::id() => {
            Some(Duration::from_micros(usec_val))
        }
        _ => None,
    }
}

/// Asks the service manager for enabled watchdog.
///
/// This does the same as the [`watchdog_enabled`] and unsets the
/// `WATCHDOG_PID` and `WATCHDOG_USEC` environment variables afterwards, so
/// that child processes no longer inherit the variable.
///
/// # Safety
///
/// Since this function calls [`std::env::remove_var`], it has the same safety
/// preconditions. See its safety documentation for more details. It can only
/// be safely called before threads are spawned, in particular before any
/// `tokio` runtime initialization or `#[tokio::main]`.
pub unsafe fn watchdog_enabled_and_unset_env() -> Option<Duration> {
    let result = watchdog_enabled();
    unsafe {
        env::remove_var(WATCHDOG_USEC);
        env::remove_var(WATCHDOG_PID);
    }
    result
}

fn monotonic_time_usec() -> io::Result<i128> {
    let mut timespec = MaybeUninit::uninit();
    let rv = unsafe { libc::clock_gettime(CLOCK_MONOTONIC, timespec.as_mut_ptr()) };
    if rv != 0 {
        return Err(io::Error::last_os_error());
    }
    let timespec = unsafe { timespec.assume_init() };

    // nanoseconds / 1_000 -> microseconds.
    let lower_msec = (timespec.tv_nsec / 1_000) as i128;
    // seconds * 1_000_000 -> microseconds
    let upper_msec = (timespec.tv_sec * 1_000_000) as i128;
    Ok(upper_msec + lower_msec)
}

#[cfg(test)]
mod tests {
    use std::os::fd::RawFd;

    #[test]
    fn listen_fds_with_names() {
        assert_eq!(
            super::zip_fds_with_names(3 as RawFd..4 as RawFd, Some("omelette".to_string()))
                .unwrap()
                .collect::<Vec<_>>(),
            vec![(3 as RawFd, "omelette".to_string())]
        );

        assert_eq!(
            super::zip_fds_with_names(
                3 as RawFd..5 as RawFd,
                Some("omelette:baguette".to_string())
            )
            .unwrap()
            .collect::<Vec<_>>(),
            vec![
                (3 as RawFd, "omelette".to_string()),
                (4 as RawFd, "baguette".to_string())
            ]
        );

        // LISTEN_FDNAMES is cleared
        assert_eq!(
            super::zip_fds_with_names(3 as RawFd..4 as RawFd, None)
                .unwrap()
                .next(),
            Some((3 as RawFd, "unknown".to_string()))
        );

        // LISTEN_FDNAMES is cleared, every fd should have the name "unknown"
        assert_eq!(
            super::zip_fds_with_names(3 as RawFd..5 as RawFd, None)
                .unwrap()
                .collect::<Vec<_>>(),
            vec![
                (3 as RawFd, "unknown".to_string()),
                (4 as RawFd, "unknown".to_string())
            ],
        );

        // Raise an error if LISTEN_FDNAMES has a different number of entries as fds
        assert!(super::zip_fds_with_names(
            3 as RawFd..6 as RawFd,
            Some("omelette:baguette".to_string())
        )
        .is_err());
    }
}
