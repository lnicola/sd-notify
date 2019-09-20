#![doc(html_root_url = "https://docs.rs/sd-notify/0.1.0")]
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
//! The notification mechanism involves sending a datagram to a Unix domain socket.
//! See [`sd_notify(3)`][sd_notify] for details.
//!
//! [sd_notify]: https://www.freedesktop.org/software/systemd/man/sd_notify.html
//!
//! # Example
//!
//! ```no_run
//! # use sd_notify::NotifyState;
//! #
//! let _ = sd_notify::notify(true, &[NotifyState::Ready]);
//! ```

use std::env;
use std::fmt::{self, Display, Formatter, Write};
use std::fs;
use std::io;
use std::os::unix::net::UnixDatagram;

/// Daemon notification for the service manager.
#[derive(Clone, Debug)]
pub enum NotifyState {
    /// Service startup is finished.
    Ready,
    /// Service is reloading its configuration.
    Reloading,
    /// Service is stopping.
    Stopping,
    /// Free-form status message for the service manager.
    Status(String),
    /// Service has failed with an `errno`-style error code, e.g. `2` for `ENOENT`.
    Errno(i32),
    /// Service has failed with a D-Bus-style error code, e.g. `org.freedesktop.DBus.Error.TimedOut`.
    BusError(String),
    /// Main process ID (PID) of the service, in case it wasn't started directly by the service manager.
    MainPid(i32),
    /// Tells the service manager to update the watchdog timestamp.
    Watchdog,
    /// Tells the service manager to trigger a watchdog failure.
    WatchdogTrigger,
    /// Resets the configured watchdog value.
    WatchdogUsec(u32),
    /// Tells the service manager to extend the service timeout.
    ExtendTimeoutUsec(u32),
    /// Custom state.
    Custom(String),
}

impl Display for NotifyState {
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
            NotifyState::WatchdogUsec(_) => write!(f, "WATCHDOG_USEC={}", 0),
            NotifyState::ExtendTimeoutUsec(_) => write!(f, "EXTEND_TIMEOUT_USEC={}", 0),
            NotifyState::Custom(state) => write!(f, "{}", state),
        }
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

/// Sends the service manager a list of state changes.
///
/// If the `unset_env` parameter is set, the `NOTIFY_SOCKET` environment variable
/// will be unset before returning. Further calls to `sd_notify` will fail, but
/// chlid processes will no longer inherit the variable.
///
/// # Limitations
///
/// The implementation of this function is somewhat naive: it doesn't support
/// sending notifications on behalf of other processes, doesn't pass file
/// descriptors, doesn't send credentials, and does not increase the send
/// buffer size. It's still useful, though, in usual situations.
///
/// # Example
///
/// ```no_run
/// # use sd_notify::NotifyState;
/// #
/// let _ = sd_notify::notify(true, &[NotifyState::Ready]);
/// ```
pub fn notify(unset_env: bool, state: &[NotifyState]) -> io::Result<()> {
    let socket_path = match env::var_os("NOTIFY_SOCKET") {
        Some(path) => path,
        None => return Ok(()),
    };
    if unset_env {
        env::remove_var("NOTIFY_SOCKET");
    }

    let mut msg = String::new();
    let sock = UnixDatagram::unbound()?;
    for s in state {
        let _ = write!(msg, "{}", s);
        msg.push('\n');
    }
    sock.send_to(msg.as_bytes(), socket_path)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::NotifyState;
    use std::env;
    use std::fs;
    use std::os::unix::net::UnixDatagram;
    use std::path::PathBuf;

    struct SocketHelper(PathBuf, UnixDatagram);

    impl SocketHelper {
        pub fn recv_string(&self) -> String {
            let mut buf = [0; 1024];
            let len = self.1.recv(&mut buf).unwrap();
            String::from_utf8(Vec::from(&buf[0..len])).unwrap()
        }
    }

    impl Drop for SocketHelper {
        fn drop(&mut self) {
            let _ = fs::remove_file(&self.0);
        }
    }

    fn bind_socket() -> SocketHelper {
        let path = env::temp_dir().join("sd-notify-test-sock");
        let _ = fs::remove_file(&path);

        env::set_var("NOTIFY_SOCKET", &path);
        let sock = UnixDatagram::bind(&path).unwrap();
        SocketHelper(path, sock)
    }

    #[test]
    fn sd_notify() {
        let s = bind_socket();

        super::notify(false, &[NotifyState::Ready]).unwrap();
        assert_eq!(s.recv_string(), "READY=1\n");
        assert!(env::var_os("NOTIFY_SOCKET").is_some());

        super::notify(
            true,
            &[
                NotifyState::Status(String::from("Reticulating splines")),
                NotifyState::Watchdog,
                NotifyState::Custom(String::from("X_WORKS=1")),
            ],
        )
        .unwrap();
        assert_eq!(
            s.recv_string(),
            "STATUS=Reticulating splines\nWATCHDOG=1\nX_WORKS=1\n"
        );
        assert!(env::var_os("NOTIFY_SOCKET").is_none());
    }
}
