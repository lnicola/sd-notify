use std::env;
use std::fmt::{self, Display, Formatter, Write};
use std::fs;
use std::io;
use std::os::unix::net::UnixDatagram;

#[derive(Clone, Debug)]
pub enum NotifyState {
    Ready,
    Reloading,
    Stopping,
    Status(String),
    Errno(i32),
    BusError(String),
    MainPid(i32),
    Watchdog,
    WatchdogTrigger,
    WatchdogUsec(u32),
    ExtendTimeoutUsec(u32),
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

pub fn sd_booted() -> bool {
    fs::symlink_metadata("/run/systemd/system")
        .map(|m| m.is_dir())
        .unwrap_or(false)
}

pub fn sd_notify(unset_env: bool, state: &[NotifyState]) -> io::Result<()> {
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

        super::sd_notify(false, &[NotifyState::Ready]).unwrap();
        assert_eq!(s.recv_string(), "READY=1\n");
        assert!(env::var_os("NOTIFY_SOCKET").is_some());

        super::sd_notify(
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
