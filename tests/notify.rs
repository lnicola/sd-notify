// There must only be one test in this file because of the thread-unsafety of
// environment variable acceses.

use sd_notify::NotifyState;
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

unsafe fn bind_socket() -> SocketHelper {
    let path = env::temp_dir().join("sd-notify-test-sock");
    let _ = fs::remove_file(&path);

    env::set_var("NOTIFY_SOCKET", &path);
    let sock = UnixDatagram::bind(&path).unwrap();
    SocketHelper(path, sock)
}

#[test]
fn notify() {
    let s = unsafe { bind_socket() };

    sd_notify::notify(&[NotifyState::Ready]).unwrap();
    assert_eq!(s.recv_string(), "READY=1\n");
    assert!(env::var_os("NOTIFY_SOCKET").is_some());

    unsafe {
        sd_notify::notify_and_unset_env(
            &[
                NotifyState::Status("Reticulating splines"),
                NotifyState::Watchdog,
                NotifyState::Custom("X_WORKS=1"),
            ],
        )
        .unwrap();
    }
    assert_eq!(
        s.recv_string(),
        "STATUS=Reticulating splines\nWATCHDOG=1\nX_WORKS=1\n"
    );
    assert!(env::var_os("NOTIFY_SOCKET").is_none());
}
