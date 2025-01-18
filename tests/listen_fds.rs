// There must only be one test in this file because of the thread-unsafety of
// environment variable acceses.

use std::env;

#[test]
fn listen_fds() {
    // We are not testing the success case because `fd_cloexec` would fail.

    unsafe {
        assert!(sd_notify::listen_fds_and_unset_env().unwrap().next().is_none());

        env::set_var("LISTEN_PID", "1");
        env::set_var("LISTEN_FDS", "1");
        assert!(sd_notify::listen_fds_and_unset_env().unwrap().next().is_none());
    }
    assert!(env::var_os("LISTEN_PID").is_none());
    assert!(env::var_os("LISTEN_FDS").is_none());

    unsafe {
        env::set_var("LISTEN_PID", "no way");
        env::set_var("LISTEN_FDS", "1");
        assert!(sd_notify::listen_fds_and_unset_env().is_err());
    }
    assert!(env::var_os("LISTEN_PID").is_none());
    assert!(env::var_os("LISTEN_FDS").is_none());
}
