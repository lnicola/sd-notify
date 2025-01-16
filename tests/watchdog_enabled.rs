// There must only be one test in this file because of the thread-unsafety of
// environment variable acceses.

use std::env;
use std::process;
use std::time::Duration;

#[test]
fn watchdog_enabled() {
    // test original logic: https://github.com/systemd/systemd/blob/f3376ee8fa28aab3f7edfad1ddfbcceca5bc841c/src/libsystemd/sd-daemon/sd-daemon.c#L632

    // invalid pid and unset env
    unsafe {
        env::set_var("WATCHDOG_USEC", "5");
        env::set_var("WATCHDOG_PID", "1");
    }

    unsafe {
        assert_eq!(sd_notify::watchdog_enabled_and_unset_env(), None);
    }

    assert!(env::var_os("WATCHDOG_USEC").is_none());
    assert!(env::var_os("WATCHDOG_PID").is_none());

    // invalid usec and no unset env
    unsafe {
        env::set_var("WATCHDOG_USEC", "invalid-usec");
        env::set_var("WATCHDOG_PID", process::id().to_string());
    }

    unsafe {
        assert_eq!(sd_notify::watchdog_enabled_and_unset_env(), None);
    }

    assert!(env::var_os("WATCHDOG_USEC").is_none());
    assert!(env::var_os("WATCHDOG_PID").is_none());

    // no usec, no pip no unset env
    assert_eq!(sd_notify::watchdog_enabled(), None);

    assert!(env::var_os("WATCHDOG_USEC").is_none());
    assert!(env::var_os("WATCHDOG_PID").is_none());

    // valid pip
    unsafe {
        env::set_var("WATCHDOG_USEC", "5");
        env::set_var("WATCHDOG_PID", process::id().to_string());
    }

    assert_eq!(sd_notify::watchdog_enabled(), Some(Duration::from_micros(5)));
    assert!(env::var_os("WATCHDOG_USEC").is_some());
    assert!(env::var_os("WATCHDOG_PID").is_some());
}
