// There must only be one test in this file because of the thread-unsafety of
// environment variable acceses.

use std::env;
use std::process;

#[test]
fn watchdog_enabled() {
    // test original logic: https://github.com/systemd/systemd/blob/f3376ee8fa28aab3f7edfad1ddfbcceca5bc841c/src/libsystemd/sd-daemon/sd-daemon.c#L632

    // invalid pid and unset env
    unsafe {
        env::set_var("WATCHDOG_USEC", "5");
        env::set_var("WATCHDOG_PID", "1");
    }

    let mut usec = 0;
    unsafe {
        assert!(!sd_notify::watchdog_enabled_and_unset_env(&mut usec));
    }
    assert_eq!(usec, 0);

    assert!(env::var_os("WATCHDOG_USEC").is_none());
    assert!(env::var_os("WATCHDOG_PID").is_none());

    // invalid usec and no unset env
    unsafe {
        env::set_var("WATCHDOG_USEC", "invalid-usec");
        env::set_var("WATCHDOG_PID", process::id().to_string());
    }

    let mut usec = 0;
    unsafe {
        assert!(!sd_notify::watchdog_enabled_and_unset_env(&mut usec));
    }
    assert_eq!(usec, 0);

    assert!(env::var_os("WATCHDOG_USEC").is_none());
    assert!(env::var_os("WATCHDOG_PID").is_none());

    // no usec, no pip no unset env
    let mut usec = 0;
    assert!(!sd_notify::watchdog_enabled(&mut usec));
    assert_eq!(usec, 0);

    assert!(env::var_os("WATCHDOG_USEC").is_none());
    assert!(env::var_os("WATCHDOG_PID").is_none());

    // valid pip
    unsafe {
        env::set_var("WATCHDOG_USEC", "5");
        env::set_var("WATCHDOG_PID", process::id().to_string());
    }

    let mut usec = 0;
    assert!(sd_notify::watchdog_enabled(&mut usec));
    assert_eq!(usec, 5);
    assert!(env::var_os("WATCHDOG_USEC").is_some());
    assert!(env::var_os("WATCHDOG_PID").is_some());
}
