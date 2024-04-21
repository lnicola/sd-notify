extern "C" {
    pub fn fcntl(fd: i32, cmd: i32, ...) -> i32;
}

pub const F_GETFD: i32 = 1;
pub const F_SETFD: i32 = 2;

pub const FD_CLOEXEC: i32 = 1;

pub const EBADF: i32 = 9;
