# Changelog

## [0.2.0] - 2021-02-18

### Changed

- (breaking) changed the `NotifyState::MainPid` and `NotifyState::Error` data from `i32` to `u32`
- (breaking) changed `listen_fds` to return `Result<u32>` instead of `Result<i32>`

### Fixed

- fixed `Display` implementation for `NotifyState::WatchdogUsec` and `NotifyState::ExtendTimeoutUsec`
- removed a stray debug print

## [0.1.1] - 2019-10-20

### Added

- `listen_fds` function for file descriptor retrieval when using socket activation

## [0.1.0] - 2019-09-22

### Added

- Initial release
