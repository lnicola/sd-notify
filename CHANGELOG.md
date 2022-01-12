# Changelog

## [0.4.0] - 2022-01-12

### Changed

- (breaking) `NotifyState::Status`, `NotifyState::BusError` and `NotifyState::Custom` now contain a `&str` instead of a `String`

## [0.3.0] - 2021-02-25

### Changed

- (breaking) `listen_fds` now returns an iterator over `RawFd` values
- (breaking) `SD_LISTEN_FDS_START` is gone

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
