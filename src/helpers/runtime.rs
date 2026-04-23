use std::time::{Duration, Instant};

pub fn gib_to_bytes(gib: u64) -> u64 {
    gib.saturating_mul(1024)
        .saturating_mul(1024)
        .saturating_mul(1024)
}

pub fn elapsed_ms(started: Instant) -> f64 {
    started.elapsed().as_secs_f64() * 1000.0
}

pub fn duration_to_ms(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1000.0
}

pub fn duration_from_ms(ms: f64) -> Duration {
    Duration::from_secs_f64(ms / 1000.0)
}

pub fn current_peak_memory_bytes() -> u64 {
    let mut usage = std::mem::MaybeUninit::<libc::rusage>::uninit();
    let rc = unsafe { libc::getrusage(libc::RUSAGE_SELF, usage.as_mut_ptr()) };
    if rc != 0 {
        return 0;
    }
    let usage = unsafe { usage.assume_init() };
    #[cfg(target_os = "macos")]
    {
        usage.ru_maxrss as u64
    }
    #[cfg(not(target_os = "macos"))]
    {
        (usage.ru_maxrss as u64) * 1024
    }
}

#[cfg(unix)]
pub fn run_with_timeout_payload<P, F, T>(timeout_secs: Option<u64>, work: F, on_timeout: T) -> P
where
    P: serde::Serialize + serde::de::DeserializeOwned,
    F: FnOnce() -> P,
    T: Fn() -> P,
{
    let Some(timeout_secs) = timeout_secs else {
        return work();
    };

    let mut fds = [0; 2];
    if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
        return work();
    }

    let pid = unsafe { libc::fork() };
    if pid == 0 {
        unsafe {
            libc::close(fds[0]);
        }
        let payload = work();
        let bytes = serde_json::to_vec(&payload).unwrap_or_default();
        unsafe {
            libc::write(fds[1], bytes.as_ptr().cast(), bytes.len());
            libc::close(fds[1]);
            libc::_exit(0);
        }
    }

    unsafe {
        libc::close(fds[1]);
    }

    let started = Instant::now();
    let mut status = 0;
    loop {
        let wait_result = unsafe { libc::waitpid(pid, &mut status, libc::WNOHANG) };
        if wait_result == pid {
            let mut payload = Vec::new();
            let mut buffer = [0u8; 4096];
            loop {
                let read_bytes =
                    unsafe { libc::read(fds[0], buffer.as_mut_ptr().cast(), buffer.len()) };
                if read_bytes <= 0 {
                    break;
                }
                payload.extend_from_slice(&buffer[..read_bytes as usize]);
            }
            unsafe {
                libc::close(fds[0]);
            }
            return serde_json::from_slice(&payload).unwrap_or_else(|_| on_timeout());
        }

        if started.elapsed() >= Duration::from_secs(timeout_secs) {
            unsafe {
                libc::kill(pid, libc::SIGKILL);
                libc::waitpid(pid, &mut status, 0);
                libc::close(fds[0]);
            }
            return on_timeout();
        }

        std::thread::sleep(Duration::from_millis(10));
    }
}

#[cfg(not(unix))]
pub fn run_with_timeout_payload<P, F, T>(_timeout_secs: Option<u64>, work: F, _on_timeout: T) -> P
where
    P: serde::Serialize + serde::de::DeserializeOwned,
    F: FnOnce() -> P,
    T: Fn() -> P,
{
    work()
}
