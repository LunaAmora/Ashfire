use std::process::{Child, ChildStdin, ChildStdout, Command, Output};

use log::info;

pub fn command_info(cmd: &Command) -> String {
    format!("{cmd:?}").replace('\"', "")
}

/// Creates a new [`Command`][Command] with the given arguments,
/// prints its content, executes it, then waits for the child process to finish.
#[macro_export]
macro_rules! cmd_wait {
    ($cmd:expr) => {
        info!("[CMD] {}", (format!("{:?}", $cmd).trim_matches('"')));
        std::process::Command::new($cmd).spawn()?.wait()?
    };
    ($cmd:expr, $($arg:expr),*) => {
        let mut __cmd = std::process::Command::new($cmd);
        __cmd$(.arg($arg))*;
        info!("[CMD] {}", $crate::cmd::command_info(&__cmd));
        __cmd.spawn()?.wait()?
    };
}

pub struct ChildGuard {
    pub(crate) guard: Option<Child>,
    pub(crate) info: String,
}

impl ChildGuard {
    pub fn new(child: Child, info: String) -> Self {
        Self { guard: Some(child), info }
    }

    pub fn take(mut self) -> Child {
        self.guard.take().expect("Failed to take the `Child`")
    }

    pub fn stdin(&mut self) -> Option<ChildStdin> {
        match &mut self.guard {
            Some(child) => child.stdin.take(),
            None => None,
        }
    }

    pub fn stdout(&mut self) -> Option<ChildStdout> {
        match &mut self.guard {
            Some(child) => child.stdout.take(),
            None => None,
        }
    }

    pub fn wait_with_output(self) -> std::io::Result<Output> {
        info!("[CMD] | {}", self.info);
        self.take().wait_with_output()
    }

    pub fn wait_with_result(self) -> anyhow::Result<()> {
        let out = self.wait_with_output()?;

        if out.status.success() {
            Ok(())
        } else {
            anyhow::bail!("{}", String::from_utf8_lossy(&out.stderr))
        }
    }
}

impl Drop for ChildGuard {
    fn drop(&mut self) {
        if let Some(mut child) = self.guard.take() &&
            matches!(child.try_wait(), Ok(None)) &&
            let Err(e) = child.kill()
        {
            eprintln!("Could not kill child process: {e}");
        }
    }
}

/// Creates a new [`Command`][std::process::Command] with the given arguments
/// and configured pipes, spawns it and returns inside an `kill on drop` struct.
#[macro_export]
macro_rules! cmd_piped {
    ($cmd:expr, $($arg:expr),* ) => {{
        let mut cmd = std::process::Command::new($cmd);
        cmd
            $(.arg($arg))*
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());

        let info = $crate::cmd::command_info(&cmd);
        $crate::cmd::ChildGuard::new(cmd.spawn()?, info)
    }};

    ($cmd:expr, $($arg:expr),* => $stdout:expr) => {{
        let mut cmd = std::process::Command::new($cmd);
        cmd
            $(.arg($arg))*
            .stdin(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .stdout($stdout);

        let info = $crate::cmd::command_info(&cmd);
        $crate::cmd::ChildGuard::new(cmd.spawn()?, info)
    }};

    ($stdin:expr => $cmd:expr, $($arg:expr),* ) => {{
        let mut cmd = std::process::Command::new($cmd);
        cmd
            $(.arg($arg))*
            .stderr(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stdin($stdin);

        let info = $crate::cmd::command_info(&cmd);
        $crate::cmd::ChildGuard::new(cmd.spawn()?, info)
    }};
}
