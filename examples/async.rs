use std::io;
use std::time::Duration;

use futures::{select, FutureExt};
use smol::{channel, Timer};

use linenoise_rs::*;

async fn output_generator(tx: channel::Sender<String>) {
    let mut counter = 0;

    loop {
        Timer::after(Duration::from_secs(1)).await;

        let msg = format!("Async output {}.", counter);
        counter += 1;

        if tx.send(msg).await.is_err() {
            // Receiver dropped, exit
            break;
        }
    }
}

// Async readline that integrates with output channel
async fn async_readline(prompt: &str, output_rx: channel::Receiver<String>) -> Option<String> {
    let mut state = match LinenoiseState::edit_start(-1, -1, prompt) {
        Ok(s) => s,
        Err(_) => return None,
    };

    // Set stdin to non-blocking mode.
    // We need this to be able to poll stdin without blocking.
    unsafe {
        let flags = libc::fcntl(0, libc::F_GETFL, 0);
        if libc::fcntl(0, libc::F_SETFL, flags | libc::O_NONBLOCK) < 0 {
            eprintln!("Failed to set non-blocking mode");
            return None;
        }
    }

    // Ensure we restore blocking mode on exit
    struct CleanupGuard;
    impl Drop for CleanupGuard {
        fn drop(&mut self) {
            unsafe {
                let flags = libc::fcntl(0, libc::F_GETFL, 0);
                libc::fcntl(0, libc::F_SETFL, flags & !libc::O_NONBLOCK);
            }
        }
    }
    let _guard = CleanupGuard;

    let result = async {
        loop {
            // Future that polls stdin for input
            let input_future = async {
                loop {
                    match state.edit_feed() {
                        Ok(Some(line)) => return Some(line),
                        Ok(None) => return None, // EOF
                        Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                            // No data available, yield and retry
                            Timer::after(Duration::from_millis(10)).await;
                            continue;
                        }
                        Err(e) if e.kind() == io::ErrorKind::Interrupted => {
                            // Ctrl-C
                            return None;
                        }
                        Err(_) => {
                            // Other error, keep trying
                            Timer::after(Duration::from_millis(10)).await;
                            continue;
                        }
                    }
                }
            };

            // Future that receives from output channel
            let output_future = output_rx.recv();

            select! {
                // Input ready
                line = input_future.fuse() => {
                    return line;
                }

                // Async output ready
                msg = output_future.fuse() => {
                    match msg {
                        Ok(text) => {
                            // Hide prompt, print output, show prompt again
                            let _ = state.hide();
                            println!("{}", text);
                            let _ = state.show();
                        }
                        Err(_) => {
                            // Channel closed
                            return None;
                        }
                    }
                }
            }
        }
    }
    .await;

    // Clean up linenoise state
    let _ = state.edit_stop();

    result
}

// Main async function
async fn async_main() -> io::Result<()> {
    println!("Async readline example with smol");
    println!("Press Ctrl-C or Ctrl-D to exit");

    // Create channel for async output
    let (tx, rx) = channel::unbounded();

    // Spawn background task
    let _output_task = smol::spawn(output_generator(tx));

    // Main loop
    loop {
        match async_readline("hello> ", rx.clone()).await {
            Some(line) => {
                if !line.is_empty() {
                    println!("echo: '{}'", line);
                }
            }
            None => {
                println!("\nExiting...");
                break;
            }
        }
    }

    Ok(())
}

fn main() -> io::Result<()> {
    smol::block_on(async_main())
}
