/* Original C example commands for reference:
 *
 * hello> /historylen 100
 * hello> /mask
 * hello> (type password - will show as ***)
 * hello> /unmask
 *
 * Special key bindings:
 * - Ctrl-A: Move to beginning of line
 * - Ctrl-E: Move to end of line
 * - Ctrl-K: Kill text from cursor to end of line
 * - Ctrl-U: Kill text from beginning to cursor
 * - Ctrl-L: Clear screen
 * - Ctrl-W: Delete previous word
 * - Ctrl-T: Transpose characters
 * - Tab: Completion
 * - Ctrl-C: Exit
 * - Ctrl-D: Exit (on empty line) or delete char
 *
 */

use std::{env, process};

use libc::{fd_set, select, timeval, FD_SET, FD_ZERO};
use linenoisers::*;

fn completion(buf: &str, lc: &mut Vec<String>) {
    if buf.starts_with("h") {
        lc.push("hello".to_string());
        lc.push("hello there".to_string());
    }
}

fn hints(buf: &str) -> Option<(String, i32, bool)> {
    if buf == "hello" {
        Some((" World".to_string(), 35, false)) // 35 = magenta
    } else {
        None
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let argv0 = &args[0];
    let mut async_mode = false;

    // Parse arguments
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--multiline" => {
                linenoise_set_multi_line(true);
                println!("Multi-line mode enabled.");
            }
            "--keycodes" => {
                linenoise_print_key_codes();
                return;
            }
            "--async" => {
                async_mode = true;
            }
            _ => {
                eprintln!("Usage: {argv0} [--multiline] [--keycodes] [--async]");
                process::exit(1);
            }
        }
        i += 1;
    }

    // Set the completion callback. This will be called every time the
    // user uses the <tab> key.
    linenoise_set_completion_callback(completion);
    linenoise_set_hints_callback(hints);

    // Load history from file. The history file is just a plain text file
    // where entries are separated by newlines.
    let _ = linenoise_history_load("history.txt");

    // Now this is the main loop of the typical linenoise-based application.
    // The call to linenoise() will block as long as the user types something
    // and presses enter.
    loop {
        let line = if !async_mode {
            linenoise("hello> ")
        } else {
            // Asynchronous mode using the multiplexing API: wait for
            // data on stdin, and simulate async data coming from some source
            // using the select(2) timeout.
            async_readline("hello> ")
        };

        match line {
            Some(line) => {
                // Do something with the string.
                if !line.is_empty() && !line.starts_with('/') {
                    println!("echo: '{line}'");
                    linenoise_history_add(&line); // Add to the history
                    let _ = linenoise_history_save("history.txt"); // Save every new entry
                } else if !line.is_empty() && line.starts_with('/') {
                    // Parse special commands
                    let parts: Vec<&str> = line.split_whitespace().collect();
                    if parts.is_empty() {
                        continue;
                    }

                    match parts[0] {
                        "/historylen" => {
                            if parts.len() > 1
                                && let Ok(len) = parts[1].parse::<usize>()
                            {
                                linenoise_history_set_max_len(len);
                            }
                        }
                        "/mask" => {
                            linenoise_mask_mode_enable();
                        }
                        "/unmask" => {
                            linenoise_mask_mode_disable();
                        }
                        _ => {
                            println!("Unreconized command: {}", parts[0]);
                        }
                    }
                } else if line.is_empty() {
                    // Empty line, do nothing
                }
            }
            None => {
                // Ctrl-D or Ctrl-C pressed
                break;
            }
        }
    }
}

fn async_readline(prompt: &str) -> Option<String> {
    let mut state = match LinenoiseState::edit_start(-1, -1, prompt) {
        Ok(s) => s,
        Err(_) => return None,
    };

    let mut counter = 0;

    loop {
        unsafe {
            let mut readfds: fd_set = std::mem::zeroed();
            FD_ZERO(&mut readfds);
            FD_SET(state.get_fd(), &mut readfds);

            let mut tv = timeval {
                tv_sec: 1, // 1 second timeout
                tv_usec: 0,
            };

            let retval = select(
                state.get_fd() + 1,
                &mut readfds,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                &mut tv,
            );

            if retval == -1 {
                eprintln!("select() error");
                let _ = state.edit_stop();
                return None;
            } else if retval > 0 {
                // Input available
                match state.edit_feed() {
                    Ok(Some(line)) => {
                        let _ = state.edit_stop();
                        return Some(line);
                    }
                    Ok(None) => {
                        // EOF
                        let _ = state.edit_stop();
                        return None;
                    }
                    Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {
                        // Ctrl-C
                        let _ = state.edit_stop();
                        return None;
                    }
                    Err(_) => {
                        // Need more input
                        continue;
                    }
                }
            } else {
                // Timeout occurred - simulate async output
                let _ = state.hide();
                println!("Async output {counter}.");
                counter += 1;
                let _ = state.show();
            }
        }
    }
}
