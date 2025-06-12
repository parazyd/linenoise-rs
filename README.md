linenoise-rs
============

Docs at <https://docs.rs/linenoise-rs/latest/linenoise_rs/>.
See [examples/linenoise.rs](examples/linenoise.rs) for usage.

Rust port of [linenoise](https://github.com/antirez/linenoise).
Should also have proper UTF-8 support.

A minimal, zero-config, BSD licensed, readline replacement.

## Features

* Single and multi line editing mode with the usual key bindings.
* History handling.
* Completion.
* Hints (suggestions at the right of the prompt as you type).
* Multiplexing mode, with prompt hiding/restoring for async output.
* About ~1000 lines of source code.
* Only uses a subset of VT100 escapes (ANSI.SYS compatible).

## Key bindings

| Key                 | Description                                                     |
| --------------------|-----------------------------------------------------------------|
| `Tab`               | Triggers completion callback if set, cycles through completions |
| `Ctrl+A` or `Home`  | Move cursor to beginning of line                                |
| `Ctrl+E` or `End`   | Move cursor to end of line                                      |
| `Ctrl+B` or `Left`  | Move cursor one character left                                  |
| `Ctrl+F` or `Right` | Move cursor one character right                                 |
| `Ctrl+U`            | Deletes line                                                    |
| `Ctrl+K`            | Deletes from cursor to end of line                              |
| `Ctrl+W`            | Deletes word to the left of cursor                              |
| `Ctrl+T`            | Swaps character at cursor with previous character               |
| `Ctrl+D`            | Deletes character at cursor, or returns EOF if line is empty    |
| `Ctrl+P` or `Up`    | Previous history entry                                          |
| `Ctrl+N` or `Down`  | Next history entry                                              |
| `Ctrl+C`            | Interrupt input and return                                      |
| `Ctrl+L`            | Clear screen                                                    |


## Quickstart

```rust
use linenoise_rs::{linenoise, linenoise_mask_mode_disable, linenoise_mask_mode_enable};

fn main() {
    loop {
        let Some(input) = linenoise("> ") else { break };

        match input.as_str() {
            ":mask" => {
                linenoise_mask_mode_enable();
                continue;
            }
            ":unmask" => {
                linenoise_mask_mode_disable();
                continue;
            }
            "help" => {
                println!(r"Commands:");
                println!(r"  :mask    - Enable mask mode");
                println!(r"  :unmask  - Disable mask mode");
                println!(r"  help     - Show this help");
                println!(r"  quit     - Exit the program");
                continue;
            }
            "quit" => {
                break;
            }
            _ => {}
        }

        println!("echo: {}", input);
    }
}
```


## License

BSD-2-Clause.
