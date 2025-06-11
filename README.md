linenoisers
===========

Rust port of [linenoise](https://github.com/antirez/linenoise).
Should also have proper UTF-8 support.

See [linenoise-example.rs](linenoise-example.rs) for usage.

## Key bindings

| Comb     | Description                          |
|----------|--------------------------------------|
| `Ctrl+A` | Move to beginning of line            |
| `Ctrl+E` | Move to end of line                  |
| `Ctrl+K` | Kill text from cursor to end of line |
| `Ctrl+U` | Kill text from beginning to cursor   |
| `Ctrl+L` | Clear screen                         |
| `Ctrl+W` | Delete previous word                 |
| `Tab`    | Completion                           |
| `Ctrl+C` | Exit                                 |
| `Ctrl+D` | Exit (on empty line) or delete char  |


## License

BSD-2-Clause
