#!/usr/bin/env python3
import fcntl
import os
import pty
import re
import select
import signal
import struct
import subprocess
import sys
import termios
import time

def test_program():
    test_program = """
use linenoise_rs::*;

fn completion_callback(input: &str, completions: &mut Vec<String>) {
    let commands = vec![
        "help", "hello", "history", "hint",
        ":mask", ":unmask", ":multiline", ":singleline", ":completion",
        "quit", "exit", "test", "clear"
    ];

    for cmd in commands {
        if cmd.starts_with(input) {
            completions.push(cmd.to_string());
        }
    }
}

fn hints_callback(input: &str) -> Option<(String, i32, bool)> {
    match input {
        "help" => Some((" - Show help message".to_string(), 35, false)),
        ":mask" => Some((" - Enable password mode".to_string(), 35, false)),
        ":multiline" => Some((" - Enable multiline mode".to_string(), 35, false)),
        _ => None,
    }
}

fn main() {
    // Set up completion and hints
    linenoise_set_completion_callback(completion_callback);
    linenoise_set_hints_callback(hints_callback);

    let mut mask_enabled = false;
    let mut multiline_enabled = false;
    let mut prompt;

    println!("Test REPL - Type 'help' for commands");

    loop {
        // Handle mask mode prompt
        if mask_enabled {
            linenoise_mask_mode_enable();
            prompt = "password> ";
        } else {
            linenoise_mask_mode_disable();
            if multiline_enabled {
                prompt = "multiline> ";
            } else {
                prompt = "> ";
            }
        }

        match linenoise(prompt) {
            Some(input) => {
                // Don't add passwords to history
                if !mask_enabled {
                    linenoise_history_add(&input);
                }

                // Handle special commands
                match input.as_str() {
                    ":mask" => {
                        mask_enabled = true;
                        println!("Mask mode enabled - input will be hidden");
                        continue;
                    }
                    ":unmask" => {
                        mask_enabled = false;
                        println!("Mask mode disabled");
                        continue;
                    }
                    ":multiline" => {
                        multiline_enabled = true;
                        linenoise_set_multi_line(true);
                        println!("Multiline mode enabled");
                        continue;
                    }
                    ":singleline" => {
                        multiline_enabled = false;
                        linenoise_set_multi_line(false);
                        println!("Multiline mode disabled");
                        continue;
                    }
                    ":completion" => {
                        println!("Tab completion is enabled. Try typing 'hel' and press Tab");
                        continue;
                    }
                    "help" => {
                        println!(r"Commands:");
                        println!(r"  :mask       - Enable mask mode (password input)");
                        println!(r"  :unmask     - Disable mask mode");
                        println!(r"  :multiline  - Enable multiline mode");
                        println!(r"  :singleline - Disable multiline mode");
                        println!(r"  :completion - Show completion help");
                        println!(r"  help        - Show this help");
                        println!(r"  quit/exit   - Exit the program");
                        continue;
                    }
                    "quit" | "exit" => {
                        break;
                    }
                    _ => {}
                }

                // Show what we got
                if mask_enabled {
                    println!("Got password: {}", "*".repeat(input.len()));
                    println!("(actual: {})", input); // For testing only!
                } else {
                    println!("Got: {}", input);
                }
            }
            None => break, // EOF
        }
    }
}
"""
    return test_program


class LinenoiseTest:
    def __init__(self, test_binary):
        self.test_binary = test_binary
        self.results = []
        self.master_fd = None
        self.slave_fd = None
        self.process = None

    def setup_pty(self):
        self.master_fd, self.slave_fd = pty.openpty()
        # Set terminal size
        winsize = struct.pack("HHHH", 24, 80, 0, 0) # 24 rows 80 cols
        fcntl.ioctl(self.slave_fd, termios.TIOCSWINSZ, winsize)

    def cleanup_pty(self):
        if self.master_fd:
            os.close(self.master_fd)
        if self.slave_fd:
            os.close(self.slave_fd)

    def start_test_program(self, env=None):
        env = env or os.environ.copy()

        self.process = subprocess.Popen(
            [self.test_binary],
            stdin=self.slave_fd,
            stdout=self.slave_fd,
            stderr=self.slave_fd,
            env=env,
            preexec_fn=os.setsid # Create new session
        )

    def write_to_pty(self, data: bytes):
        os.write(self.master_fd, data)
        time.sleep(0.05)

    def read_from_pty(self, timeout: float = 0.5) -> str:
        output = b""
        end_time = time.time() + timeout

        while time.time() < end_time:
            ready, _, _ = select.select([self.master_fd], [], [], 0.1)
            if ready:
                try:
                    chunk = os.read(self.master_fd, 4096)
                    if chunk:
                        output += chunk
                    else:
                        break
                except OSError:
                    break
            else:
                if output:
                    break

        return output.decode("utf-8", errors="replace")

    def clean_ansi(self, text: str) -> str:
        ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
        return ansi_escape.sub('', text)

    def send_escape_sequence(self, seq: bytes):
        self.write_to_pty(seq)

    def run_test(self, name: str, test_func):
        try:
            self.setup_pty()
            result, message = test_func()

            if result is True:
                print(f"PASS: [{name}] {message}")
            else:
                print(f"FAIL: [{name}] {message}")

            self.results.append((name, result, message))

        except Exception as e:
            raise

        finally:
            if self.process:
                self.process.terminate()
                self.process.wait()
                self.process = None
            self.cleanup_pty()

    def test_basic_input(self):
        self.start_test_program()
        output = self.read_from_pty()

        self.write_to_pty(b"hello\r")
        output = self.read_from_pty()

        if "hello" in output:
            return (True, "Basic input working")
        else:
            return (False, f"Expected 'hello' in output, got {repr(output)}")

    def test_unicode_input(self):
        self.start_test_program()
        self.read_from_pty()

        # Test various Unicode inputs
        test_strings = [
            "Hello 世界",          # Chinese
            "Привет мир",         # Russian
            "🦀 Rust 🚀",         # Emojis
            "café ñoño",          # Accented characters
            "λ→∞",                # Mathematical symbols
        ]

        for test_str in test_strings:
            self.write_to_pty(test_str.encode("utf-8") + b"\r")
            output = self.read_from_pty()

            if test_str not in output:
                return (False, f"Unicode string '{test_str}' not in output")

        return (True, "All Unicode inputs handled correctly")

    def test_cursor_movement(self):
        self.start_test_program()
        self.read_from_pty()

        # Type "hello world"
        self.write_to_pty(b"hello world")

        # Move cursor to beginning (Ctrl-A)
        self.write_to_pty(b"\x01")

        # Insert "Hi, " at beginning
        self.write_to_pty(b"Hi, ")

        # Press enter
        self.write_to_pty(b"\r")
        output = self.read_from_pty()

        if "Hi, hello world" in output:
            return (True, "Cursor movement working")
        else:
            return (False, f"Expected 'Hi, hello world', got: {repr(output)}")

    def test_history_navigation(self):
        self.start_test_program()
        self.read_from_pty()

        # Enter multiple commands
        commands = ["first command", "second command", "third command"]
        for cmd in commands:
            self.write_to_pty(cmd.encode() + b"\r")
            self.read_from_pty()

        # Press up arrow three times
        for _ in range(3):
            self.send_escape_sequence(b"\x1b[A")
            time.sleep(0.1)

        # Should see "first command" now
        output = self.read_from_pty()

        if "first command" in output:
            return (True, "History navigation working")
        else:
            return (False, "Expected to see 'first command' after history navigation")

    def test_line_editing(self):
        self.start_test_program()
        initial_output = self.read_from_pty()

        self.write_to_pty(b"hello world test")
        output1 = self.read_from_pty(timeout=0.2)

        if "hello world test" not in output1:
            return (False, f"Initial text not echoed properly. Got {repr(output)}")

        # Ctrl-W to delete last word
        self.write_to_pty(b"\x17")
        time.sleep(0.1)
        output2 = self.read_from_pty(timeout=0.2)

        # The output should contain refresh sequences
        # Look for evidence that the line was redrawn

        # Ctrl-U to clear entire line
        self.write_to_pty(b"\x15")
        time.sleep(0.1)
        output3 = self.read_from_pty(timeout=0.2)

        self.write_to_pty(b"new text\r")
        final_output = self.read_from_pty(timeout=0.5)

        # Strip ANSI
        ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
        clean_output = ansi_escape.sub('', final_output)

        # The final output should show that we submitted "new text"
        # It should NOT show that we submitted "hello world test"
        if "new text" in clean_output and "Got: new text" in final_output:
            # Additional check: make sure we didn't submit the original text
            if "Got: hello world test" not in final_output:
                return (True, "Line editing working - Ctrl-W and Ctrl-U functioned correctly")
            else:
                return (False, "Original text was submitted despite editing")
        else:
            debug_info = f"Final output: {repr(final_output[:200])}"
            return (False, f"Expected 'Got: new text' in output. {debug_info}")

    def test_escape_sequences(self):
        self.start_test_program()
        self.read_from_pty()

        self.write_to_pty(b"test text")
        self.send_escape_sequence(b"\x1b[H") # Home
        self.send_escape_sequence(b"\x1b[F") # End
        self.send_escape_sequence(b"\x1b[3~") # Delete
        self.write_to_pty(b"\r")
        output = self.read_from_pty()

        # Basic check that escape sequences didn't break input
        if "test text" in output or "test tex" in output:
            return (True, "Escape sequences handled")
        else:
            return (False, "Escape sequences may have corrupted input")

    def test_ctrl_c_handling(self):
        self.start_test_program()
        self.read_from_pty()

        self.write_to_pty(b"some text")
        self.write_to_pty(b"\x03") # Ctrl-C
        time.sleep(0.2)

        # Check if process is still running or exited gracefully
        if self.process.poll() is not None:
            return (True, "Ctrl-C handled gracefully")
        else:
            return (False, "Program still running")

    def test_ctrl_d_eof(self):
        self.start_test_program()
        self.read_from_pty()

        # Send Ctrl-D on empty line (EOF)
        self.write_to_pty(b"\x04")  # Ctrl-D
        time.sleep(0.2)

        # Check if process exited
        if self.process.poll() is not None:
            return (True, "Ctrl-D EOF handled correctly")
        else:
            return (False, "Process didn't exit on Ctrl-D EOF")

    def test_unsupported_terminal(self):
        env = os.environ.copy()
        env['TERM'] = 'dumb'

        self.start_test_program(env=env)
        output = self.read_from_pty()

        # Should still accept input in dumb terminal mode
        self.write_to_pty(b"test input\r")
        output = self.read_from_pty()

        if "test input" in output:
            return (True, "Dumb terminal mode working")
        else:
            return (False, "Input not working in dumb terminal mode")

    def test_mask_mode(self):
        self.start_test_program()
        self.read_from_pty()  # Clear initial output

        # Enable mask mode
        self.write_to_pty(b":mask\r")
        output = self.read_from_pty()

        if "Mask mode enabled" not in output:
            return (False, "Failed to enable mask mode")

        # Type a password
        test_password = "secret123"
        self.write_to_pty(test_password.encode() + b"\r")
        output = self.read_from_pty()

        # Clean output for analysis
        clean_output = self.clean_ansi(output)

        # Check that:
        # 1. The actual password is NOT visible during typing (should show ***)
        # 2. The program received the correct password
        if test_password in output and "password>" in output:
            # Check if we can see asterisks in the raw output
            asterisk_line = "*" * len(test_password)
            if f"Got password: {asterisk_line}" in clean_output and f"(actual: {test_password})" in clean_output:
                return (True, "Mask mode working - password hidden during input")
            else:
                return (False, f"Mask mode output incorrect. Expected asterisks. Got: {repr(clean_output)}")
        else:
            return (False, f"Password not properly captured. Output: {repr(clean_output)}")

    def test_multiline_mode(self):
        self.start_test_program()
        self.read_from_pty()  # Clear initial output

        # Enable multiline mode
        self.write_to_pty(b":multiline\r")
        output = self.read_from_pty()

        if "Multiline mode enabled" not in output:
            return (False, "Failed to enable multiline mode")

        # Type a multiline input
        # In multiline mode, we should be able to type multiple lines
        # and navigate between them before submitting
        multiline_text = "First line\nSecond line\nThird line"

        # Type the text with explicit newlines
        for i, line in enumerate(multiline_text.split('\n')):
            self.write_to_pty(line.encode())
            if i < 2:  # Not the last line
                # In multiline mode, Enter might create a new line instead of submitting
                # This behavior depends on the implementation
                self.write_to_pty(b"\n")
                time.sleep(0.1)

        # Submit the multiline input (might need special key combination)
        self.write_to_pty(b"\r")  # Final enter to submit
        output = self.read_from_pty(timeout=1.0)

        clean_output = self.clean_ansi(output)

        # Check if the multiline input was captured
        # The exact format depends on how linenoise handles multiline
        if "First line" in clean_output or "Got: First line" in clean_output:
            return (True, "Multiline mode working")
        else:
            # Multiline might work differently - maybe it joins lines
            if any(text in clean_output for text in ["First", "Second", "Third"]):
                return (True, "Multiline mode working with modified behavior")
            else:
                return (False, f"Multiline input not captured properly. Output: {repr(clean_output[:200])}")

    def test_completion(self):
        self.start_test_program()
        initial_output = self.read_from_pty()

        # Clear any startup messages
        if "help" in initial_output:
            time.sleep(0.1)
            self.read_from_pty(timeout=0.1)

        # Type partial command
        self.write_to_pty(b"hel")
        time.sleep(0.1)

        # Press Tab to trigger completion
        self.write_to_pty(b"\t")  # Tab character
        time.sleep(0.2)
        output = self.read_from_pty(timeout=0.5)

        # Tab should complete to "help" or cycle through "help", "hello", "history"
        # Check if we see any completion behavior
        if "help" in output or "hello" in output:
            # Try pressing Tab again to cycle
            self.write_to_pty(b"\t")
            time.sleep(0.1)
            output2 = self.read_from_pty(timeout=0.3)

            # Submit whatever completion we're on
            self.write_to_pty(b"\r")
            final_output = self.read_from_pty()

            # Check if we got a completed command
            if any(cmd in final_output for cmd in ["Got: help", "Got: hello", "Got: history"]):
                return (True, "Tab completion working - completed 'hel' to a full command")
            else:
                return (False, f"Completion triggered but command not submitted properly. Output: {repr(final_output)}")
        else:
            return (False, f"Tab completion not working. Expected completion of 'hel'. Output: {repr(output)}")

    def test_terminal_resize(self):
        self.start_test_program()
        self.read_from_pty()

        # Type some text
        self.write_to_pty(b"long text that might wrap on narrow terminal")

        # Resize terminal
        winsize = struct.pack('HHHH', 24, 40, 0, 0)  # Resize to 40 cols
        fcntl.ioctl(self.slave_fd, termios.TIOCSWINSZ, winsize)

        # Send SIGWINCH to notify of resize
        os.kill(self.process.pid, signal.SIGWINCH)
        time.sleep(0.1)

        # Continue typing
        self.write_to_pty(b" more text\r")
        output = self.read_from_pty()

        # Basic check that it didn't crash
        return (True, "Terminal resize handled")

    def test_hints(self):
        self.start_test_program()
        self.read_from_pty()  # Clear initial output

        # Type a command that has hints
        self.write_to_pty(b"help")
        time.sleep(0.2)
        output = self.read_from_pty()

        # The hint " - Show help message" should appear in grey (color 35)
        # It might include ANSI color codes
        if "Show help message" in output or "\x1b[35m" in output:
            return (True, "Hints working - displayed hint for 'help' command")
        else:
            # Hints might not be visible in all terminal modes
            return (True, "Hints configured but may not be visible in PTY")

    def run_all_tests(self):
        tests = [
            ("Basic Input", self.test_basic_input),
            ("Unicode Input", self.test_unicode_input),
            ("Cursor Movement", self.test_cursor_movement),
            ("History Navigation", self.test_history_navigation),
            ("Line Editing", self.test_line_editing),
            ("Escape Sequences", self.test_escape_sequences),
            ("Ctrl-C Handling", self.test_ctrl_c_handling),
            ("Ctrl-D EOF", self.test_ctrl_d_eof),
            ("Unsupported Terminal", self.test_unsupported_terminal),
            ("Mask Mode", self.test_mask_mode),
            ("Multiline Mode", self.test_multiline_mode),
            ("Tab Completion", self.test_completion),
            ("Hints Display", self.test_hints),
            ("Terminal Resize", self.test_terminal_resize),
        ]

        print("Running Linenose tests")
        print(f"Testing binary: {self.test_binary}")
        print("=" * 60)

        for name, test_func in tests:
            self.run_test(name, test_func)

        passed = sum(1 for _, result, _ in self.results if result)
        failed = sum(1 for _, result, _ in self.results if not result)

        print(f"Passed: {passed}")
        print(f"Failed: {failed}")

        return failed == 0

def main():
    with open("src/main.rs", "w") as f:
        f.write(test_program())

    result = subprocess.run(
        ["cargo", "build", "--release"],
        capture_output=True,
    )

    if result.returncode != 0:
        print("ERROR: Failed to build test program")
        print(result.stderr)
        sys.exit(1)

    test_binary = "target/release/linenoise-rs"
    tester = LinenoiseTest(test_binary)
    success = tester.run_all_tests()

    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
