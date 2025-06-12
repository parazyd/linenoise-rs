use std::sync::Arc;

use linenoise_rs::*;

#[derive(Debug, Clone)]
struct CmdNode {
    children: std::collections::HashMap<String, CmdNode>,
}

impl CmdNode {
    fn new() -> Self {
        CmdNode {
            children: std::collections::HashMap::new(),
        }
    }

    fn hints(&self, buf: &str) -> Option<(String, i32, bool)> {
        if buf.is_empty() {
            return None;
        }

        let parts: Vec<&str> = buf.split_whitespace().collect();
        if parts.is_empty() {
            return self.first_level_hint();
        }

        // Check if buffer ends with space - means last word is complete
        let ends_with_space = buf.ends_with(' ');

        self.find_hints(&parts, 0, ends_with_space)
    }

    fn find_hints(
        &self,
        parts: &[&str],
        depth: usize,
        ends_with_space: bool,
    ) -> Option<(String, i32, bool)> {
        if depth >= parts.len() {
            // We've consumed all parts, suggest next level
            return self.suggest_children("");
        }

        let current = parts[depth];

        // Check if we're at the last part
        if depth == parts.len() - 1 {
            if ends_with_space {
                // The last word is complete, look for exact match and suggest its children
                if let Some(child) = self.children.get(current) {
                    return child.suggest_children("");
                }
                return None;
            } else {
                // The last word might be partial or complete
                // First check if it's an exact match with children
                if let Some(child) = self.children.get(current) {
                    // Exact match exists - show its first child as a hint
                    // As an edge case we should prefix a whitespace to make
                    // it look better.
                    if let Some((hint, color, bold)) = child.first_level_hint() {
                        return Some((format!(" {hint}"), color, bold));
                    }
                    return None;
                }
                // Not an exact match, try to complete it
                return self.suggest_children(current);
            }
        }

        // Not at the last part, traverse deeper if exact match exists
        if let Some(child) = self.children.get(current) {
            return child.find_hints(parts, depth + 1, ends_with_space);
        }

        // No exact match found
        None
    }

    fn suggest_children(&self, prefix: &str) -> Option<(String, i32, bool)> {
        // Find all children that start with the prefix
        let mut matches: Vec<&String> = self
            .children
            .keys()
            .filter(|k| k.starts_with(prefix))
            .collect();

        if matches.is_empty() {
            return None;
        }

        // Sort for consistent ordering
        matches.sort();

        if matches.len() == 1 {
            // Single match - complete it
            let completion = &matches[0][prefix.len()..];
            if !completion.is_empty() {
                return Some((completion.to_string(), 35, false));
            }
        } else {
            // Multiple matches - show first one as hint
            let first_match = matches[0];
            let completion = &first_match[prefix.len()..];
            if !completion.is_empty() {
                return Some((completion.to_string(), 35, false));
            }
        }

        None
    }

    fn first_level_hint(&self) -> Option<(String, i32, bool)> {
        let mut keys: Vec<&String> = self.children.keys().collect();
        keys.sort();

        if let Some(first) = keys.first() {
            return Some(((*first).clone(), 35, false));
        }

        None
    }
}

/// Main macro for building command trees
macro_rules! cmds {
    // Entry point
    ( $($cmd:literal => { $($inner:tt)* }),* $(,)? ) => {{
        let mut root = CmdNode::new();
        $(
            cmds!(@insert root, $cmd => { $($inner)* });
        )*
        root
    }};

    // Insert a command with children
    (@insert $node:expr, $cmd:literal => { $($inner:tt)* }) => {{
        let child = cmds!(@build { $($inner)* });
        $node.children.insert($cmd.to_string(), child);
    }};

    // Build a node from its children
    (@build { $($cmd:literal => { $($inner:tt)* }),* $(,)? }) => {{
        #[allow(unused_mut)]
        let mut node = CmdNode::new();
        $(
            cmds!(@insert node, $cmd => { $($inner)* });
        )*
        node
    }};

    // Handle empty node
    (@build { }) => {{
        CmdNode::new()
    }};
}

thread_local! {
    static COMMANDS: std::cell::RefCell<Option<Arc<CmdNode>>> = std::cell::RefCell::new(None);
}

fn set_commands(commands: CmdNode) {
    COMMANDS.with(|c| {
        *c.borrow_mut() = Some(Arc::new(commands));
    });
}

fn hints_tls(buf: &str) -> Option<(String, i32, bool)> {
    COMMANDS.with(|c| c.borrow().as_ref().and_then(|commands| commands.hints(buf)))
}

fn main() {
    let commands = cmds! {
        "git" => {
            "checkout" => {
                "-b" => {}
            },
            "commit" => {
                "-m" => {},
                "--amend" => {}
            },
            "status" => {}
        },
        "cargo" => {
            "build" => {
                "--release" => {},
                "--features" => {}
            },
            "test" => {}
        },
        "/mask" => {},
        "/unmask" => {}
    };

    // Test various inputs
    assert!(commands.hints("g").is_some()); // Should complete to "it"
    assert!(commands.hints("git").is_some()); // Should suggest subcommands
    assert!(commands.hints("git ch").is_some()); // Should complete "eckout"
    assert!(commands.hints("git checkout").is_some()); // Should suggest "-b" or "<branch>"

    set_commands(commands.clone());
    linenoise_set_hints_callback(hints_tls);

    while let Some(input) = linenoise("> ") {
        match input.as_str() {
            "quit" => break,
            "/mask" => linenoise_mask_mode_enable(),
            "/unmask" => linenoise_mask_mode_disable(),
            _ => {}
        }

        println!("echo: {input}");
    }
}
