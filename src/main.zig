const std = @import("std");
const repl = @import("repl.zig");

pub fn main() !void {
    const stdIn = std.io.getStdIn();
    const stdOut = std.io.getStdOut();

    std.debug.print("Welcome to the Monkey programming language!\n", .{});
    std.debug.print("Feel free to type in commands.\n", .{});

    try repl.start(stdIn.reader(), stdOut.writer());
}
