const std = @import("std");
const os = @import("builtin").os;
const evaluator = @import("evaluator.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Token = @import("token.zig").Token;

const PROMPT = ">> ";

pub fn start(reader: anytype, writer: anytype) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var buffer: [1024]u8 = undefined;

    while (true) {
        try std.fmt.format(writer, PROMPT, .{});

        const line = blk: {
            const l = try reader.readUntilDelimiter(&buffer, '\n');

            if (os.tag == .windows) {
                break :blk std.mem.trimRight(u8, l, "\r");
            }
            break :blk l;
        };

        if (line.len == 0) {
            break;
        }

        var lexer = Lexer.init(line);
        var parser = try Parser.init(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.allocParseProgram(allocator);
        defer program.deinit();

        if (parser.errors.items.len != 0) {
            try printParserErrors(writer, parser.errors.items);
            continue;
        }

        const evaluated = evaluator.eval(program.toAnyNodePointer());
        if (evaluated) |e| {
            const inspect_string = try e.inspect(allocator);
            defer allocator.free(inspect_string);
            try writer.print("{s}\n", .{inspect_string});
        }
    }
}

const MONKEY_FACE =
    \\            __,__
    \\   .--.  .-"     "-.  .--.
    \\  / .. \/  .-. .-.  \/ .. \
    \\ | |  '|  /   Y   \  |'  | |
    \\ | \   \  \ 0 | 0 /  /   / |
    \\  \ '- ,\.-"""""""-./, -' /
    \\   ''-' /_   ^ ^   _\ '-''
    \\       |  \._   _./  |
    \\       \   \ '~' /   /
    \\        '._ '-=-' _.'
    \\           '-----'
;

fn printParserErrors(writer: anytype, errors: []const []const u8) !void {
    try writer.writeAll(MONKEY_FACE);
    try writer.writeAll("\n");
    try writer.writeAll("Whoops! We ran into some monkey business here!\n");
    try writer.writeAll("  parser errors:\n");
    for (errors) |e| {
        try writer.print("    - {s}\n", .{e});
    }
}
