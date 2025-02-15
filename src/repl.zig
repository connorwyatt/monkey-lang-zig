const std = @import("std");
const os = @import("builtin").os;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;

const PROMPT = ">> ";

pub fn start(reader: anytype, writer: anytype) !void {
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

        var token = lexer.nextToken();
        while (!std.mem.eql(u8, token.type, Token.EOF)) : (token = lexer.nextToken()) {
            try std.fmt.format(
                writer,
                "{{ type = \"{s}\", literal = \"{s}\" }}\n",
                .{ token.type, token.literal },
            );
        }
    }
}
