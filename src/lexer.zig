const std = @import("std");
const Token = @import("token.zig").Token;

const Lexer = struct {
    input: []const u8,
    position: usize = 0,
    read_position: usize = 0,
    character: u8 = 0,

    const Self = @This();

    fn init(input: []const u8) Self {
        var self = Self{ .input = input };
        self.readCharacter();
        return self;
    }

    fn nextToken(self: *Self) Token {
        self.skipWhitespace();

        const token: Token = switch (self.character) {
            '=' => .{
                .type = Token.ASSIGN,
                .literal = self.input[self.position..self.read_position],
            },
            ';' => .{
                .type = Token.SEMICOLON,
                .literal = self.input[self.position..self.read_position],
            },
            '(' => .{
                .type = Token.LPAREN,
                .literal = self.input[self.position..self.read_position],
            },
            ')' => .{
                .type = Token.RPAREN,
                .literal = self.input[self.position..self.read_position],
            },
            ',' => .{
                .type = Token.COMMA,
                .literal = self.input[self.position..self.read_position],
            },
            '+' => .{
                .type = Token.PLUS,
                .literal = self.input[self.position..self.read_position],
            },
            '{' => .{
                .type = Token.LBRACE,
                .literal = self.input[self.position..self.read_position],
            },
            '}' => .{
                .type = Token.RBRACE,
                .literal = self.input[self.position..self.read_position],
            },
            0 => .{ .type = Token.EOF, .literal = "" },
            else => blk: {
                if (isLetter(self.character)) {
                    const literal = self.readIdentifier();
                    return .{
                        .type = Token.lookupIdent(literal),
                        .literal = literal,
                    };
                } else if (isDigit(self.character)) {
                    return .{ .type = Token.INT, .literal = self.readNumber() };
                } else {
                    break :blk .{
                        .type = Token.ILLEGAL,
                        .literal = self.input[self.position..self.read_position],
                    };
                }
            },
        };

        self.readCharacter();

        return token;
    }

    fn readCharacter(self: *Self) void {
        self.character = if (self.read_position >= self.input.len)
            0
        else
            self.input[self.read_position];

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn readIdentifier(self: *Self) []const u8 {
        const start_position = self.position;
        while (isLetter(self.character)) {
            self.readCharacter();
        }
        return self.input[start_position..self.position];
    }

    fn readNumber(self: *Self) []const u8 {
        const start_position = self.position;
        while (isDigit(self.character)) {
            self.readCharacter();
        }
        return self.input[start_position..self.position];
    }

    fn skipWhitespace(self: *Self) void {
        while (self.character == ' ' or
            self.character == '\t' or
            self.character == '\n' or
            self.character == '\r')
        {
            self.readCharacter();
        }
    }
};

fn isLetter(character: u8) bool {
    return ('a' <= character and character <= 'z') or ('A' <= character and character <= 'Z') or character == '_';
}

fn isDigit(character: u8) bool {
    return '0' <= character and character <= '9';
}

test "1.3 basic test" {
    const testing = std.testing;

    const input: []const u8 = "=+(){},;";

    const expected_output = [_]Token{
        .{ .type = Token.ASSIGN, .literal = "=" },
        .{ .type = Token.PLUS, .literal = "+" },
        .{ .type = Token.LPAREN, .literal = "(" },
        .{ .type = Token.RPAREN, .literal = ")" },
        .{ .type = Token.LBRACE, .literal = "{" },
        .{ .type = Token.RBRACE, .literal = "}" },
        .{ .type = Token.COMMA, .literal = "," },
        .{ .type = Token.SEMICOLON, .literal = ";" },
        .{ .type = Token.EOF, .literal = "" },
    };

    var lexer = Lexer.init(input);

    for (expected_output) |expected_output_value| {
        const output = lexer.nextToken();
        try testing.expectEqualStrings(expected_output_value.type, output.type);
        try testing.expectEqualStrings(expected_output_value.literal, output.literal);
    }
}

test "1.3 extended test" {
    const testing = std.testing;

    const input: []const u8 =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\
        \\let result = add(five, ten);
    ;

    const expected_output = [_]Token{
        .{ .type = Token.LET, .literal = "let" },
        .{ .type = Token.IDENT, .literal = "five" },
        .{ .type = Token.ASSIGN, .literal = "=" },
        .{ .type = Token.INT, .literal = "5" },
        .{ .type = Token.SEMICOLON, .literal = ";" },
        .{ .type = Token.LET, .literal = "let" },
        .{ .type = Token.IDENT, .literal = "ten" },
        .{ .type = Token.ASSIGN, .literal = "=" },
        .{ .type = Token.INT, .literal = "10" },
        .{ .type = Token.SEMICOLON, .literal = ";" },
        .{ .type = Token.LET, .literal = "let" },
        .{ .type = Token.IDENT, .literal = "add" },
        .{ .type = Token.ASSIGN, .literal = "=" },
        .{ .type = Token.FUNCTION, .literal = "fn" },
        .{ .type = Token.LPAREN, .literal = "(" },
        .{ .type = Token.IDENT, .literal = "x" },
        .{ .type = Token.COMMA, .literal = "," },
        .{ .type = Token.IDENT, .literal = "y" },
        .{ .type = Token.RPAREN, .literal = ")" },
        .{ .type = Token.LBRACE, .literal = "{" },
        .{ .type = Token.IDENT, .literal = "x" },
        .{ .type = Token.PLUS, .literal = "+" },
        .{ .type = Token.IDENT, .literal = "y" },
        .{ .type = Token.SEMICOLON, .literal = ";" },
        .{ .type = Token.RBRACE, .literal = "}" },
        .{ .type = Token.SEMICOLON, .literal = ";" },
        .{ .type = Token.LET, .literal = "let" },
        .{ .type = Token.IDENT, .literal = "result" },
        .{ .type = Token.ASSIGN, .literal = "=" },
        .{ .type = Token.IDENT, .literal = "add" },
        .{ .type = Token.LPAREN, .literal = "(" },
        .{ .type = Token.IDENT, .literal = "five" },
        .{ .type = Token.COMMA, .literal = "," },
        .{ .type = Token.IDENT, .literal = "ten" },
        .{ .type = Token.RPAREN, .literal = ")" },
        .{ .type = Token.SEMICOLON, .literal = ";" },
        .{ .type = Token.EOF, .literal = "" },
    };

    var lexer = Lexer.init(input);

    for (expected_output) |expected_output_value| {
        const output = lexer.nextToken();
        try testing.expectEqualStrings(expected_output_value.type, output.type);
        try testing.expectEqualStrings(expected_output_value.literal, output.literal);
    }
}
