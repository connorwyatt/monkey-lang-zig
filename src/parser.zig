const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;

pub const Parser = struct {
    allocator: Allocator,
    lexer: *Lexer,
    current_token: Token,
    peek_token: Token,
    errors: std.ArrayList([]const u8),

    pub fn init(allocator: Allocator, lexer: *Lexer) Parser {
        var parser = Parser{
            .allocator = allocator,
            .lexer = lexer,
            .current_token = undefined,
            .peek_token = undefined,
            .errors = std.ArrayList([]const u8).init(allocator),
        };

        // Read two tokens, so current_token and peek_token are both set.
        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    pub fn allocParseProgram(self: *Parser, allocator: Allocator) !ast.Program {
        var statements = std.ArrayList(ast.Statement).init(allocator);
        defer statements.deinit();

        while (!self.currentTokenIs(Token.EOF)) : (self.nextToken()) {
            if (try self.parseStatement()) |statement| {
                try statements.append(statement);
            }
        }

        const statement_items = try allocator.dupe(ast.Statement, statements.items);

        return ast.Program.init(allocator, statement_items);
    }

    fn nextToken(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    fn parseStatement(self: *Parser) !?ast.Statement {
        if (self.currentTokenIs(Token.LET)) {
            const let_statement = try self.parseLetStatement() orelse return null;
            return ast.Statement{ .let_statement = let_statement };
        } else {
            return null;
        }
    }

    fn parseLetStatement(self: *Parser) !?ast.LetStatement {
        const let_token = self.current_token;

        if (!try self.expectPeek(Token.IDENT)) {
            return null;
        }

        const name = ast.Identifier{
            .token = self.current_token,
            .value = self.current_token.literal,
        };

        if (!try self.expectPeek(Token.ASSIGN)) {
            return null;
        }

        // TODO: We're skipping the expressions until we encounter a semicolon

        while (!self.currentTokenIs(Token.SEMICOLON)) : (self.nextToken()) {}

        return ast.LetStatement{ .token = let_token, .name = name, .value = undefined };
    }

    fn currentTokenIs(self: *const Parser, token_type: []const u8) bool {
        return std.mem.eql(u8, self.current_token.type, token_type);
    }

    fn peekTokenIs(self: *const Parser, token_type: []const u8) bool {
        return std.mem.eql(u8, self.peek_token.type, token_type);
    }

    fn expectPeek(self: *Parser, token_type: []const u8) !bool {
        if (self.peekTokenIs(token_type)) {
            self.nextToken();
            return true;
        } else {
            try self.peekError(token_type);
            return false;
        }
    }

    fn peekError(self: *Parser, token_type: []const u8) !void {
        const message = try std.fmt.allocPrint(
            self.allocator,
            "expected next token to be {s}, found {s} instead",
            .{ token_type, self.peek_token.type },
        );
        try self.errors.append(message);
    }

    fn deinit(self: *const Parser) void {
        for (self.errors.items) |e| {
            self.allocator.free(e);
        }
        self.errors.deinit();
    }
};

fn expectNoParserErrors(parser: *const Parser) !void {
    const testing = std.testing;
    const errors = parser.errors.items;

    try testing.expectEqual(0, errors.len);
}

test "LetStatements" {
    const testing = std.testing;

    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    const expected_statements = [_]struct { expected_identifier: []const u8 }{
        .{ .expected_identifier = "x" },
        .{ .expected_identifier = "y" },
        .{ .expected_identifier = "foobar" },
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(testing.allocator, &lexer);
    defer parser.deinit();

    const program = try parser.allocParseProgram(testing.allocator);
    defer program.deinit();

    try expectNoParserErrors(&parser);

    try testing.expectEqual(expected_statements.len, program.statements.len);

    for (expected_statements, program.statements) |es, ps| {
        try testing.expectEqualStrings("let", ps.let_statement.tokenLiteral());
        try testing.expectEqualStrings(es.expected_identifier, ps.let_statement.name.value);
    }
}
