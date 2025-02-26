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
    prefix_parse_fns: std.StringHashMap(*const PrefixParseFn),
    infix_parse_fns: std.StringHashMap(*const InfixParseFn),

    pub fn init(allocator: Allocator, lexer: *Lexer) !Parser {
        var parser = Parser{
            .allocator = allocator,
            .lexer = lexer,
            .current_token = undefined,
            .peek_token = undefined,
            .errors = std.ArrayList([]const u8).init(allocator),
            .prefix_parse_fns = std.StringHashMap(*const PrefixParseFn).init(allocator),
            .infix_parse_fns = std.StringHashMap(*const InfixParseFn).init(allocator),
        };
        try parser.registerPrefix(Token.IDENT, Parser.parseIdentifier);
        try parser.registerPrefix(Token.INT, Parser.parseIntegerLiteral);

        // Read two tokens, so current_token and peek_token are both set.
        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        for (self.errors.items) |e| {
            self.allocator.free(e);
        }
        self.errors.deinit();
        self.prefix_parse_fns.deinit();
        self.infix_parse_fns.deinit();
    }

    pub fn allocParseProgram(
        self: *Parser,
        allocator: Allocator,
    ) !ast.Program {
        var statements = std.ArrayList(ast.Statement).init(allocator);
        defer statements.deinit();

        while (!self.currentTokenIs(Token.EOF)) : (self.nextToken()) {
            if (try self.parseStatement()) |statement| {
                try statements.append(statement);
            }
        }

        return ast.Program.init(allocator, statements.items);
    }

    pub fn registerPrefix(
        self: *Parser,
        token_type: []const u8,
        prefix_parse_fn: PrefixParseFn,
    ) !void {
        try self.prefix_parse_fns.putNoClobber(token_type, prefix_parse_fn);
    }

    pub fn registerInfix(
        self: *Parser,
        token_type: []const u8,
        infix_parse_fn: InfixParseFn,
    ) void {
        try self.infix_parse_fns.putNoClobber(token_type, infix_parse_fn);
    }

    fn nextToken(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    fn parseStatement(self: *Parser) !?ast.Statement {
        if (self.currentTokenIs(Token.LET)) {
            const let_statement =
                try self.parseLetStatement() orelse return null;

            return try ast.Statement.init(self.allocator, .{
                .let_statement = let_statement,
            });
        } else if (self.currentTokenIs(Token.RETURN)) {
            const return_statement =
                try self.parseReturnStatement() orelse return null;

            return try ast.Statement.init(self.allocator, .{
                .return_statement = return_statement,
            });
        } else {
            const expression_statement = try self.parseExpressionStatement();

            return try ast.Statement.init(self.allocator, .{
                .expression_statement = expression_statement,
            });
        }
    }

    fn parseLetStatement(self: *Parser) !?ast.LetStatement {
        const let_token = self.current_token;

        if (!try self.expectPeek(Token.IDENT)) {
            return null;
        }

        const name = try ast.Identifier.init(
            self.allocator,
            self.current_token,
            self.current_token.literal,
        );

        if (!try self.expectPeek(Token.ASSIGN)) {
            return null;
        }

        // TODO: We're skipping the expressions until we encounter a semicolon

        while (!self.currentTokenIs(Token.SEMICOLON)) : (self.nextToken()) {}

        return try ast.LetStatement.init(
            self.allocator,
            let_token,
            name,
            undefined,
        );
    }

    fn parseReturnStatement(self: *Parser) !?ast.ReturnStatement {
        const return_token = self.current_token;

        self.nextToken();

        while (!self.currentTokenIs(Token.SEMICOLON)) : (self.nextToken()) {}

        return try ast.ReturnStatement.init(self.allocator, return_token, undefined);
    }

    fn parseExpressionStatement(self: *Parser) !ast.ExpressionStatement {
        const expression_token = self.current_token;

        const expression = try self.parseExpression(Precedence.LOWEST);

        if (self.peekTokenIs(Token.SEMICOLON)) {
            self.nextToken();
        }

        return try ast.ExpressionStatement.init(
            self.allocator,
            expression_token,
            expression,
        );
    }

    fn parseExpression(self: *Parser, _: Precedence) !?ast.Expression {
        const prefix_fn =
            self.prefix_parse_fns.get(self.current_token.type) orelse
            return null;

        return prefix_fn(self);
    }

    fn parseIdentifier(self: *Parser) !?ast.Expression {
        return try ast.Expression.init(self.allocator, .{
            .identifier = try ast.Identifier.init(
                self.allocator,
                self.current_token,
                self.current_token.literal,
            ),
        });
    }

    fn parseIntegerLiteral(self: *Parser) !?ast.Expression {
        const value = std.fmt.parseInt(i64, self.current_token.literal, 10) catch {
            // TODO: Work out if there is a nicer way to handle these errors.
            const message = std.fmt.allocPrint(
                self.allocator,
                "could not parse {s} as an integer",
                .{self.current_token.literal},
            ) catch return null;
            self.errors.append(message) catch return null;
            return null;
        };

        return try ast.Expression.init(self.allocator, .{
            .integer_literal = try ast.IntegerLiteral.init(
                self.allocator,
                self.current_token,
                value,
            ),
        });
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
};

const Precedence = enum(u8) {
    LOWEST = 1,
    EQUALS, // ==
    LESSGREATER, // > or <
    SUM, // +
    PRODUCT, // *
    PREFIX, // -X or !X
    CALL, // myFunction(X)
};

const PrefixParseFn = fn (parser: *Parser) Allocator.Error!?ast.Expression;

const InfixParseFn = fn (
    parser: *Parser,
    left_side: ast.Expression,
) Allocator.Error!?ast.Expression;

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
    var parser = try Parser.init(testing.allocator, &lexer);
    defer parser.deinit();

    const program = try parser.allocParseProgram(testing.allocator);
    defer program.deinit();

    try expectNoParserErrors(&parser);

    try testing.expectEqual(expected_statements.len, program.statements.len);

    for (expected_statements, program.statements) |es, ps| {
        try testing.expectEqualStrings(
            "let",
            ps.subtype.let_statement.tokenLiteral(),
        );
        try testing.expectEqualStrings(
            es.expected_identifier,
            ps.subtype.let_statement.name.value,
        );
    }
}

test "ReturnStatements" {
    const testing = std.testing;

    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var lexer = Lexer.init(input);
    var parser = try Parser.init(testing.allocator, &lexer);
    defer parser.deinit();

    const program = try parser.allocParseProgram(testing.allocator);
    defer program.deinit();

    try expectNoParserErrors(&parser);

    try testing.expectEqual(3, program.statements.len);

    for (program.statements) |ps| {
        try testing.expectEqualStrings(
            "return",
            ps.subtype.return_statement.tokenLiteral(),
        );
    }
}

test "IdentifierExpressions" {
    const testing = std.testing;

    const input = "foobar;";

    var lexer = Lexer.init(input);
    var parser = try Parser.init(testing.allocator, &lexer);
    defer parser.deinit();

    const program = try parser.allocParseProgram(testing.allocator);
    defer program.deinit();

    try expectNoParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const statement = program.statements[0];
    try testing.expect(statement.subtype.* == .expression_statement);

    const expression = statement.subtype.expression_statement.expression.*.?;
    try testing.expect(expression.subtype.* == .identifier);

    const identifier = expression.subtype.identifier;

    try testing.expectEqualStrings("foobar", identifier.value);
    try testing.expectEqualStrings("foobar", identifier.tokenLiteral());
}

test "IntegerLiteralExpression" {
    const testing = std.testing;

    const input = "5;";

    var lexer = Lexer.init(input);
    var parser = try Parser.init(testing.allocator, &lexer);
    defer parser.deinit();

    const program = try parser.allocParseProgram(testing.allocator);
    defer program.deinit();

    try expectNoParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const statement = program.statements[0];
    try testing.expect(statement.subtype.* == .expression_statement);

    const expression = statement.subtype.expression_statement.expression.*.?;
    try testing.expect(expression.subtype.* == .integer_literal);

    const integer_literal = expression.subtype.integer_literal;

    try testing.expectEqual(5, integer_literal.value);
    try testing.expectEqualStrings("5", integer_literal.tokenLiteral());
}
