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
        try parser.registerPrefix(Token.TRUE, Parser.parseBoolean);
        try parser.registerPrefix(Token.FALSE, Parser.parseBoolean);
        try parser.registerPrefix(Token.BANG, Parser.parsePrefixExpression);
        try parser.registerPrefix(Token.MINUS, Parser.parsePrefixExpression);
        try parser.registerPrefix(Token.LPAREN, Parser.parseGroupedExpression);
        try parser.registerPrefix(Token.IF, Parser.parseIfExpression);
        try parser.registerPrefix(Token.FUNCTION, Parser.parseFunctionLiteral);

        try parser.registerInfix(Token.PLUS, Parser.parseInfixExpression);
        try parser.registerInfix(Token.MINUS, Parser.parseInfixExpression);
        try parser.registerInfix(Token.SLASH, Parser.parseInfixExpression);
        try parser.registerInfix(Token.ASTERISK, Parser.parseInfixExpression);
        try parser.registerInfix(Token.EQ, Parser.parseInfixExpression);
        try parser.registerInfix(Token.NOT_EQ, Parser.parseInfixExpression);
        try parser.registerInfix(Token.LT, Parser.parseInfixExpression);
        try parser.registerInfix(Token.GT, Parser.parseInfixExpression);

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
    ) !void {
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

    fn parseBlockStatement(self: *Parser) !ast.BlockStatement {
        const token = self.current_token;

        var statements = std.ArrayList(ast.Statement).init(self.allocator);
        defer statements.deinit();

        self.nextToken();

        while (!self.currentTokenIs(Token.RBRACE) and
            !self.currentTokenIs(Token.EOF))
        {
            const statement = try self.parseStatement();
            if (statement) |s| {
                try statements.append(s);
            }
            self.nextToken();
        }

        return try ast.BlockStatement.init(
            self.allocator,
            token,
            statements.items,
        );
    }

    fn parseExpression(self: *Parser, precedence: Precedence) !?ast.Expression {
        const prefix_fn =
            self.prefix_parse_fns.get(self.current_token.type) orelse {
                try self.noPrefixParseFnError(self.current_token.type);
                return null;
            };

        var left = try prefix_fn(self);

        while (!self.peekTokenIs(Token.SEMICOLON) and
            @intFromEnum(precedence) < @intFromEnum(self.peekTokenPrecedence()))
        {
            const infix_fn = self.infix_parse_fns.get(self.peek_token.type) orelse {
                return left;
            };

            self.nextToken();

            left = try infix_fn(self, left.?);
        }

        return left;
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

    fn parseBoolean(self: *Parser) !?ast.Expression {
        return try ast.Expression.init(
            self.allocator,
            .{
                .boolean = try ast.Boolean.init(
                    self.allocator,
                    self.current_token,
                    self.currentTokenIs(Token.TRUE),
                ),
            },
        );
    }

    fn parsePrefixExpression(self: *Parser) !?ast.Expression {
        const prefix_expression_token = self.current_token;

        self.nextToken();

        const expression = try self.parseExpression(.PREFIX);

        const prefix_expression = try ast.PrefixExpression.init(
            self.allocator,
            prefix_expression_token,
            prefix_expression_token.literal,
            expression,
        );

        return try ast.Expression.init(self.allocator, .{
            .prefix_expression = prefix_expression,
        });
    }

    fn parseGroupedExpression(self: *Parser) !?ast.Expression {
        self.nextToken();

        const expression = self.parseExpression(.LOWEST);

        if (!try self.expectPeek(Token.RPAREN)) {
            return null;
        }

        return expression;
    }

    fn parseIfExpression(self: *Parser) !?ast.Expression {
        const token = self.current_token;

        if (!try self.expectPeek(Token.LPAREN)) {
            return null;
        }

        self.nextToken();
        const condition = try self.parseExpression(Precedence.LOWEST);

        if (!try self.expectPeek(Token.RPAREN)) {
            return null;
        }
        if (!try self.expectPeek(Token.LBRACE)) {
            return null;
        }

        const consequence = try self.parseBlockStatement();

        const alternative = blk: {
            if (self.peekTokenIs(Token.ELSE)) {
                self.nextToken();

                if (!try self.expectPeek(Token.LBRACE)) {
                    return null;
                }

                break :blk try self.parseBlockStatement();
            } else {
                break :blk null;
            }
        };

        return try ast.Expression.init(self.allocator, .{
            .if_expression = try ast.IfExpression.init(
                self.allocator,
                token,
                condition.?,
                consequence,
                alternative,
            ),
        });
    }

    fn parseFunctionLiteral(self: *Parser) !?ast.Expression {
        const token = self.current_token;

        if (!try self.expectPeek(Token.LPAREN)) {
            return null;
        }

        const parameters = try self.parseFunctionParameters();

        if (parameters == null) {
            return null;
        }

        defer self.allocator.free(parameters.?);

        if (!try self.expectPeek(Token.LBRACE)) {
            return null;
        }

        const body = try self.parseBlockStatement();

        return try ast.Expression.init(self.allocator, .{
            .function_literal = try ast.FunctionLiteral.init(
                self.allocator,
                token,
                parameters.?,
                body,
            ),
        });
    }

    fn parseFunctionParameters(self: *Parser) !?[]const ast.Identifier {
        var identifiers = std.ArrayList(ast.Identifier).init(self.allocator);
        defer identifiers.deinit();

        if (self.peekTokenIs(Token.RPAREN)) {
            self.nextToken();
            return try self.allocator.dupe(ast.Identifier, identifiers.items);
        }

        self.nextToken();

        {
            const identifier = try ast.Identifier.init(
                self.allocator,
                self.current_token,
                self.current_token.literal,
            );
            try identifiers.append(identifier);
        }

        while (self.peekTokenIs(Token.COMMA)) {
            self.nextToken();
            self.nextToken();

            const identifier = try ast.Identifier.init(
                self.allocator,
                self.current_token,
                self.current_token.literal,
            );
            try identifiers.append(identifier);
        }

        if (!try self.expectPeek(Token.RPAREN)) {
            return null;
        }

        return try self.allocator.dupe(ast.Identifier, identifiers.items);
    }

    fn parseInfixExpression(
        self: *Parser,
        left: ast.Expression,
    ) Allocator.Error!ast.Expression {
        const operator_token = self.current_token;
        const precedence = self.currentTokenPrecedence();

        self.nextToken();

        const right = try self.parseExpression(precedence);

        return ast.Expression.init(self.allocator, .{
            .infix_expression = try ast.InfixExpression.init(
                self.allocator,
                operator_token,
                left,
                operator_token.literal,
                right,
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

    fn currentTokenPrecedence(self: *Parser) Precedence {
        return precedenceForTokenType(self.current_token.type) orelse
            Precedence.LOWEST;
    }

    fn peekTokenPrecedence(self: *Parser) Precedence {
        return precedenceForTokenType(self.peek_token.type) orelse
            Precedence.LOWEST;
    }

    fn peekError(self: *Parser, token_type: []const u8) Allocator.Error!void {
        const message = try std.fmt.allocPrint(
            self.allocator,
            "expected next token to be {s}, found {s} instead",
            .{ token_type, self.peek_token.type },
        );
        try self.errors.append(message);
    }

    fn noPrefixParseFnError(
        self: *Parser,
        token_type: []const u8,
    ) Allocator.Error!void {
        const message = try std.fmt.allocPrint(
            self.allocator,
            "no prefix parse function for {s} found",
            .{token_type},
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

fn precedenceForTokenType(token_type: []const u8) ?Precedence {
    if (std.mem.eql(u8, token_type, Token.EQ)) {
        return Precedence.EQUALS;
    } else if (std.mem.eql(u8, token_type, Token.NOT_EQ)) {
        return Precedence.EQUALS;
    } else if (std.mem.eql(u8, token_type, Token.LT)) {
        return Precedence.LESSGREATER;
    } else if (std.mem.eql(u8, token_type, Token.GT)) {
        return Precedence.LESSGREATER;
    } else if (std.mem.eql(u8, token_type, Token.PLUS)) {
        return Precedence.SUM;
    } else if (std.mem.eql(u8, token_type, Token.MINUS)) {
        return Precedence.SUM;
    } else if (std.mem.eql(u8, token_type, Token.SLASH)) {
        return Precedence.PRODUCT;
    } else if (std.mem.eql(u8, token_type, Token.ASTERISK)) {
        return Precedence.PRODUCT;
    } else {
        return null;
    }
}

const PrefixParseFn = fn (parser: *Parser) Allocator.Error!?ast.Expression;

const InfixParseFn = fn (
    parser: *Parser,
    left: ast.Expression,
) Allocator.Error!ast.Expression;

fn expectNoParserErrors(parser: *const Parser) !void {
    const testing = std.testing;
    const errors = parser.errors.items;

    testing.expectEqual(0, errors.len) catch |expect_error| {
        std.debug.print("Messages:\n", .{});
        for (errors) |e| {
            std.debug.print("  - {s}\n", .{e});
        }
        return expect_error;
    };
}

fn expectIdentifier(
    identifier: *const ast.Identifier,
    expected_value: []const u8,
) !void {
    const testing = std.testing;

    try testing.expectEqualStrings(expected_value, identifier.value);
    try testing.expectEqualStrings(expected_value, identifier.tokenLiteral());
}

fn expectExpressionToBeIdentifier(
    expression: *const ast.Expression,
    expected_value: []const u8,
) !void {
    const testing = std.testing;

    try testing.expect(expression.subtype.* == .identifier);

    const identifier = expression.subtype.identifier;

    try expectIdentifier(&identifier, expected_value);
}

fn expectExpressionToBeIntegerLiteral(
    expression: *const ast.Expression,
    expected_value: i64,
) !void {
    const testing = std.testing;

    try testing.expect(expression.subtype.* == .integer_literal);

    const integer_literal = expression.subtype.integer_literal;

    try testing.expectEqual(expected_value, integer_literal.value);
    var buf: [2]u8 = undefined;
    const expected_token_literal = try std.fmt.bufPrint(
        &buf,
        "{}",
        .{expected_value},
    );
    try testing.expectEqualStrings(
        expected_token_literal,
        integer_literal.tokenLiteral(),
    );
}

fn expectExpressionToBeBoolean(
    expression: *const ast.Expression,
    expected_value: bool,
) !void {
    const testing = std.testing;

    try testing.expect(expression.subtype.* == .boolean);

    const boolean = expression.subtype.boolean;

    try testing.expectEqual(expected_value, boolean.value);

    const expected_token_literal = if (expected_value) "true" else "false";
    try testing.expectEqualStrings(
        expected_token_literal,
        boolean.tokenLiteral(),
    );
}

fn expectExpressionToBeLiteralExpression(
    expression: *const ast.Expression,
    expected: anytype,
) !void {
    const testing = std.testing;

    const type_of = @TypeOf(expected);
    const type_info = @typeInfo(type_of);
    switch (type_info) {
        .int => |x| {
            try testing.expectEqual(64, x.bits);
            try testing.expectEqual(std.builtin.Signedness.signed, x.signedness);
            try expectExpressionToBeIntegerLiteral(expression, expected);
        },
        .bool => {
            try expectExpressionToBeBoolean(expression, expected);
        },
        .pointer => |x| {
            try testing.expect(x.is_const);
            const child_type_info = @typeInfo(x.child);
            switch (child_type_info) {
                .int => |y| {
                    try testing.expectEqual(8, y.bits);
                    try testing.expectEqual(
                        std.builtin.Signedness.unsigned,
                        y.signedness,
                    );
                    try expectExpressionToBeIdentifier(expression, expected);
                },
                else => {
                    std.debug.print("InvalidType: {}", .{child_type_info});
                    return error.InvalidType;
                },
            }
        },
        else => {
            return error.InvalidType;
        },
    }
}

fn expectExpressionToBeInfixExpression(
    expression: *const ast.Expression,
    left: anytype,
    operator: []const u8,
    right: @TypeOf(left),
) !void {
    const testing = std.testing;

    try testing.expect(expression.subtype.* == .infix_expression);

    const infix_expression = expression.subtype.infix_expression;

    try expectExpressionToBeLiteralExpression(&infix_expression.left.*.?, left);

    try testing.expectEqualStrings(operator, infix_expression.operator);

    try expectExpressionToBeLiteralExpression(&infix_expression.right.*.?, right);
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

    inline for (expected_statements, program.statements) |es, ps| {
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
    try expectExpressionToBeIdentifier(&expression, "foobar");
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
    try expectExpressionToBeIntegerLiteral(&expression, 5);
}

test "BooleanExpression" {
    const testing = std.testing;

    const input = "true;";

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
    try expectExpressionToBeBoolean(&expression, true);
}

test "PrefixExpression" {
    const testing = std.testing;

    const int_test_cases = [_]struct {
        input: []const u8,
        operator: []const u8,
        value: i64,
    }{
        .{ .input = "!5;", .operator = "!", .value = 5 },
        .{ .input = "-15;", .operator = "-", .value = 15 },
    };

    const bool_test_cases = [_]struct {
        input: []const u8,
        operator: []const u8,
        value: bool,
    }{
        .{ .input = "!true;", .operator = "!", .value = true },
        .{ .input = "!false;", .operator = "!", .value = false },
    };

    const handleTestCase = struct {
        fn handleTestCase(
            input: []const u8,
            operator: []const u8,
            value: anytype,
        ) !void {
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
            try testing.expect(expression.subtype.* == .prefix_expression);

            const prefix_expression = expression.subtype.prefix_expression;

            try testing.expectEqualStrings(
                operator,
                prefix_expression.operator,
            );

            try expectExpressionToBeLiteralExpression(&prefix_expression.right.*.?, value);
        }
    }.handleTestCase;

    inline for (int_test_cases) |test_case| {
        try handleTestCase(test_case.input, test_case.operator, test_case.value);
    }

    inline for (bool_test_cases) |test_case| {
        try handleTestCase(test_case.input, test_case.operator, test_case.value);
    }
}

test "InfixExpression" {
    const testing = std.testing;

    const int_test_cases = [_]struct {
        input: []const u8,
        left_value: i64,
        operator: []const u8,
        right_value: i64,
    }{
        .{ .input = "5 + 5;", .left_value = 5, .operator = "+", .right_value = 5 },
        .{ .input = "5 - 5;", .left_value = 5, .operator = "-", .right_value = 5 },
        .{ .input = "5 * 5;", .left_value = 5, .operator = "*", .right_value = 5 },
        .{ .input = "5 / 5;", .left_value = 5, .operator = "/", .right_value = 5 },
        .{ .input = "5 > 5;", .left_value = 5, .operator = ">", .right_value = 5 },
        .{ .input = "5 < 5;", .left_value = 5, .operator = "<", .right_value = 5 },
        .{ .input = "5 == 5;", .left_value = 5, .operator = "==", .right_value = 5 },
        .{ .input = "5 != 5;", .left_value = 5, .operator = "!=", .right_value = 5 },
    };

    const bool_test_cases = [_]struct {
        input: []const u8,
        left_value: bool,
        operator: []const u8,
        right_value: bool,
    }{
        .{ .input = "true == true", .left_value = true, .operator = "==", .right_value = true },
        .{ .input = "true != false", .left_value = true, .operator = "!=", .right_value = false },
        .{ .input = "false == false", .left_value = false, .operator = "==", .right_value = false },
    };

    const handleTestCase = struct {
        fn handleTestCase(
            input: []const u8,
            left_value: anytype,
            operator: []const u8,
            right_value: @TypeOf(left_value),
        ) !void {
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

            try expectExpressionToBeInfixExpression(
                &expression,
                left_value,
                operator,
                right_value,
            );
        }
    }.handleTestCase;

    inline for (int_test_cases) |test_case| {
        try handleTestCase(
            test_case.input,
            test_case.left_value,
            test_case.operator,
            test_case.right_value,
        );
    }

    inline for (bool_test_cases) |test_case| {
        try handleTestCase(
            test_case.input,
            test_case.left_value,
            test_case.operator,
            test_case.right_value,
        );
    }
}

test "IfExpression" {
    const testing = std.testing;

    const input = "if (x < y) { x }";

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
    try testing.expect(expression.subtype.* == .if_expression);

    const if_expression = expression.subtype.if_expression;
    try expectExpressionToBeInfixExpression(
        if_expression.condition,
        @as([]const u8, "x"),
        "<",
        @as([]const u8, "y"),
    );

    try testing.expectEqual(1, if_expression.consequence.statements.len);

    const consequence_statement = if_expression.consequence.statements[0];
    try testing.expect(consequence_statement.subtype.* == .expression_statement);

    const consequence_expression_statement =
        consequence_statement.subtype.expression_statement;

    const consequence_expression =
        consequence_expression_statement.expression.*.?;

    try expectExpressionToBeIdentifier(&consequence_expression, "x");

    try testing.expectEqual(null, if_expression.alternative.*);
}

test "IfElseExpression" {
    const testing = std.testing;

    const input = "if (x < y) { x } else { y }";

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
    try testing.expect(expression.subtype.* == .if_expression);

    const if_expression = expression.subtype.if_expression;
    try expectExpressionToBeInfixExpression(
        if_expression.condition,
        @as([]const u8, "x"),
        "<",
        @as([]const u8, "y"),
    );

    try testing.expectEqual(1, if_expression.consequence.statements.len);

    const consequence_statement = if_expression.consequence.statements[0];
    try testing.expect(consequence_statement.subtype.* == .expression_statement);

    const consequence_expression_statement =
        consequence_statement.subtype.expression_statement;

    const consequence_expression =
        consequence_expression_statement.expression.*.?;

    try expectExpressionToBeIdentifier(&consequence_expression, "x");

    try testing.expect(if_expression.alternative.* != null);

    const alternative = if_expression.alternative.*.?;
    try testing.expectEqual(1, alternative.statements.len);

    const alternative_statement = alternative.statements[0];
    try testing.expect(alternative_statement.subtype.* == .expression_statement);

    const alternative_expression_statement =
        alternative_statement.subtype.expression_statement;

    const alternative_expression =
        alternative_expression_statement.expression.*.?;

    try expectExpressionToBeIdentifier(&alternative_expression, "y");
}

test "FunctionLiteral" {
    const testing = std.testing;

    const input = "fn(x, y) { x + y; }";

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
    try testing.expect(expression.subtype.* == .function_literal);

    const function = expression.subtype.function_literal;
    try testing.expectEqual(2, function.parameters.len);

    try expectIdentifier(&function.parameters[0], "x");
    try expectIdentifier(&function.parameters[1], "y");

    try testing.expectEqual(1, function.body.statements.len);
    const body_statement = function.body.statements[0];

    try testing.expect(body_statement.subtype.* == .expression_statement);

    try expectExpressionToBeInfixExpression(
        &body_statement.subtype.expression_statement.expression.*.?,
        @as([]const u8, "x"),
        "+",
        @as([]const u8, "y"),
    );
}

test "OperatorPrecedenceParsing" {
    const testing = std.testing;

    const test_cases = [_]struct { input: []const u8, expected: []const u8 }{
        .{
            .input = "-a * b",
            .expected = "((-a) * b)",
        },
        .{
            .input = "!-a",
            .expected = "(!(-a))",
        },
        .{
            .input = "a + b + c",
            .expected = "((a + b) + c)",
        },
        .{
            .input = "a + b - c",
            .expected = "((a + b) - c)",
        },
        .{
            .input = "a * b * c",
            .expected = "((a * b) * c)",
        },
        .{
            .input = "a * b / c",
            .expected = "((a * b) / c)",
        },
        .{
            .input = "a + b / c",
            .expected = "(a + (b / c))",
        },
        .{
            .input = "a + b * c + d / e - f",
            .expected = "(((a + (b * c)) + (d / e)) - f)",
        },
        .{
            .input = "3 + 4; -5 * 5",
            .expected = "(3 + 4)((-5) * 5)",
        },
        .{
            .input = "5 > 4 == 3 < 4",
            .expected = "((5 > 4) == (3 < 4))",
        },
        .{
            .input = "5 < 4 != 3 > 4",
            .expected = "((5 < 4) != (3 > 4))",
        },
        .{
            .input = "3 + 4 * 5 == 3 * 1 + 4 * 5",
            .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        .{
            .input = "true",
            .expected = "true",
        },
        .{
            .input = "false",
            .expected = "false",
        },
        .{
            .input = "3 > 5 == false",
            .expected = "((3 > 5) == false)",
        },
        .{
            .input = "3 < 5 == true",
            .expected = "((3 < 5) == true)",
        },
        .{
            .input = "1 + (2 + 3) + 4",
            .expected = "((1 + (2 + 3)) + 4)",
        },
        .{
            .input = "(5 + 5) * 2",
            .expected = "((5 + 5) * 2)",
        },
        .{
            .input = "2 / (5 + 5)",
            .expected = "(2 / (5 + 5))",
        },
        .{
            .input = "-(5 + 5)",
            .expected = "(-(5 + 5))",
        },
        .{
            .input = "!(true == true)",
            .expected = "(!(true == true))",
        },
    };

    inline for (test_cases) |test_case| {
        var lexer = Lexer.init(test_case.input);
        var parser = try Parser.init(testing.allocator, &lexer);
        defer parser.deinit();

        const program = try parser.allocParseProgram(testing.allocator);
        defer program.deinit();

        try expectNoParserErrors(&parser);

        const program_string = try program.allocString(testing.allocator);
        defer testing.allocator.free(program_string);

        try testing.expectEqualStrings(test_case.expected, program_string);
    }
}
