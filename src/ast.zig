const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("token.zig").Token;

pub const Node = struct {
    allocator: Allocator,
    subtype: *const Subtype,

    const Self = @This();

    pub const Subtype = union(enum) {
        statement: Statement,
        expression: Expression,
    };

    pub fn init(allocator: Allocator, subtype: Subtype) Allocator.Error!Self {
        const subtype_ptr = try allocator.create(Subtype);
        subtype_ptr.* = subtype;
        return .{
            .allocator = allocator,
            .subtype = subtype_ptr,
        };
    }

    pub fn deinit(self: *const Self) void {
        switch (self.subtype.*) {
            inline else => |x| x.deinit(),
        }
        self.allocator.destroy(self.subtype);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return switch (self.subtype) {
            inline else => |x| x.tokenLiteral(),
        };
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        return switch (self.subtype) {
            inline else => |x| x.allocString(allocator),
        };
    }

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return switch (self.subtype.*) {
            inline else => |x| x.toAnyNodePointer(),
        };
    }
};

pub const Statement = struct {
    allocator: Allocator,
    subtype: *const Subtype,

    const Self = @This();

    pub const Subtype = union(enum) {
        program: Program,
        let_statement: LetStatement,
        return_statement: ReturnStatement,
        expression_statement: ExpressionStatement,
        block_statement: BlockStatement,
    };

    pub fn init(allocator: Allocator, subtype: Subtype) Allocator.Error!Self {
        const subtype_ptr = try allocator.create(Subtype);
        subtype_ptr.* = subtype;
        return .{
            .allocator = allocator,
            .subtype = subtype_ptr,
        };
    }

    pub fn deinit(self: *const Self) void {
        switch (self.subtype.*) {
            inline else => |x| x.deinit(),
        }
        self.allocator.destroy(self.subtype);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return switch (self.subtype.*) {
            inline else => |x| x.tokenLiteral(),
        };
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        return switch (self.subtype.*) {
            inline else => |x| x.allocString(allocator),
        };
    }

    pub fn statementNode(self: *const Self) void {
        _ = self;
    }

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return switch (self.subtype.*) {
            inline else => |x| x.toAnyNodePointer(),
        };
    }
};

pub const Expression = struct {
    allocator: Allocator,
    subtype: *const Subtype,

    const Self = @This();

    pub const Subtype = union(enum) {
        identifier: Identifier,
        integer_literal: IntegerLiteral,
        boolean: Boolean,
        prefix_expression: PrefixExpression,
        infix_expression: InfixExpression,
        if_expression: IfExpression,
        function_literal: FunctionLiteral,
        call_expression: CallExpression,
    };

    pub fn init(allocator: Allocator, subtype: Subtype) Allocator.Error!Self {
        const subtype_ptr = try allocator.create(Subtype);
        subtype_ptr.* = subtype;
        return .{
            .allocator = allocator,
            .subtype = subtype_ptr,
        };
    }

    pub fn deinit(self: *const Self) void {
        switch (self.subtype.*) {
            inline else => |x| x.deinit(),
        }
        self.allocator.destroy(self.subtype);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return switch (self.subtype.*) {
            inline else => |x| x.tokenLiteral(),
        };
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        return switch (self.subtype.*) {
            inline else => |x| x.allocString(allocator),
        };
    }

    pub fn expressionNode(self: *const Self) void {
        _ = self;
    }

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return switch (self.subtype.*) {
            inline else => |x| x.toAnyNodePointer(),
        };
    }
};

pub const Program = struct {
    allocator: Allocator,
    statements: []const Statement,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        statements: []const Statement,
    ) Allocator.Error!Self {
        return .{
            .allocator = allocator,
            .statements = try allocator.dupe(Statement, statements),
        };
    }

    pub fn deinit(self: *const Self) void {
        for (self.statements) |statement| {
            statement.deinit();
        }
        self.allocator.free(self.statements);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        var list = std.ArrayList(u8).init(allocator);
        defer list.deinit();

        var writer = list.writer();

        for (self.statements) |statement| {
            const statement_string = try statement.allocString(allocator);
            defer allocator.free(statement_string);

            try writer.writeAll(statement_string);
        }

        return allocator.dupe(u8, list.items);
    }

    pub fn statementNode(self: *const Self) void {
        _ = self;
    }

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .program = self };
    }
};

pub const LetStatement = struct {
    allocator: Allocator,
    token: Token,
    name: *const Identifier,
    value: *const ?Expression,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        name: Identifier,
        value: ?Expression,
    ) Allocator.Error!Self {
        const name_ptr = try allocator.create(Identifier);
        name_ptr.* = name;
        const value_ptr = try allocator.create(?Expression);
        value_ptr.* = value;
        return .{
            .allocator = allocator,
            .token = token,
            .name = name_ptr,
            .value = value_ptr,
        };
    }

    pub fn deinit(self: *const Self) void {
        self.name.deinit();
        self.allocator.destroy(self.name);
        if (self.value.*) |value| {
            value.deinit();
        }
        self.allocator.destroy(self.value);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        var list = std.ArrayList(u8).init(allocator);
        defer list.deinit();

        var writer = list.writer();

        try writer.writeAll(self.tokenLiteral());
        try writer.writeAll(" ");

        {
            const name_string = try self.name.allocString(allocator);
            defer allocator.free(name_string);
            try writer.writeAll(name_string);
        }

        try writer.writeAll(" = ");

        // TODO: Remove this check once we're setting the field.
        if (self.value.*) |value| {
            const value_string = try value.allocString(allocator);
            defer allocator.free(value_string);
            try writer.writeAll(value_string);
        }

        try writer.writeAll(";");

        return allocator.dupe(u8, list.items);
    }

    pub fn statementNode(_: *const Self) void {}

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .let_statement = self };
    }
};

pub const ReturnStatement = struct {
    allocator: Allocator,
    token: Token,
    return_value: *const ?Expression,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        return_value: ?Expression,
    ) Allocator.Error!Self {
        const return_value_ptr = try allocator.create(?Expression);
        return_value_ptr.* = return_value;
        return .{
            .allocator = allocator,
            .token = token,
            .return_value = return_value_ptr,
        };
    }

    pub fn deinit(self: *const Self) void {
        if (self.return_value.*) |return_value| {
            return_value.deinit();
        }
        self.allocator.destroy(self.return_value);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        var list = std.ArrayList(u8).init(allocator);
        defer list.deinit();

        var writer = list.writer();

        try writer.writeAll(self.tokenLiteral());
        try writer.writeAll(" ");

        // TODO: Remove this check once we're setting the field.
        if (self.return_value.*) |return_value| {
            const return_value_string = try return_value.allocString(allocator);
            defer allocator.free(return_value_string);
            try writer.writeAll(return_value_string);
        }

        try writer.writeAll(";");

        return allocator.dupe(u8, list.items);
    }

    pub fn statementNode(self: *const Self) void {
        _ = self;
    }

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .return_statement = self };
    }
};

pub const ExpressionStatement = struct {
    allocator: Allocator,
    token: Token,
    expression: *const ?Expression,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        expression: ?Expression,
    ) Allocator.Error!Self {
        const expression_ptr = try allocator.create(?Expression);
        expression_ptr.* = expression;
        return .{
            .allocator = allocator,
            .token = token,
            .expression = expression_ptr,
        };
    }

    pub fn deinit(self: *const Self) void {
        if (self.expression.*) |expression| {
            expression.deinit();
        }
        self.allocator.destroy(self.expression);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        // TODO: Remove this check once we're setting the field.
        if (self.expression.*) |expression| {
            const expression_string = try expression.allocString(allocator);
            defer allocator.free(expression_string);

            return allocator.dupe(u8, expression_string);
        } else {
            return "";
        }
    }

    pub fn statementNode(self: *const Self) void {
        _ = self;
    }

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .expression_statement = self };
    }
};

pub const BlockStatement = struct {
    allocator: Allocator,
    token: Token,
    statements: []const Statement,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        statements: []const Statement,
    ) Allocator.Error!Self {
        return .{
            .allocator = allocator,
            .token = token,
            .statements = try allocator.dupe(Statement, statements),
        };
    }

    pub fn deinit(self: *const Self) void {
        for (self.statements) |statement| {
            statement.deinit();
        }
        self.allocator.free(self.statements);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        var list = std.ArrayList(u8).init(allocator);
        defer list.deinit();

        var writer = list.writer();

        for (self.statements) |statement| {
            const statement_string = try statement.allocString(allocator);
            defer allocator.free(statement_string);

            try writer.writeAll(statement_string);
        }

        return allocator.dupe(u8, list.items);
    }

    pub fn statementNode(self: *const Self) void {
        _ = self;
    }

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .block_statement = self };
    }
};

pub const Identifier = struct {
    allocator: Allocator,
    token: Token,
    value: []const u8,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        value: []const u8,
    ) Allocator.Error!Self {
        return .{
            .allocator = allocator,
            .token = token,
            .value = try allocator.dupe(u8, value),
        };
    }

    pub fn deinit(self: *const Self) void {
        self.allocator.free(self.value);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        return allocator.dupe(u8, self.value);
    }

    pub fn expressionNode(self: *const Self) void {
        _ = self;
    }

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .identifier = self };
    }
};

pub const IntegerLiteral = struct {
    allocator: Allocator,
    token: Token,
    value: i64,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        value: i64,
    ) Allocator.Error!Self {
        return .{
            .allocator = allocator,
            .token = token,
            .value = value,
        };
    }

    pub fn deinit(self: *const Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        return allocator.dupe(u8, self.token.literal);
    }

    pub fn expressionNode(_: *const Self) void {}

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .integer_literal = self };
    }
};

pub const Boolean = struct {
    allocator: Allocator,
    token: Token,
    value: bool,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        value: bool,
    ) Allocator.Error!Self {
        return .{
            .allocator = allocator,
            .token = token,
            .value = value,
        };
    }

    pub fn deinit(self: *const Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        return allocator.dupe(u8, self.token.literal);
    }

    pub fn expressionNode(_: *const Self) void {}

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .boolean = self };
    }
};

pub const PrefixExpression = struct {
    allocator: Allocator,
    token: Token,
    operator: []const u8,
    right: *const ?Expression,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        operator: []const u8,
        right: ?Expression,
    ) Allocator.Error!Self {
        const right_ptr = try allocator.create(?Expression);
        right_ptr.* = right;
        return .{
            .allocator = allocator,
            .token = token,
            .operator = try allocator.dupe(u8, operator),
            .right = right_ptr,
        };
    }

    pub fn deinit(self: *const Self) void {
        self.allocator.free(self.operator);
        if (self.right.*) |right| {
            right.deinit();
        }
        self.allocator.destroy(self.right);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        const right_string = try self.right.*.?.allocString(allocator);
        defer allocator.free(right_string);
        return try std.fmt.allocPrint(
            allocator,
            "({s}{s})",
            .{ self.operator, right_string },
        );
    }

    pub fn expressionNode(_: *const Self) void {}

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .prefix_expression = self };
    }
};

pub const InfixExpression = struct {
    allocator: Allocator,
    token: Token,
    left: *const ?Expression,
    operator: []const u8,
    right: *const ?Expression,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        left: ?Expression,
        operator: []const u8,
        right: ?Expression,
    ) Allocator.Error!Self {
        const left_ptr = try allocator.create(?Expression);
        left_ptr.* = left;
        const right_ptr = try allocator.create(?Expression);
        right_ptr.* = right;
        return .{
            .allocator = allocator,
            .token = token,
            .left = left_ptr,
            .operator = try allocator.dupe(u8, operator),
            .right = right_ptr,
        };
    }

    pub fn deinit(self: *const Self) void {
        if (self.left.*) |left| {
            left.deinit();
        }
        self.allocator.destroy(self.left);
        self.allocator.free(self.operator);
        if (self.right.*) |right| {
            right.deinit();
        }
        self.allocator.destroy(self.right);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        const left_string = try self.left.*.?.allocString(allocator);
        defer allocator.free(left_string);
        const right_string = try self.right.*.?.allocString(allocator);
        defer allocator.free(right_string);
        return try std.fmt.allocPrint(
            allocator,
            "({s} {s} {s})",
            .{ left_string, self.operator, right_string },
        );
    }

    pub fn expressionNode(_: *const Self) void {}

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .infix_expression = self };
    }
};

pub const IfExpression = struct {
    allocator: Allocator,
    token: Token,
    condition: *const Expression,
    consequence: *const BlockStatement,
    alternative: *const ?BlockStatement,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        condition: Expression,
        consequence: BlockStatement,
        alternative: ?BlockStatement,
    ) Allocator.Error!Self {
        const condition_ptr = try allocator.create(Expression);
        condition_ptr.* = condition;
        const consequence_ptr = try allocator.create(BlockStatement);
        consequence_ptr.* = consequence;
        const alternative_ptr = try allocator.create(?BlockStatement);
        alternative_ptr.* = alternative;
        return .{
            .allocator = allocator,
            .token = token,
            .condition = condition_ptr,
            .consequence = consequence_ptr,
            .alternative = alternative_ptr,
        };
    }

    pub fn deinit(self: *const Self) void {
        self.condition.deinit();
        self.allocator.destroy(self.condition);
        self.consequence.deinit();
        self.allocator.destroy(self.consequence);
        if (self.alternative.*) |alternative| {
            alternative.deinit();
        }
        self.allocator.destroy(self.alternative);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        var list = std.ArrayList(u8).init(allocator);
        defer list.deinit();

        const writer = list.writer();

        {
            const condition_string = try self.condition.allocString(allocator);
            defer allocator.free(condition_string);
            try writer.print("if{s} ", .{condition_string});
        }

        {
            const consequence_string = try self.consequence.allocString(allocator);
            defer allocator.free(consequence_string);
            try writer.writeAll(consequence_string);
        }

        if (self.alternative.*) |alternative| {
            const alternative_string = try alternative.allocString(allocator);
            defer allocator.free(alternative_string);
            try writer.print("else {s}", .{alternative_string});
        }

        return try allocator.dupe(u8, list.items);
    }

    pub fn expressionNode(_: *const Self) void {}

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .if_expression = self };
    }
};

pub const FunctionLiteral = struct {
    allocator: Allocator,
    token: Token,
    parameters: []const Identifier,
    body: *const BlockStatement,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        parameters: []const Identifier,
        body: BlockStatement,
    ) Allocator.Error!Self {
        const body_ptr = try allocator.create(BlockStatement);
        body_ptr.* = body;
        return .{
            .allocator = allocator,
            .token = token,
            .parameters = try allocator.dupe(Identifier, parameters),
            .body = body_ptr,
        };
    }

    pub fn deinit(self: *const Self) void {
        for (self.parameters) |parameter| {
            parameter.deinit();
        }
        self.allocator.free(self.parameters);
        self.body.deinit();
        self.allocator.destroy(self.body);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        var list = std.ArrayList(u8).init(allocator);
        defer list.deinit();

        const writer = list.writer();

        try writer.print("{s}(", .{self.tokenLiteral()});

        for (self.parameters, 1..) |parameter, i| {
            const parameter_string = try parameter.allocString(allocator);
            defer allocator.free(parameter_string);
            try writer.writeAll(parameter_string);

            if (i != self.parameters.len) {
                try writer.writeAll(", ");
            }
        }

        try writer.writeAll(") ");

        {
            const body_string = try self.body.allocString(allocator);
            defer allocator.free(body_string);
            try writer.writeAll(body_string);
        }

        return try allocator.dupe(u8, list.items);
    }

    pub fn expressionNode(_: *const Self) void {}

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .function_literal = self };
    }
};

pub const CallExpression = struct {
    allocator: Allocator,
    token: Token,
    function: *const Expression,
    arguments: []const Expression,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        token: Token,
        function: Expression,
        arguments: []const Expression,
    ) Allocator.Error!Self {
        const function_ptr = try allocator.create(Expression);
        function_ptr.* = function;
        return .{
            .allocator = allocator,
            .token = token,
            .function = function_ptr,
            .arguments = try allocator.dupe(Expression, arguments),
        };
    }

    pub fn deinit(self: *const Self) void {
        self.function.deinit();
        self.allocator.destroy(self.function);
        for (self.arguments) |argument| {
            argument.deinit();
        }
        self.allocator.free(self.arguments);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        var list = std.ArrayList(u8).init(allocator);
        defer list.deinit();

        const writer = list.writer();

        const function_string = try self.function.allocString(allocator);
        defer allocator.free(function_string);
        try writer.print("{s}(", .{function_string});

        for (self.arguments, 1..) |argument, i| {
            const argument_string = try argument.allocString(allocator);
            defer allocator.free(argument_string);
            try writer.writeAll(argument_string);

            if (i != self.arguments.len) {
                try writer.writeAll(", ");
            }
        }

        try writer.writeAll(")");

        return try allocator.dupe(u8, list.items);
    }

    pub fn expressionNode(_: *const Self) void {}

    pub fn toAnyNodePointer(self: *const Self) AnyNodePointer {
        return .{ .call_expression = self };
    }
};

// TODO: See if this could be a bit less manual with comptime
pub const AnyNodePointer = union(enum) {
    node: *const Node,

    statement: *const Statement,
    program: *const Program,
    let_statement: *const LetStatement,
    return_statement: *const ReturnStatement,
    expression_statement: *const ExpressionStatement,
    block_statement: *const BlockStatement,

    expression: *const Expression,
    identifier: *const Identifier,
    integer_literal: *const IntegerLiteral,
    boolean: *const Boolean,
    prefix_expression: *const PrefixExpression,
    infix_expression: *const InfixExpression,
    if_expression: *const IfExpression,
    function_literal: *const FunctionLiteral,
    call_expression: *const CallExpression,
};

test "allocString" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const statements = [_]Statement{
        try Statement.init(arena.allocator(), .{
            .let_statement = try LetStatement.init(
                arena.allocator(),
                Token{ .type = Token.LET, .literal = "let" },
                try Identifier.init(
                    arena.allocator(),
                    Token{ .type = Token.IDENT, .literal = "myVar" },
                    "myVar",
                ),
                try Expression.init(
                    arena.allocator(),
                    .{
                        .identifier = try Identifier.init(
                            arena.allocator(),
                            Token{ .type = Token.IDENT, .literal = "anotherVar" },
                            "anotherVar",
                        ),
                    },
                ),
            ),
        }),
    };

    const program = try Program.init(testing.allocator, statements[0..]);
    defer program.deinit();

    const program_string = try program.allocString(testing.allocator);
    defer testing.allocator.free(program_string);

    try testing.expectEqualStrings("let myVar = anotherVar;", program_string);
}
