const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("token.zig").Token;

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,

    pub fn tokenLiteral(self: *const Node) []const u8 {
        return switch (self.*) {
            inline else => |x| x.tokenLiteral(),
        };
    }

    pub fn allocString(self: *const Node, allocator: Allocator) Allocator.Error![]const u8 {
        return switch (self.*) {
            inline else => |x| x.allocString(allocator),
        };
    }
};

pub const Statement = union(enum) {
    program: Program,
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,

    pub fn tokenLiteral(self: *const Statement) []const u8 {
        return switch (self.*) {
            inline else => |x| x.tokenLiteral(),
        };
    }

    pub fn allocString(self: *const Statement, allocator: Allocator) Allocator.Error![]const u8 {
        return switch (self.*) {
            inline else => |x| x.allocString(allocator),
        };
    }

    pub fn statementNode(self: *const Statement) void {
        return switch (self.*) {
            inline else => |x| x.statementNode(),
        };
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,

    pub fn tokenLiteral(self: *const Expression) []const u8 {
        return switch (self.*) {
            inline else => |x| x.tokenLiteral(),
        };
    }

    pub fn allocString(self: *const Expression, allocator: Allocator) Allocator.Error![]const u8 {
        return switch (self.*) {
            inline else => |x| x.allocString(allocator),
        };
    }

    pub fn expressionNode(self: *const Expression) void {
        return switch (self.*) {
            inline else => |x| x.expressionNode(),
        };
    }
};

pub const Program = struct {
    allocator: Allocator,
    statements: []const Statement,

    pub fn init(allocator: Allocator, statements: []const Statement) Program {
        return .{ .allocator = allocator, .statements = statements };
    }

    pub fn tokenLiteral(self: *const Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn allocString(self: *const Program, allocator: Allocator) Allocator.Error![]const u8 {
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

    pub fn statementNode(_: *const Program) void {}

    pub fn deinit(self: *const Program) void {
        self.allocator.free(self.statements);
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    value: ?Expression,

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(self: *const LetStatement, allocator: Allocator) Allocator.Error![]const u8 {
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
        if (self.value) |value| {
            const value_string = try value.allocString(allocator);
            defer allocator.free(value_string);
            try writer.writeAll(value_string);
        }

        try writer.writeAll(";");

        return allocator.dupe(u8, list.items);
    }

    pub fn statementNode(_: *const LetStatement) void {}
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: ?Expression,

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(self: *const ReturnStatement, allocator: Allocator) Allocator.Error![]const u8 {
        var list = std.ArrayList(u8).init(allocator);
        defer list.deinit();

        var writer = list.writer();

        try writer.writeAll(self.tokenLiteral());
        try writer.writeAll(" ");

        // TODO: Remove this check once we're setting the field.
        if (self.return_value) |return_value| {
            const return_value_string = try return_value.allocString(allocator);
            defer allocator.free(return_value_string);
            try writer.writeAll(return_value_string);
        }

        try writer.writeAll(";");

        return allocator.dupe(u8, list.items);
    }

    pub fn statementNode(_: *const ReturnStatement) void {}
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: ?Expression,

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(self: *const ExpressionStatement, allocator: Allocator) Allocator.Error![]const u8 {
        // TODO: Remove this check once we're setting the field.
        if (self.expression) |expression| {
            const expression_string = try expression.allocString(allocator);
            defer allocator.free(expression_string);

            return allocator.dupe(u8, expression_string);
        } else {
            return "";
        }
    }

    pub fn statementNode(_: *const ExpressionStatement) void {}
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn allocString(self: *const Identifier, allocator: Allocator) Allocator.Error![]const u8 {
        return allocator.dupe(u8, self.value);
    }

    pub fn expressionNode(_: *const Identifier) void {}
};

test "allocString()" {
    const testing = std.testing;

    const statements = [_]Statement{
        Statement{
            .let_statement = LetStatement{
                .token = Token{ .type = Token.LET, .literal = "let" },
                .name = Identifier{
                    .token = Token{ .type = Token.IDENT, .literal = "myVar" },
                    .value = "myVar",
                },
                .value = Expression{
                    .identifier = Identifier{
                        .token = Token{ .type = Token.IDENT, .literal = "anotherVar" },
                        .value = "anotherVar",
                    },
                },
            },
        },
    };

    const program = Program{
        .allocator = testing.allocator,
        .statements = statements[0..],
    };

    const program_string = try program.allocString(testing.allocator);
    defer testing.allocator.free(program_string);

    try testing.expectEqualStrings("let myVar = anotherVar;", program_string);
}
