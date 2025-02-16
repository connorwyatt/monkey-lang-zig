const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("token.zig").Token;

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,

    pub fn tokenLiteral(self: *const Node) []const u8 {
        return switch (self.*) {
            inline else => self.tokenLiteral(),
        };
    }
};

pub const Statement = union(enum) {
    program: Program,
    let_statement: LetStatement,

    pub fn tokenLiteral(self: *const Statement) []const u8 {
        return switch (self.*) {
            inline else => self.tokenLiteral(),
        };
    }

    pub fn statementNode(self: *const Statement) void {
        return switch (self.*) {
            inline else => self.statementNode(),
        };
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,

    pub fn tokenLiteral(self: *const Expression) []const u8 {
        return switch (self.*) {
            inline else => self.tokenLiteral(),
        };
    }

    pub fn expressionNode(self: *const Expression) void {
        return switch (self.*) {
            inline else => self.expressionNode(),
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

    pub fn statementNode(_: *const Program) void {}

    pub fn deinit(self: *const Program) void {
        self.allocator.free(self.statements);
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    value: Expression,

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn statementNode(_: *const LetStatement) void {}
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn expressionNode(_: *const Identifier) void {}
};
