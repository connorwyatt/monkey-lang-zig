const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const object = @import("object.zig");
const Parser = @import("parser.zig").Parser;

pub const TRUE = blk: {
    const value = object.Boolean{ .value = true };
    break :blk value.toObject();
};

pub const FALSE = blk: {
    const value = object.Boolean{ .value = false };
    break :blk value.toObject();
};

pub const NULL = blk: {
    const value = object.Null{};
    break :blk value.toObject();
};

pub fn eval(node: ast.AnyNodePointer) ?object.Object {
    switch (node) {
        .program => |program| {
            return evalStatements(program.statements);
        },
        .statement => |statement| {
            return eval(statement.toAnyNodePointer());
        },
        .expression_statement => |expression_statement| {
            return eval(expression_statement.expression.*.?.toAnyNodePointer());
        },
        .expression => |expression| {
            return eval(expression.toAnyNodePointer());
        },
        .prefix_expression => |prefix_expression| {
            if (prefix_expression.right.*) |*r| {
                const right = eval(r.toAnyNodePointer());
                return evalPrefixExpression(prefix_expression.operator, right.?);
            } else {
                return null;
            }
        },
        .infix_expression => |infix_expression| {
            const left = eval(infix_expression.left.*.?.toAnyNodePointer());
            const right = eval(infix_expression.right.*.?.toAnyNodePointer());
            return evalInfixExpression(infix_expression.operator, left.?, right.?);
        },
        .integer_literal => |integer_literal| {
            const o = object.Integer{
                .value = integer_literal.value,
            };
            return o.toObject();
        },
        .boolean => |boolean| {
            return nativeBoolToBooleanObject(boolean.value);
        },
        else => {},
    }

    return null;
}

fn evalStatements(statements: []const ast.Statement) ?object.Object {
    var result: ?object.Object = null;

    for (statements) |*statement| {
        result = eval(statement.toAnyNodePointer());
    }

    return result;
}

fn evalPrefixExpression(
    operator: []const u8,
    right: object.Object,
) object.Object {
    if (std.mem.eql(u8, operator, "!")) {
        return evalBangOperatorExpression(right);
    } else if (std.mem.eql(u8, operator, "-")) {
        return evalMinusPrefixOperatorExpression(right);
    } else {
        return NULL;
    }
}

fn evalBangOperatorExpression(right: object.Object) object.Object {
    switch (right.subtype) {
        .boolean => |boolean| {
            if (boolean.value) {
                return FALSE;
            } else {
                return TRUE;
            }
        },
        .null => {
            return TRUE;
        },
        else => {
            return FALSE;
        },
    }
}

fn evalMinusPrefixOperatorExpression(right: object.Object) object.Object {
    if (!std.mem.eql(u8, right.type(), object.ObjectType.INTEGER_OBJ)) {
        return NULL;
    }

    const value = right.subtype.integer.value;
    const o = object.Integer{ .value = -value };
    return o.toObject();
}

fn evalInfixExpression(
    operator: []const u8,
    left: object.Object,
    right: object.Object,
) object.Object {
    if (std.mem.eql(u8, left.type(), object.ObjectType.INTEGER_OBJ) and
        std.mem.eql(u8, right.type(), object.ObjectType.INTEGER_OBJ))
    {
        return evalIntegerInfixExpression(
            operator,
            left.subtype.integer,
            right.subtype.integer,
        );
    } else {
        return NULL;
    }
}

fn evalIntegerInfixExpression(
    operator: []const u8,
    left: object.Integer,
    right: object.Integer,
) object.Object {
    if (std.mem.eql(u8, operator, "+")) {
        const o = object.Integer{ .value = left.value + right.value };
        return o.toObject();
    } else if (std.mem.eql(u8, operator, "-")) {
        const o = object.Integer{ .value = left.value - right.value };
        return o.toObject();
    } else if (std.mem.eql(u8, operator, "*")) {
        const o = object.Integer{ .value = left.value * right.value };
        return o.toObject();
    } else if (std.mem.eql(u8, operator, "/")) {
        const o = object.Integer{ .value = @divTrunc(left.value, right.value) };
        return o.toObject();
    } else if (std.mem.eql(u8, operator, "<")) {
        return nativeBoolToBooleanObject(left.value < right.value);
    } else if (std.mem.eql(u8, operator, ">")) {
        return nativeBoolToBooleanObject(left.value > right.value);
    } else if (std.mem.eql(u8, operator, "==")) {
        return nativeBoolToBooleanObject(left.value == right.value);
    } else if (std.mem.eql(u8, operator, "!=")) {
        return nativeBoolToBooleanObject(left.value != right.value);
    } else {
        return NULL;
    }
}

fn nativeBoolToBooleanObject(input: bool) object.Object {
    if (input) {
        return TRUE;
    } else {
        return FALSE;
    }
}

fn testEval(input: []const u8) !object.Object {
    const testing = std.testing;

    var lexer = Lexer.init(input);
    var parser = try Parser.init(testing.allocator, &lexer);
    defer parser.deinit();

    const program = try parser.allocParseProgram(testing.allocator);
    defer program.deinit();

    const eval_result = eval(program.toAnyNodePointer());

    try testing.expect(eval_result != null);

    return eval_result.?;
}

fn expectIntegerObject(o: object.Object, expected_value: i64) !void {
    const testing = std.testing;

    try testing.expect(o.subtype == .integer);
    const integer_object = o.subtype.integer;

    try testing.expectEqual(expected_value, integer_object.value);
}

fn expectBooleanObject(o: object.Object, expected_value: bool) !void {
    const testing = std.testing;

    try testing.expect(o.subtype == .boolean);
    const boolean_object = o.subtype.boolean;

    try testing.expectEqual(expected_value, boolean_object.value);
}

test "eval IntegerExpression" {
    const test_cases = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
        .{ .input = "-5", .expected = -5 },
        .{ .input = "-10", .expected = -10 },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = 10 },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = 32 },
        .{ .input = "-50 + 100 + -50", .expected = 0 },
        .{ .input = "5 * 2 + 10", .expected = 20 },
        .{ .input = "5 + 2 * 10", .expected = 25 },
        .{ .input = "20 + 2 * -10", .expected = 0 },
        .{ .input = "50 / 2 * 2 + 10", .expected = 60 },
        .{ .input = "2 * (5 + 10)", .expected = 30 },
        .{ .input = "3 * 3 * 3 + 10", .expected = 37 },
        .{ .input = "3 * (3 * 3) + 10", .expected = 37 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = 50 },
    };

    inline for (test_cases) |test_case| {
        const evaluated = try testEval(test_case.input);
        try expectIntegerObject(evaluated, test_case.expected);
    }
}

test "eval BooleanExpression" {
    const test_cases = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "1 < 2", .expected = true },
        .{ .input = "1 > 2", .expected = false },
        .{ .input = "1 < 1", .expected = false },
        .{ .input = "1 > 1", .expected = false },
        .{ .input = "1 == 1", .expected = true },
        .{ .input = "1 != 1", .expected = false },
        .{ .input = "1 == 2", .expected = false },
        .{ .input = "1 != 2", .expected = true },
    };

    inline for (test_cases) |test_case| {
        const evaluated = try testEval(test_case.input);
        try expectBooleanObject(evaluated, test_case.expected);
    }
}

test "eval BangOperator" {
    const test_cases = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "!true", .expected = false },
        .{ .input = "!false", .expected = true },
        .{ .input = "!5", .expected = false },
        .{ .input = "!!true", .expected = true },
        .{ .input = "!!false", .expected = false },
        .{ .input = "!!5", .expected = true },
    };

    inline for (test_cases) |test_case| {
        const evaluated = try testEval(test_case.input);
        try expectBooleanObject(evaluated, test_case.expected);
    }
}
