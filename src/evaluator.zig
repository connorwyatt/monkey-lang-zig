const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const object = @import("object.zig");
const Parser = @import("parser.zig").Parser;

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
        .integer_literal => |integer_literal| {
            return object.Object{
                .subtype = .{
                    .integer = object.Integer{
                        .value = integer_literal.value,
                    },
                },
            };
        },
        else => {},
    }

    return null;
}

fn evalStatements(statements: []const ast.Statement) ?object.Object {
    var result: ?object.Object = null;

    for (statements) |*statement| {
        result = eval(.{ .statement = statement });
    }

    return result;
}

fn testEval(input: []const u8) !object.Object {
    const testing = std.testing;

    var lexer = Lexer.init(input);
    var parser = try Parser.init(testing.allocator, &lexer);
    defer parser.deinit();

    const program = try parser.allocParseProgram(testing.allocator);
    defer program.deinit();

    const eval_result = eval(.{ .program = &program });

    try testing.expect(eval_result != null);

    return eval_result.?;
}

fn expectIntegerObject(o: *const object.Object, expected_value: i64) !void {
    const testing = std.testing;

    try testing.expect(o.subtype == .integer);
    const integer_object = o.subtype.integer;

    try testing.expectEqual(expected_value, integer_object.value);
}

test "eval IntegerExpression" {
    const test_cases = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
    };

    inline for (test_cases) |test_case| {
        const evaluated = try testEval(test_case.input);
        try expectIntegerObject(&evaluated, test_case.expected);
    }
}
