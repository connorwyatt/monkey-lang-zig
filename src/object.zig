const std = @import("std");
const Allocator = std.mem.Allocator;

pub const ObjectType = struct {
    pub const INTEGER_OBJ = "INTEGER";
    pub const BOOLEAN_OBJ = "BOOLEAN";
    pub const NULL_OBJ = "NULL";
};

pub const Object = struct {
    subtype: Subtype,

    const Self = @This();

    pub const Subtype = union(enum) {
        integer: Integer,
        boolean: Boolean,
    };

    pub fn @"type"(self: *const Self) []const u8 {
        return switch (self.subtype) {
            inline else => |x| x.type(),
        };
    }

    pub fn inspect(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        return switch (self.subtype) {
            inline else => |x| x.inspect(allocator),
        };
    }
};

pub const Integer = struct {
    value: i64,

    const Self = @This();

    pub fn @"type"(self: *const Self) []const u8 {
        _ = self;
        return ObjectType.INTEGER_OBJ;
    }

    pub fn inspect(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        return try std.fmt.allocPrint(allocator, "{}", .{self.value});
    }
};

pub const Boolean = struct {
    value: bool,

    const Self = @This();

    pub fn @"type"(self: *const Self) []const u8 {
        _ = self;
        return ObjectType.BOOLEAN_OBJ;
    }

    pub fn inspect(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        return try std.fmt.allocPrint(allocator, "{}", .{self.value});
    }
};

pub const Null = struct {
    const Self = @This();

    pub fn @"type"(self: *const Self) []const u8 {
        _ = self;
        return ObjectType.NULL_OBJ;
    }

    pub fn inspect(
        self: *const Self,
        allocator: Allocator,
    ) Allocator.Error![]const u8 {
        _ = self;
        _ = allocator;
        return "null";
    }
};
