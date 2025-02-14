const std = @import("std");

pub const Token = struct {
    type: []const u8,
    literal: []const u8,

    pub const ILLEGAL: []const u8 = "ILLEGAL";
    pub const EOF: []const u8 = "EOF";

    // Identifiers + literals
    pub const IDENT: []const u8 = "IDENT"; // add, foobar, x, y, ...
    pub const INT: []const u8 = "INT"; // 1343456

    // Operators
    pub const ASSIGN: []const u8 = "=";
    pub const PLUS: []const u8 = "+";

    // Delimiters
    pub const COMMA: []const u8 = ",";
    pub const SEMICOLON: []const u8 = ";";

    pub const LPAREN: []const u8 = "(";
    pub const RPAREN: []const u8 = ")";
    pub const LBRACE: []const u8 = "{";
    pub const RBRACE: []const u8 = "}";

    // Keywords
    pub const FUNCTION: []const u8 = "FUNCTION";
    pub const LET: []const u8 = "LET";

    pub fn lookupIdent(ident: []const u8) []const u8 {
        if (std.mem.eql(u8, ident, "fn")) {
            return Token.FUNCTION;
        } else if (std.mem.eql(u8, ident, "let")) {
            return Token.LET;
        } else {
            return Token.IDENT;
        }
    }
};
