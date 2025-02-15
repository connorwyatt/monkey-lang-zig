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
    pub const MINUS: []const u8 = "-";
    pub const BANG: []const u8 = "!";
    pub const ASTERISK: []const u8 = "*";
    pub const SLASH: []const u8 = "/";

    pub const LT: []const u8 = "<";
    pub const GT: []const u8 = ">";

    pub const EQ: []const u8 = "==";
    pub const NOT_EQ: []const u8 = "!=";

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
    pub const TRUE: []const u8 = "TRUE";
    pub const FALSE: []const u8 = "FALSE";
    pub const IF: []const u8 = "IF";
    pub const ELSE: []const u8 = "ELSE";
    pub const RETURN: []const u8 = "RETURN";

    pub fn lookupIdent(ident: []const u8) []const u8 {
        if (std.mem.eql(u8, ident, "fn")) {
            return Token.FUNCTION;
        } else if (std.mem.eql(u8, ident, "let")) {
            return Token.LET;
        } else if (std.mem.eql(u8, ident, "true")) {
            return Token.TRUE;
        } else if (std.mem.eql(u8, ident, "false")) {
            return Token.FALSE;
        } else if (std.mem.eql(u8, ident, "if")) {
            return Token.IF;
        } else if (std.mem.eql(u8, ident, "else")) {
            return Token.ELSE;
        } else if (std.mem.eql(u8, ident, "return")) {
            return Token.RETURN;
        } else {
            return Token.IDENT;
        }
    }
};
