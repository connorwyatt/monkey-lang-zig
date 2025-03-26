# Monkey Lang Interpreter/Compiler

In this repo I'm doing the following book in Zig:
[https://interpreterbook.com](https://interpreterbook.com).

The book is implemented in Go so I've had to translate the code to Zig, whilst
handling memory management manually and finding idiomatic ways to do certain
things the "Zig" way. One example is that Zig doesn't have interfaces, but
they're used heavily in the AST nodes in the book.
