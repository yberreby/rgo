# Sun Mar 27 - Starting the journey

I've always wanted to write a compiler or an interpreter - *something* that
takes code and turns it into something runnable.

Lisp has been reimplemented again and again, and was too simple. C compilers are
also commonplace, AND the language is, well, not a joy to work with.

So I chose Go. I dislike many of the language's design choices but it seemed
like a good fit. To my knowledge, there is no reimplementation of the Go
compiler in Rust or similar languages, so I decided to write one.

There's an additional challenge: I know very, very little Go. As of writing,
I've only written a few hundred lines of Go, and found two bugs in the viper
package. So I'm learning more Go by writing a compiler for it :)

I'm starting with parsing. I have received no formal education on compilers, but
as far as I know, the first step toward building a compiler is to parse the
source code into an Abstract Syntax Tree, or AST.

To do that, I'm using the [Go Specification](https://golang.org/ref/spec) to
look up the syntax of various constructs and translating that into a bunch of
Rust structs and enums. This is somewhat tedious, but I'm learning a few things
about Go's syntax along the way.
