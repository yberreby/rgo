# Log

This is the log file for the `rgo` project. I (@yberreby) write my thoughts down
periodically. I hope this will help understand the history of the project as it
grows and time passes.

## Sun Mar 27 - Starting the journey

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


## Wed Mar 30 19:18 - Thoughts on testing

A *critical* part of a complex system like a compiler is testing, so we'll want
to write a lot of tests to cover as much surface as possible.

There are three main things we want to test:

- lexical analysis
- parsing
- translation

If all three phases works correctly, we have are likely to produce a correct
program.

Now, we would only test the *output* of the program, because in most cases, if
either lexical analysis or parsing produce incorrect results or fail, the output
of the compiled program will be wrong. The advantage of this approach is that
it's easier to write tests for a handful of big, complex programs than for a
myriad of very small programs. The disadvantage is that in case there's a
failure, it's much harder to track it down to the piece of code that is
responsible.

I think it is best to combine the two approaches: integration tests that only
care about the output, and unit tests that care only about a very small part of
the code. Lexing should be very easy to test; parsing, a bit harder, as AFAIK it
requires more context (e.g. `foo` can be a package name, a package alias, a
constant, a function parameter...).

## Thu Mar 31 15:16 - Next steps

The lexer can now tokenize a "Hello, rgo" program properly!

Next steps: integer literals (very easy); semicolon; various compound tokens.

I'm going to have to set this project aside for a few months because of my
studies.
