# rgo

`rgo` is a **work-in-progress** (i.e. unfinished) Go compiler, written
in Rust.

For now, this is primarily a fun learning project.

I chose Go as the source language because C compilers have been written over and
over, and I wanted to do something new. Go's spec is pretty simple, so it seemed
like a good choice. The fact that is has a GC also makes implementing a compiler
for it more challenging and, therefore, more interesting.

Additionally, Go's reference implementation uses a custom backend for
optimization and codegen, while `rgo` will use LLVM for optimization and machine
code generation.

## License

Copyright (c) 2016 Yohaï Berreby.

Licensed under either of

 * [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)
 * [MIT license](http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
