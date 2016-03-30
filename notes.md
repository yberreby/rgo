# Notes

## Limitations

Not done at first; should be done in the future.

- Unicode identifiers are not supported. I'm not sure how to do this right;
  additionally, the Go specification defines runes as Unicode code points, but
  Rust `char`s are Unicode scalar values.
- Complex and 128-bit numbers are not supported.
- Strings are not interned.
