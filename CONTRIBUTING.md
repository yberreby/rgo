# Contributing

If you wish to contribute, please follow these guidelines:

- **run tests** before and after your change with `cargo test`
- run `rustfmt` on your code before submitting a PR
- put tests for a module in a separate file named "test.rs", and import it like
  so from the module you want to test:

  ```rust
  #[cfg(test)]
  mod test;
  ```

You can find sections of the code in need of attention by exploring GitHub
issues, and/or with the help of grep:

``` shell
$ grep -RE '(XXX)|(TODO)|(FIXME)' src
```
