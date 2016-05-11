# Contributing

If you wish to contribute, please follow these guidelines:

- **run tests** before and after your change with `cargo test`
- run `rustfmt` on your code before submitting a PR
- rebase your changes on top of `origin/master` before submitting a PR, to keep
  your branch up-to-date
- strive to keep the git history clean
- put tests for a module in a separate file named "test.rs", and import it like
  so from the module you want to test:

  ```rust
  #[cfg(test)]
  mod test;
  ```
- when in doubt, [refer to the Go spec](https://golang.org/ref/spec)

You can find sections of the code in need of attention by exploring GitHub
issues, and/or with the help of grep:

``` shell
$ grep -RE 'XXX|TODO|FIXME|unimplemented' src
```
