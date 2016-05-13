# Contributing

You can find sections of the code in need of attention by exploring GitHub
issues, and/or with the help of grep:

``` shell
$ grep -RE 'XXX|TODO|FIXME|unimplemented' src
```

Contributions are encouraged, but to maintain a high level of quality, we ask
that you follow the guidelines laid out in this document.

## Guidelines

**Run tests** with `cargo test` after your committing your changes.

We use [`rustfmt`](https://github.com/rust-lang-nursery/rustfmt) to maintain a
consistent style throughout the project. Please run it on your code before
submitting a PR.

Put tests for a module in a separate file named `test.rs`, and import them like
so from the module you want to test:

```rust
#[cfg(test)]
mod test;
```

Rebase your changes on top of `origin/master` before submitting a PR, to keep
your branch up-to-date.

When in doubt, [refer to the Go spec](https://golang.org/ref/spec).

### Git Best Practices

We want to keep the git history as clean as possible.

If you need to make changes to a Pull Request after submitting it, please
include them in your previous commit(s) and force-push instead of adding new
commits. You can do so by staging your changes and running `git commit --amend`.

If the `master` branch is updated while your PR is open, **do not merge it** into
your feature branch. Instead, rebase your branch on top of `origin/master` and
force-push to your fork.

Where possible, please avoid mixing up many unrelated changes in single PR;
prefer making several smaller PRs instead. If you have to change many things in
a single PR, try to separate them into semantically meaningul commits. This is
not a hard rule, but it is preferred.



