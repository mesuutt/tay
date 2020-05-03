## tay

Interpreted programming language based on https://interpreterbook.com/.

![](https://mesuutt.com/static/tay/tay-lang.svg)

You can look at [examples](https://github.com/mesuutt/tay/tree/master/examples) directory for more usage examples.

---

#### Development

```sh
# Start REPL
cargo run

# Execute file
cargo run examples/fib.ty

# Build
cargo build

# Test
cargo test
```

##### TODO

- [x] Interpreter
- [ ] VM
- [ ] Error reporting
- [ ] Loops
- [ ] Modules
- [ ] Python like `*args` and `**kwargs`
- [ ] Js like arrow functions
- [ ] Standard libraries
- [ ] Playground
- [ ] And more ...