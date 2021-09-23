This will eventually be an [LPC](https://mud.fandom.com/wiki/LPC) compiler and driver, but currently it's WIP and a personal project for me to learn from and hack on.

### Quick start if you want to tinker

1. pull this repo down locally
2. cd to the repo root
3. `cp config.toml.default config.toml`
4. `mkdir lib`
5. `cd lib`
6. open `hello.c`, with content:
```
#define MSG "hello, world!"

void create() {
    dump(MSG);
}
```
7. `cd ..` (This step is needed only because it's making an assumption that `config.toml` is in the same directory you're running from. It will eventually be squeezed out.)
8. `cargo run --bin lpcc lib/hello.c`