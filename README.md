This will eventually be an [LPC](https://mud.fandom.com/wiki/LPC) compiler and driver, but currently it's WIP and a personal project for me to learn from and hack on.

### Quick start if you want to tinker

1. pull this repo down locally
2. cd to the repo root
3. open `hello.c`, with content:
```c
#define MSG "hello, world!"

void create() {
    dump(MSG);
}
```
4`cargo run -p lpc-rs-lpcc -- hello.c`