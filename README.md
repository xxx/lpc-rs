This will eventually be an [LPC](https://mud.fandom.com/wiki/LPC) compiler and driver, but currently it's WIP and a personal project for me to learn from and hack on.

### Quick start if you want to tinker

1. pull this repo down locally
2. cd to the repo root
3. `mkdir lib`
3. create `lib/hello.c`, with content:
    ```c
    #define MSG "hello, world!"
    
    void create() {
        dump(MSG);
    }
    ```
4. `cargo run -p lpc-rs-lpcc lib/hello.c`

Some compile-time defaults can be changed in `lpc-rs/src/compile_time_config.rs`.

Runtime configuration is entirely via environment variables, and `.env` files are also supported.
See `default.env` for a commented example.
