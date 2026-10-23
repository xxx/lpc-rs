# Write your first LPC program

You will build a small lantern program that spends fuel when it burns. Along
the way you will run LPC, write functions, change state, and diagnose a
compiler error. You need basic familiarity with variables and functions in
another programming language, but no LPC experience.

## Prepare the runner

You need an lpc-rs source checkout and the Rust build tools described in the
[installation guide](../../README.md#build-and-install). From the checkout's
root directory, install the command-line compiler and runner:

```sh
cargo install --locked --path lpc-rs-lpcc
```

Ensure Cargo's binary directory, normally `~/.cargo/bin`, is on your `PATH`.
You also need a text editor and a terminal with a POSIX shell.

Create a working directory and move into it:

```sh
mkdir -p ~/lpc-learning
cd ~/lpc-learning
```

Run the remaining shell commands in this directory. Write the LPC examples in
your editor and save them to the specified files.

## Run a greeting

Create `hello.c` with this entire contents:

```lpc
#pragma strict_types

void create() {
    dump("Your lantern is ready.\n");
}
```

Run it:

```sh
LPC_LIB_DIR=. lpc-rs-lpcc hello.c
```

The output includes:

```text
Your lantern is ready.
```

`LPC_LIB_DIR=.` tells the runner to use the current directory as the root for
LPC files. `hello.c` is the file to compile and run. Although LPC uses the
`.c` extension, these files are compiled by the LPC driver.

Read the program from the outside in:

- `#pragma strict_types` asks the compiler to require explicit return and
  parameter types. Keep it at the top of the files in these lessons.
- `void create()` defines a function named `create` that takes no arguments
  and returns no useful value. Braces surround its body.
- `dump(...)` displays a value. Text in double quotes is a string; `\n` puts
  the next output on a new line. A statement ends with a semicolon.

The runner compiles the file, creates a running **object** from its program,
and calls that object's `create()` function. Then the runner exits. In a game
server, objects remain available for later player commands and callbacks.

`create` is an **apply**: a function you write that the driver calls at a
particular time. `dump` is an **efun**: a built-in function supplied by the
driver. Ordinary functions you write are often called **lfuns** in LPC docs.
Here there is no connected player, so `dump` goes to the debug log, which
defaults to the terminal.

Change the greeting, save the file, and run the same command again. You should
see your new text.

If the shell cannot find `lpc-rs-lpcc`, check your `PATH` or invoke
`~/.cargo/bin/lpc-rs-lpcc` directly. If the compiler cannot find `hello.c`,
check that you saved it in `~/lpc-learning` and are running the command there.

## Give the lantern state

Create a second file, `lantern.c`:

```lpc
#pragma strict_types

private int fuel = 3;

int burn(int amount) {
    if (amount <= 0 || amount > fuel) {
        return 0;
    }

    fuel = fuel - amount;
    return 1;
}

void create() {
    dump("Fuel before: " + fuel + "\n");

    if (burn(1)) {
        dump("The lantern glows.\n");
    }

    dump("Fuel after: " + fuel + "\n");
}
```

Run it:

```sh
LPC_LIB_DIR=. lpc-rs-lpcc lantern.c
```

Expected output:

```text
Fuel before: 3
The lantern glows.
Fuel after: 2
```

`int` declares an integer. `fuel` is outside the functions, so it is an
object variable, often called a **global** in LPC. It belongs to this object
and keeps its value between calls. The initializer `= 3` runs before
`create()`. `private` keeps the variable accessible only to the code that
declares it.

`amount` is a parameter: each call to `burn` supplies its value. The leading
`int` in `int burn(int amount)` says the function returns an integer.
`return` ends the call immediately and gives that value to its caller.

LPC commonly uses `1` for success and `0` for failure. In a condition, zero is
false and a nonzero integer is true. Our `burn` function refuses a nonpositive
amount or an amount larger than the remaining fuel. `||` means “or.” A
successful call reduces the fuel and returns `1`.

The `+` operations in the messages join text and the integer's textual value
into a new string. `=` assigns a value; `==` tests whether values are equal.
Both `// line comments` and `/* block comments */` are available when you
need to explain your code.

Try changing `burn(1)` to `burn(4)`. The glow message should disappear and the
fuel should stay at `3`: the failed call did not spend anything.

## Repeat an action

Replace only the `create()` function in `lantern.c` with this one, keeping
the variable and `burn()` function above it:

```lpc
void create() {
    for (int attempt = 1; attempt <= 4; attempt = attempt + 1) {
        if (burn(1)) {
            dump("The lantern glows.\n");
        } else {
            dump("The lantern is empty.\n");
        }
    }

    dump("Fuel left: " + fuel + "\n");
}
```

Run `lantern.c` again. Expect:

```text
The lantern glows.
The lantern glows.
The lantern glows.
The lantern is empty.
Fuel left: 0
```

The three parts of the `for` loop initialize a counter, decide whether to run
another iteration, and advance the counter after each iteration. `attempt`
is a local variable used by this call to `create`. The `else` branch runs
when `burn(1)` returns zero.

Run the program once more. You get the same output because each invocation
starts a new runner and creates a fresh object. A global keeps state during
an object's lifetime; saving it across server restarts requires explicit
save/load code.

## Read a compiler error

Temporarily change the declaration of `fuel` to:

```lpc
private int fuel = "three";
```

Ask the runner to check the file without running `create()`:

```sh
LPC_LIB_DIR=. lpc-rs-lpcc --check lantern.c
```

The compiler reports a type mismatch and points to the declaration. It expects
an `int`, but `"three"` is a `string`. Read the diagnostic's file and line
location first, then check the marked expression and the surrounding code.
A missing semicolon can make the compiler complain at the following token.

Restore `private int fuel = 3;` and check again. A successful check produces
no program output and returns you to the shell. Run without `--check` to see
the lantern burn again.

## Try a change

Set the initial fuel to `5` and change the loop to make three calls to
`burn(2)`. Before running it, predict the result. You should get two glow
messages, one empty message, and `Fuel left: 1`. The failure means there was
not enough fuel for the requested amount; the message could be more precise.

Change that failure message to `There is not enough fuel.` and run again.
You have changed both the rule and the explanation a reader sees.

Continue with [Make objects players can use](first_objects.md), where the
same idea becomes two lanterns in a multiplayer room. For more runner options,
see the [compiler guide](../../lpc-rs-lpcc/README.md).
