use lpc_rs::Result;
use std::env;

use itertools::Itertools;
use lazy_format::lazy_format;
use lpc_rs::{
    compiler::compile_file,
    interpreter::{
        asm_interpreter::{current_registers, AsmInterpreter},
        program::Program,
    },
};
use rustyline::{error::ReadlineError, Editor};

const DEFAULT_FILE: &str = "local/mathfile.c";

const HELP: &str = r"
           lpcdb Help
? | .help - This help text
q | ctrl-C | ctrl-D - Exit the program
load <path>: Load the program at <path>
             as the current program
i: Print the current instruction
r: Print the current stack frame's registers
s: Print the call stack
pc: Print the current pc value
l: Show a listing of instructions around
   the current instruction
n: Execute the next instruction
";

fn main() {
    let args: Vec<String> = env::args().collect();

    let filename = args.get(1).map_or(DEFAULT_FILE, |name| name);

    match compile_file(filename) {
        Ok(program) => {
            let mut repl = Repl::new(program);
            let _ = repl.repl();
        }
        Err(e) => eprintln!("unable to compile {}: {:?}", filename, e),
    }
}

#[derive(Debug, Default)]
pub struct Repl {
    vm: AsmInterpreter,
}

impl Repl {
    pub fn new(program: Program) -> Self {
        let mut vm = AsmInterpreter::default();
        vm.load(program);
        vm.setup_program_globals_frame();

        Self { vm }
    }

    pub fn repl(&mut self) -> Result<()> {
        let mut rl = Editor::<()>::new();
        println!("Welcome to lpcdb! Type '?' for help");

        loop {
            let readline = rl.readline("> ");

            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str());

                    match line.trim() {
                        "q" | "quit" => {
                            std::process::exit(0);
                        }
                        "help" | "?" => {
                            println!("{}", HELP);
                        }
                        "i" | "instruction" => match self.vm.process.instruction() {
                            Some(x) => println!("{}", x),
                            None => println!("There's no instruction at the current address."),
                        },
                        "pc" => {
                            println!("{}", self.vm.process.pc())
                        }
                        "r" | "registers" => {
                            let r = match current_registers(&self.vm.stack) {
                                Ok(r) => r,
                                Err(e) => {
                                    println!("Error: {}", e);
                                    continue;
                                }
                            };

                            println!(
                                "{}",
                                r.iter()
                                    .enumerate()
                                    .map(|(i, r)| lazy_format!("r{}: {}", i, r))
                                    .join("\n")
                            );
                        }
                        "n" | "next" => {
                            if let Some(i) = self.vm.process.instruction() {
                                println!("executing: {}", i);
                            }
                            match self.vm.eval_one_instruction() {
                                Ok(halted) => {
                                    if halted {
                                        println!("machine has halted.");
                                    }
                                }
                                Err(e) => {
                                    println!("Error: {}", e);
                                }
                            }
                        }
                        x => {
                            println!("Invalid input: {}", x);
                        }
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    println!("...Interrupted");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    println!("EOF");
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break;
                }
            }
        }

        Ok(())
    }
}
