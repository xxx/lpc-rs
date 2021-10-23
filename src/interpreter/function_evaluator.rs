use crate::interpreter::stack_frame::StackFrame;
use crate::semantic::program_function::ProgramFunction;
use crate::Result;
use std::rc::Rc;
use crate::asm::instruction::{Address, Instruction};
use crate::asm::register::Register;
use crate::interpreter::lpc_ref::LpcRef;
use crate::util::config::Config;
use crate::errors::LpcError;
use crate::interpreter::lpc_value::LpcValue;
use crate::interpreter::memory::Memory;
use crate::interpreter::efun::{EFUNS};
use crate::interpreter::call_stack::CallStack;
use delegate::delegate;
use crate::interpreter::efun::EFUN_PROTOTYPES;
use crate::interpreter::task_context::TaskContext;
use crate::interpreter::instruction_counter::InstructionCounter;
use crate::interpreter::efun::efun_context::EfunContext;
use std::borrow::Cow;
use std::cell::RefCell;
use crate::codegen::codegen_walker::INIT_PROGRAM;
use crate::interpreter::object_space::ObjectSpace;
use crate::interpreter::process::Process;
use crate::interpreter::program::Program;

/// Store the various limits we need to check against often, in a single place,
/// so we don't have to query the config every time.
#[derive(Debug, Clone, Default)]
struct Limits {
    /// How many instructions can run during this task, before we throw an error for
    /// too long execution? `0` means unlimited.
    max_task_instructions: usize,
}

impl From<&Config> for Limits {
    fn from(config: &Config) -> Self {
        Self {
            max_task_instructions: config.max_task_instructions().unwrap_or(0)
        }
    }
}

/// A type to track where `catch` calls need to go if there is an error
#[derive(Debug, Clone)]
struct CatchPoint {
    /// The index of the stack frame that contains this `catch`
    frame_index: usize,

    /// The address to jump in the current function, if there is an error
    address: Address,

    /// The register to put the error in, within the above [`StackFrame`]
    register: Register,
}

/// An abstraction to allow for isolated running to completion of a specified function.
/// It represents a single thread of execution
#[derive(Debug)]
pub struct FunctionEvaluator<'pool, const STACKSIZE: usize> {
    /// The call stack
    pub stack: CallStack<STACKSIZE>,

    /// How many instructions have run during the current [`Task`]?
    instruction_counter: InstructionCounter,

    /// Stack of [`CatchPoint`]s
    catch_points: Vec<CatchPoint>,

    /// A pointer to a memory pool to allocate new values from
    memory: Cow<'pool, Memory>,
}

impl<'pool, const STACKSIZE: usize> FunctionEvaluator<'pool, STACKSIZE> {
    pub fn new<T>(memory: T) -> Self
    where
        T: Into<Cow<'pool, Memory>>
    {
        Self {
            memory: memory.into(),
            stack: CallStack::default(),
            instruction_counter: InstructionCounter::default(),
            catch_points: Vec::new()
        }
    }

    /// Convenience helper to get a Program initialized.
    pub fn initialize_program<C, O>(&mut self, program: Program, config: C, object_space: O) -> Result<Rc<RefCell<Process>>>
    where
        C: Into<Rc<Config>>,
        O: Into<Rc<RefCell<ObjectSpace>>>
    {
        let process: Rc<RefCell<Process>> = Process::new(program).into();
        let context = TaskContext::new(config, process.clone(), object_space);
        context.insert_process(process.clone());
        let borrowed = process.borrow();
        let function = borrowed.lookup_function(INIT_PROGRAM);

        match function {
            Some(init_function) => {
                self.eval(
                    init_function.clone(),
                    &[],
                    context,
                )?;

                Ok(process.clone())
            }
            None => Err(LpcError::new("Init function not found?"))
        }
    }

    /// Evaluate `f` to completion, or an error
    pub fn eval<F>(&mut self, f: F, args: &[LpcRef], task_context: TaskContext) -> Result<()>
    where
        F: Into<Rc<ProgramFunction>>
    {
        let function = f.into();
        let process = task_context.process();
        let mut frame = StackFrame::new(process.clone(), function);
        // TODO: handle partial applications
        if args.len() > 0_usize {
            frame.registers[1..=args.len()].clone_from_slice(args);
        }

        self.stack.push(frame)?;

        let mut halted = false;

        while !halted {
            halted = match self.eval_one_instruction(&task_context) {
                Ok(x) => x,
                Err(e) => {
                    if !self.catch_points.is_empty() {
                        self.catch_error(e)?;
                        false
                    } else {
                        return Err(e);
                    }
                }
            };
        }

        Ok(())
    }

    /// Evaluate the instruction at the current value of the PC
    /// The boolean represents whether we are at the end of input (i.e. we should halt the machine)
    fn eval_one_instruction(&mut self, task_context: &TaskContext) -> Result<bool> {
        if self.stack.is_empty() {
            return Ok(true);
        }

        self.instruction_counter.increment(1)?;

        let frame = match self.stack.current_frame() {
            Ok(x) => x,
            Err(_) => return Ok(true),
        };

        let instruction = match frame.instruction() {
            Some(i) => i.clone(),
            None => return Ok(true),
        };

        frame.inc_pc();

        match instruction {
            Instruction::AConst(..) => {
                self.handle_aconst(&instruction)?;
            }
            Instruction::And(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x & y)?;
            }
            Instruction::Call {..} => {
                self.handle_call(&instruction, task_context)?;
            }
            Instruction::GStore(r1, r2) => {
                // store local r1 into global r2
                let registers = &mut self.stack.current_frame_mut()?.registers;
                let process = task_context.process();
                let globals = &mut process.borrow_mut().globals;
                globals[r2.index()].replace(registers[r1.index()].clone());
            }
            Instruction::IConst(r, i) => {
                let registers = &mut self.stack.current_frame_mut()?.registers;
                registers[r.index()] = LpcRef::Int(i);
            }
            Instruction::IConst0(r) => {
                let registers = &mut self.stack.current_frame_mut()?.registers;
                registers[r.index()] = LpcRef::Int(0);
            }
            Instruction::IConst1(r) => {
                let registers = &mut self.stack.current_frame_mut()?.registers;
                registers[r.index()] = LpcRef::Int(1);
            }
            Instruction::RegCopy(r1, r2) => {
                let registers = &mut self.stack.current_frame_mut()?.registers;
                registers[r2.index()] = registers[r1.index()].clone()
            }

            Instruction::Ret => {
                if let Some(ref mut frame) = self.pop_frame() {
                    let return_value = std::mem::take(&mut frame.registers[0]);
                    self.stack.set_call_result(return_value)?;

                    // self.popped_frame = Some(frame);
                }

                // halt at the end of all input
                if self.stack.is_empty() {
                    return Ok(true);
                }
            }

            Instruction::SConst(..) => {
                self.handle_sconst(&instruction)?;
            }

            x => todo!("{}", x)
        }

       Ok(false)
    }

    fn handle_aconst(&mut self, instruction: &Instruction) -> Result<()> {
        match instruction {
            Instruction::AConst(r, vec) => {
                let registers = &mut self.stack.current_frame_mut()?.registers;
                let vars = vec
                    .iter()
                    .map(|i| registers[i.index()].clone())
                    .collect::<Vec<_>>();
                let new_ref = self.memory.value_to_ref(LpcValue::from(vars));

                registers[r.index()] = new_ref;

                Ok(())
            }
            _ => {
                Err(self.runtime_error("non-AConst instruction passed to `handle_aconst`"))
            }
        }
    }

    fn handle_call(&mut self, instruction: &Instruction, task_context: &TaskContext) -> Result<()> {
        match instruction {
            Instruction::Call {
                name,
                num_args,
                initial_arg,
            } => {
                let frame = self.stack.current_frame()?;
                let process = frame.process.clone();
                let borrowed = process.borrow();
                let function = borrowed.functions.get(name);
                let mut new_frame = if let Some(func) = function {
                    StackFrame::new(process.clone(), func.clone())
                } else if let Some(prototype) = EFUN_PROTOTYPES.get(name.as_str()) {
                    let sym = ProgramFunction::new(name.clone(), prototype.num_args, 0);

                    StackFrame::new(process.clone(), Rc::new(sym))
                } else {
                    println!("proc {:#?}", process);
                    println!("functions {:#?}", borrowed.functions);
                    let msg = format!("Call to unknown function `{}`", name);
                    return Err(self.runtime_error(msg));
                };

                let num_args = *num_args;

                // copy argument registers from old frame to new
                if num_args > 0_usize {
                    let index = initial_arg.index();
                    // let current_frame = self.stack.last().unwrap();
                    let registers = &frame.registers;
                    println!("copying registers: {:?}", registers);
                    new_frame.registers[1..=num_args]
                        .clone_from_slice(&registers[index..(index + num_args)]);
                }

                let function_is_local = frame.process.borrow().functions.contains_key(name);

                // println!("pushing frame in Call: {:?}", new_frame);
                self.stack.push(new_frame)?;

                // if let Some(x) = self.process.functions.get(name) {
                //     frame.set_pc(x.address);
                // } else if let Some(efun) = EFUNS.get(name.as_str()) {
                if let Some(efun) = EFUNS.get(name.as_str()) {
                    // TODO this is almost certain broken for efun overrides
                    // the efun is responsible for populating the return value in its own frame
                    let frame = self.stack.current_frame_mut()?;
                    let mut ctx = EfunContext::new(frame, task_context, &self.memory);
                    efun(&mut ctx)?;

                    if let Some(frame) = self.stack.pop() {
                        self.stack.copy_call_result(&frame)?;
                        // self.popped_frame = Some(frame);
                    }
                } else if !function_is_local {
                    return Err(self.runtime_error(format!(
                        "Call to unknown function (that had a valid prototype?) `{}`",
                        name
                    )));
                }

                Ok(())
            },

            _ => {
                Err(self.runtime_error("non-Call instruction passed to `handle_call`"))
            }
        }
    }

    fn handle_sconst(&mut self, instruction: &Instruction) -> Result<()> {
        match instruction {
            Instruction::SConst(r, s) => {
                let registers = &mut self.stack.current_frame_mut()?.registers;
                let new_ref = self.memory.value_to_ref(LpcValue::from(s));

                registers[r.index()] = new_ref;
                Ok(())
            }
            _ => {
                Err(self.runtime_error("non-SConst instruction passed to `handle_sconst`"))
            }
        }
    }

    /// Set the state to handle a caught error.
    /// Panics if there aren't actually any catch points.
    fn catch_error(&mut self, error: LpcError) -> Result<()> {
        let catch_point = self.catch_points.last().unwrap();
        let result_index = catch_point.register.index();
        let frame_index = catch_point.frame_index;
        let new_pc = catch_point.address;

        // clear away stack frames that won't be executed any further, which lie between the
        // error and the catch point's stack frame.
        // Does nothing if you're already in the correct stack frame, or one away.
        self.stack.truncate(frame_index + 2);

        // If these aren't equal, we're already in the correct stack frame.
        if self.stack.len() == frame_index + 2 {
            // Pop the final frame via pop_frame(), to keep other state changes to a single
            // code path, (e.g. changing the current process)
            if let Some(_frame) = self.pop_frame() {
                // self.popped_frame = Some(frame);
            }
        }

        if self.stack.is_empty() {
            return Err(self.runtime_error("stack is empty after popping to catch point?"));
        }

        // set up the catch point's return value
        let value = LpcValue::from(error.to_string());
        let lpc_ref = self.memory.value_to_ref(value);
        let frame = self.stack.current_frame_mut()?;
        frame.registers[result_index] = lpc_ref;

        // jump to the corresponding catchend instruction
        frame.set_pc(new_pc);

        Ok(())
    }

    fn binary_operation<F>(
        &mut self,
        r1: Register,
        r2: Register,
        r3: Register,
        operation: F,
    ) -> Result<()>
    where
        F: Fn(&LpcRef, &LpcRef) -> Result<LpcValue>,
    {
        let frame = self.stack.current_frame_mut()?;
        let ref1 = &frame.resolve_lpc_ref(r1);
        let ref2 = &frame.resolve_lpc_ref(r2);

        match operation(ref1, ref2) {
            Ok(result) => {
                let var = self.memory.value_to_ref(result);

                let registers = &mut frame.registers;
                registers[r3.index()] = var
            }
            Err(e) => {
                return Err(e.with_span(frame.current_debug_span()));
            }
        }

        Ok(())
    }

    delegate! {
        to self.stack {
            /// Pop the top frame from the stack, and return it.
            #[call(pop)]
            fn pop_frame(&mut self) -> Option<StackFrame>;
        }
    }

    /// convenience helper to generate runtime errors
    #[inline]
    fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        self.stack.runtime_error(msg)
    }
}
