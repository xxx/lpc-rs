use crate::{
    asm::{
        instruction::{Address, Instruction},
        register::Register,
    },
    codegen::codegen_walker::INIT_PROGRAM,
    errors::LpcError,
    interpreter::{
        call_stack::CallStack,
        efun::{efun_context::EfunContext, EFUNS, EFUN_PROTOTYPES},
        instruction_counter::InstructionCounter,
        lpc_ref::LpcRef,
        lpc_value::LpcValue,
        memory::Memory,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        stack_frame::StackFrame,
        task_context::TaskContext,
    },
    semantic::program_function::ProgramFunction,
    util::config::Config,
    Result,
};
use std::{borrow::Cow, cell::RefCell, rc::Rc};

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
            max_task_instructions: config.max_task_instructions().unwrap_or(0),
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
pub struct Task<'pool, const STACKSIZE: usize> {
    /// The call stack
    pub stack: CallStack<STACKSIZE>,

    /// How many instructions have run during the current [`Task`]?
    instruction_counter: InstructionCounter,

    /// Stack of [`CatchPoint`]s
    catch_points: Vec<CatchPoint>,

    /// A pointer to a memory pool to allocate new values from
    memory: Cow<'pool, Memory>,

    /// Store the most recently popped frame, for testing
    #[cfg(test)]
    pub popped_frame: Option<StackFrame>,

    /// Store a snapshot of a specific state, for testing
    #[cfg(test)]
    pub snapshot: Option<Box<Task<'pool, STACKSIZE>>>,
}

impl<'pool, const STACKSIZE: usize> Task<'pool, STACKSIZE> {
    pub fn new<T>(memory: T) -> Self
    where
        T: Into<Cow<'pool, Memory>>,
    {
        Self {
            memory: memory.into(),
            stack: CallStack::default(),
            instruction_counter: InstructionCounter::default(),
            catch_points: Vec::new(),

            #[cfg(test)]
            popped_frame: None,

            #[cfg(test)]
            snapshot: None
        }
    }

    /// Convenience helper to get a Program initialized.
    pub fn initialize_program<C, O>(
        &mut self,
        program: Program,
        config: C,
        object_space: O,
    ) -> Result<TaskContext>
    where
        C: Into<Rc<Config>>,
        O: Into<Rc<RefCell<ObjectSpace>>>,
    {
        let init_function = {
            let function = program.lookup_function(INIT_PROGRAM);
            if function.is_none() {
                return Err(LpcError::new("Init function not found?"));
            }

            function.unwrap().clone()
        };
        let process: Rc<RefCell<Process>> = Process::new(program).into();
        let context = TaskContext::new(config, process.clone(), object_space);
        context.insert_process(process.clone());

        self.eval(init_function, &[], context)
    }

    /// Evaluate `f` to completion, or an error
    pub fn eval<F>(&mut self, f: F, args: &[LpcRef], task_context: TaskContext) -> Result<TaskContext>
    where
        F: Into<Rc<ProgramFunction>>,
    {
        let function = f.into();
        let process = task_context.process();
        let mut frame = StackFrame::new(process, function);
        // TODO: handle partial applications
        if !args.is_empty() {
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

        Ok(task_context)
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
            Instruction::Call { .. } => {
                self.handle_call(&instruction, task_context)?;
            }
            Instruction::FConst(r, f) => {
                let registers = &mut self.stack.current_frame_mut()?.registers;
                registers[r.index()] = LpcRef::Float(f);
            }
            Instruction::GLoad(r1, r2) => {
                // load from global r1, into local r2
                let process = task_context.process();
                let global = process.borrow().globals[r1.index()].borrow().clone();
                let registers = &mut self.stack.current_frame_mut()?.registers;
                registers[r2.index()] = global
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
                }

                // halt at the end of all input
                if self.stack.is_empty() {
                    return Ok(true);
                }
            }

            Instruction::SConst(..) => {
                self.handle_sconst(&instruction)?;
            }

            x => todo!("{}", x),
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
            _ => Err(self.runtime_error("non-AConst instruction passed to `handle_aconst`")),
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

                    if let Some(frame) = self.pop_frame() {
                        self.stack.copy_call_result(&frame)?;
                    }
                } else if !function_is_local {
                    return Err(self.runtime_error(format!(
                        "Call to unknown function (that had a valid prototype?) `{}`",
                        name
                    )));
                }

                Ok(())
            }

            _ => Err(self.runtime_error("non-Call instruction passed to `handle_call`")),
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
            _ => Err(self.runtime_error("non-SConst instruction passed to `handle_sconst`")),
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
            self.pop_frame();
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

    /// convenience helper to generate runtime errors
    #[inline]
    fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        self.stack.runtime_error(msg)
    }

    /// Pop the top frame from the stack, and return it.
    #[inline]
    fn pop_frame(&mut self) -> Option<StackFrame> {
        let frame = self.stack.pop();

        #[cfg(test)]
        {
            self.popped_frame = frame.clone();
        }

        frame
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use super::*;
    use crate::{compiler::Compiler, extract_value, LpcFloat, LpcInt};
    use indoc::indoc;
    use std::hash::{Hash, Hasher};
    use crate::interpreter::MAX_CALL_STACK_SIZE;

    /// TODO: share this
    fn compile_prog(code: &str) -> Program {
        let compiler = Compiler::default();
        compiler
            .compile_string("~/my_file.c", code)
            .expect("Failed to compile.")
    }

    fn run_prog(code: &str) -> Task<MAX_CALL_STACK_SIZE> {
        let mut task = Task::new(Memory::default());
        let program = compile_prog(code);
        let _ = task.initialize_program(program, Config::default(), ObjectSpace::default()).expect("failed to initialize");

        task
    }

    /// A type to make it easier to set up test expectations for register contents
    #[derive(Debug, PartialEq, Eq)]
    enum BareVal {
        String(String),
        Int(LpcInt),
        Float(LpcFloat),
        Array(Box<Vec<BareVal>>),
        Mapping(HashMap<BareVal, BareVal>),
        Object(HashMap<BareVal, BareVal>),
    }

    impl From<&LpcRef> for BareVal {
        fn from(lpc_ref: &LpcRef) -> Self {
            match lpc_ref {
                LpcRef::Float(x) => BareVal::Float(*x),
                LpcRef::Int(x) => BareVal::Int(*x),
                LpcRef::String(x) => {
                    let xb = x.borrow();
                    let s = extract_value!(&*xb, LpcValue::String);
                    BareVal::String(s.clone())
                }
                LpcRef::Array(x) => {
                    let xb = x.borrow();
                    let a = extract_value!(&*xb, LpcValue::Array);
                    let array = a.into_iter().map(|item| item.into()).collect::<Vec<_>>();
                    BareVal::Array(Box::new(array))
                }
                LpcRef::Mapping(x) => {
                    let xb = x.borrow();
                    let m = extract_value!(&*xb, LpcValue::Mapping);
                    let mapping = m
                        .into_iter()
                        .map(|(k, v)| (k.into(), v.into()))
                        .collect::<HashMap<_, _>>();
                    BareVal::Mapping(mapping)
                }
                LpcRef::Object(_x) => {
                    todo!()
                    // let o = extract_value!(&*x, LpcValue::Object);
                }
                LpcRef::Function(_x) => {
                    todo!()
                }
            }
        }
    }

    impl PartialEq<LpcRef> for BareVal {
        fn eq(&self, other: &LpcRef) -> bool {
            &BareVal::from(other) == self
        }
    }

    impl Hash for BareVal {
        fn hash<H: Hasher>(&self, state: &mut H) {
            match self {
                BareVal::Float(x) => x.hash(state),
                BareVal::Int(x) => x.hash(state),
                BareVal::String(x) => x.hash(state),
                BareVal::Array(x) => std::ptr::hash(&**x, state),
                BareVal::Mapping(x) => std::ptr::hash(&*x, state),
                BareVal::Object(x) => std::ptr::hash(&*x, state),
            }
        }
    }

    mod test_instructions {
        use super::*;
        use crate::interpreter::task::tests::BareVal::*;

        mod test_aconst {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed *a = ({ 12, 4.3, "hello", ({ 1, 2, 3 }) });
                "##};

                let task = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(12),
                    Float(LpcFloat::from(4.3)),
                    String("hello".into()),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(3)].into()),
                    Array(
                        vec![
                            Int(12),
                            Float(LpcFloat::from(4.3)),
                            String("hello".into()),
                            Array(
                                vec![Int(1), Int(2), Int(3)].into(),
                            ),
                        ]
                        .into(),
                    ),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_and {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 15 & 27;
                    mixed b = 0 & a;
                "##};

                let task = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(11), Int(0), Int(11), Int(0)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_andand {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 123 && 333;
                    mixed b = 0;
                    mixed c = b && a;
                "##};

                let task = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(123),
                    Int(333),
                    Int(333),
                    Int(0),
                    Int(0),
                    Int(0),
                    Int(0),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_call {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = tacos();
                    int tacos() { return 666; }
                "##};

                let task = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(666), Int(666)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_call_other {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    int tacos() { return 666; }
                "##};

                let task = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![BareVal::Int(666), BareVal::Int(666), BareVal::Int(0)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_catch {
            use super::*;

            #[test]
            fn stores_the_error_string() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        catch(10 / j);

                        debug("in_memory_snapshot");
                    }
                "##};

                let task = run_prog(code);
                let stack = task.snapshot.unwrap().stack;

                // The top of the stack in the snapshot is the object initialization frame,
                // which is not what we care about here, so we get the second-to-top frame instead.
                let registers = &stack[stack.len() - 2].registers;

                let expected = vec![
                    Int(0),
                    Int(0),
                    String("Runtime Error: Division by zero".into()),
                    Int(10),
                    Int(0),
                    String("in_memory_snapshot".into()),
                    Int(0),
                ];

                assert_eq!(&expected, registers);
            }

            #[test]
            fn stores_zero_when_no_error() {
                let code = indoc! { r##"
                    void create() {
                        int j = 5;
                        catch(10 / j);

                        debug("in_memory_snapshot");
                    }
                "##};

                let task = run_prog(code);
                let stack = task.snapshot.unwrap().stack;

                // The top of the stack in the snapshot is the object initialization frame,
                // which is not what we care about here, so we get the second-to-top frame instead.
                let registers = &stack[stack.len() - 2].registers;

                let expected = vec![
                    Int(0),
                    Int(5),
                    Int(0),
                    Int(10),
                    Int(2),
                    String("in_memory_snapshot".into()),
                    Int(0),
                ];

                assert_eq!(&expected, registers);
            }
        }

        mod test_catch_end {
            use super::*;

            #[test]
            fn pops_the_catch_point() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        catch(catch(catch(catch(10 / j))));
                    }
                "##};

                let interpreter = run_prog(code);

                assert!(interpreter.catch_points.is_empty());
            }
        }
//
//         mod test_eq_eq {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 2 == 2;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(2),
//                     BareVal::Int(2),
//                     BareVal::Int(1),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_fconst {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 3.14;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![BareVal::Int(0), BareVal::Float(3.14.into())];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_gload {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 3.14;
//                     mixed j = q + 1.1;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Float(3.14.into()),
//                     BareVal::Float(3.14.into()),
//                     BareVal::Float(1.1.into()),
//                     BareVal::Float(4.24.into()),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//
//                 let global_registers = interpreter
//                     .process
//                     .globals
//                     .iter()
//                     .map(|global| (*global.borrow()).clone())
//                     .collect::<Vec<_>>();
//
//                 let global_expected = vec![
//                     BareVal::Int(0), // "wasted" global r0
//                     BareVal::Float(3.14.into()),
//                     BareVal::Float(4.24.into()),
//                 ];
//
//                 assert_eq!(&global_expected, &global_registers);
//             }
//         }
//
//         mod test_gstore {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 3.14;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![BareVal::Int(0), BareVal::Float(3.14.into())];
//
//                 assert_eq!(&expected, &registers);
//
//                 let global_registers = interpreter
//                     .process
//                     .globals
//                     .iter()
//                     .map(|global| (*global.borrow()).clone())
//                     .collect::<Vec<_>>();
//
//                 let global_expected = vec![
//                     BareVal::Int(0), // "wasted" global r0
//                     BareVal::Float(3.14.into()),
//                 ];
//
//                 assert_eq!(&global_expected, &global_registers);
//             }
//         }
//
//         mod test_gt {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 1200 > 1199;
//                     mixed r = 1199 > 1200;
//                     mixed s = 1200 > 1200;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(1200),
//                     BareVal::Int(1199),
//                     BareVal::Int(1),
//                     BareVal::Int(1199),
//                     BareVal::Int(1200),
//                     BareVal::Int(0),
//                     BareVal::Int(1200),
//                     BareVal::Int(1200),
//                     BareVal::Int(0),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_gte {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 1200 >= 1199;
//                     mixed r = 1199 >= 1200;
//                     mixed s = 1200 >= 1200;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(1200),
//                     BareVal::Int(1199),
//                     BareVal::Int(1),
//                     BareVal::Int(1199),
//                     BareVal::Int(1200),
//                     BareVal::Int(0),
//                     BareVal::Int(1200),
//                     BareVal::Int(1200),
//                     BareVal::Int(1),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_iadd {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 16 + 34;
//                     mixed r = 12 + -4;
//                     mixed s = q + r;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     // the constant expressions are folded at parse time
//                     BareVal::Int(50),
//                     BareVal::Int(8),
//                     BareVal::Int(50),
//                     BareVal::Int(8),
//                     BareVal::Int(58),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_iconst {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 666;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![BareVal::Int(0), BareVal::Int(666)];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_iconst0 {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 0;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![BareVal::Int(0), BareVal::Int(0)];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_iconst1 {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 1;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![BareVal::Int(0), BareVal::Int(1)];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_idiv {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 16 / 2;
//                     mixed r = 12 / -4;
//                     mixed s = q / r;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     // the constant expressions are folded at parse time
//                     BareVal::Int(8),
//                     BareVal::Int(-3),
//                     BareVal::Int(8),
//                     BareVal::Int(-3),
//                     BareVal::Int(-2),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_imod {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 16 % 7;
//                     mixed r = 12 % -7;
//                     mixed s = q % r;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     // the constant expressions are folded at parse time
//                     BareVal::Int(2),
//                     BareVal::Int(5),
//                     BareVal::Int(2),
//                     BareVal::Int(5),
//                     BareVal::Int(2),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//
//             #[test]
//             fn errors_on_division_by_zero() {
//                 let code = indoc! { r##"
//                     mixed q = 5;
//                     mixed r = 0;
//                     mixed s = q / r;
//                 "##};
//
//                 let mut interpreter = AsmInterpreter::default();
//
//                 let program = compile_prog(code);
//
//                 let r = interpreter.init_master(program);
//
//                 assert_eq!(
//                     r.unwrap_err().to_string(),
//                     "Runtime Error: Division by zero"
//                 )
//             }
//         }
//
//         mod test_imul {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 16 * 2;
//                     mixed r = 12 * -4;
//                     mixed s = q * r;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(32),
//                     BareVal::Int(-48),
//                     BareVal::Int(32),
//                     BareVal::Int(-48),
//                     BareVal::Int(-1536),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_isub {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 16 - 2;
//                     mixed r = 12 - -4;
//                     mixed s = q - r;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(14),
//                     BareVal::Int(16),
//                     BareVal::Int(14),
//                     BareVal::Int(16),
//                     BareVal::Int(-2),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_jmp {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     void create() {
//                         mixed j;
//                         int i = 12;
//                         if (i > 10) {
//                             j = 69;
//                         } else {
//                             j = 3;
//                         }
//
//                         // Store a snapshot, so we can test this even though this stack
//                         // frame would otherwise have been popped off into the aether.
//                         debug("in_memory_snapshot");
//                     }
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let stack = interpreter.snapshot.unwrap().stack;
//
//                 // The top of the stack in the snapshot is the object initialization frame,
//                 // which is not what we care about here, so we get the second-to-top frame instead.
//                 let registers = &stack[stack.len() - 2].registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(69),
//                     BareVal::Int(12),
//                     BareVal::Int(10),
//                     BareVal::Int(1),
//                     BareVal::Int(69),
//                     BareVal::Int(0),
//                     BareVal::String("in_memory_snapshot".into()),
//                     BareVal::Int(0),
//                 ];
//
//                 assert_eq!(&expected, registers);
//             }
//         }
//
//         mod test_jnz {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     void create() {
//                         int j;
//                         do {
//                             j += 1;
//                         } while(j < 8);
//
//                         // Store a snapshot, so we can test this even though this stack
//                         // frame would otherwise have been popped off into the aether.
//                         debug("in_memory_snapshot");
//                     }
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let stack = interpreter.snapshot.unwrap().stack;
//
//                 // The top of the stack in the snapshot is the object initialization frame,
//                 // which is not what we care about here, so we get the second-to-top frame instead.
//                 let registers = &stack[stack.len() - 2].registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(8),
//                     BareVal::Int(1),
//                     BareVal::Int(8),
//                     BareVal::Int(8),
//                     BareVal::Int(0),
//                     BareVal::String("in_memory_snapshot".into()),
//                     BareVal::Int(0),
//                 ];
//
//                 assert_eq!(&expected, registers);
//             }
//         }
//
//         mod test_jz {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     int i = 12;
//                     int j = i > 12 ? 10 : 1000;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(12),
//                     BareVal::Int(1000),
//                     BareVal::Int(12),
//                     BareVal::Int(12),
//                     BareVal::Int(0),
//                     BareVal::Int(0),
//                     BareVal::Int(1000),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_load {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     int *i = ({ 1, 2, 3 });
//                     int j = i[1];
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(1),
//                     BareVal::Int(2),
//                     BareVal::Int(3),
//                     BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)].into()),
//                     BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)].into()),
//                     BareVal::Int(1),
//                     BareVal::Int(2),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_lt {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 1200 < 1199;
//                     mixed r = 1199 < 1200;
//                     mixed s = 1200 < 1200;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(1200),
//                     BareVal::Int(1199),
//                     BareVal::Int(0),
//                     BareVal::Int(1199),
//                     BareVal::Int(1200),
//                     BareVal::Int(1),
//                     BareVal::Int(1200),
//                     BareVal::Int(1200),
//                     BareVal::Int(0),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_lte {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = 1200 <= 1199;
//                     mixed r = 1199 <= 1200;
//                     mixed s = 1200 <= 1200;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(1200),
//                     BareVal::Int(1199),
//                     BareVal::Int(0),
//                     BareVal::Int(1199),
//                     BareVal::Int(1200),
//                     BareVal::Int(1),
//                     BareVal::Int(1200),
//                     BareVal::Int(1200),
//                     BareVal::Int(1),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_mapconst {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed q = ([
//                         "asdf": 123,
//                         456: 3.14
//                     ]);
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let mut hashmap = HashMap::new();
//                 hashmap.insert(BareVal::String("asdf".into()), BareVal::Int(123));
//                 hashmap.insert(BareVal::Int(456), BareVal::Float(3.14.into()));
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(456),
//                     BareVal::Float(3.14.into()),
//                     BareVal::String("asdf".into()),
//                     BareVal::Int(123),
//                     BareVal::Mapping(hashmap),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_madd {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed a = "abc";
//                     mixed b = 123;
//                     mixed c = a + b;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::String("abc".into()),
//                     BareVal::Int(123),
//                     BareVal::String("abc".into()),
//                     BareVal::Int(123),
//                     BareVal::String("abc123".into()),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_mmul {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed a = "abc";
//                     mixed b = 4;
//                     mixed c = a * b;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::String("abc".into()),
//                     BareVal::Int(4),
//                     BareVal::String("abc".into()),
//                     BareVal::Int(4),
//                     BareVal::String("abcabcabcabc".into()),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_msub {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed a = ({ 1, 1, 2, 3 });
//                     mixed b = a - ({ 1 });
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(1),
//                     BareVal::Int(1),
//                     BareVal::Int(2),
//                     BareVal::Int(3),
//                     BareVal::Array(
//                         vec![
//                             BareVal::Int(1),
//                             BareVal::Int(1),
//                             BareVal::Int(2),
//                             BareVal::Int(3),
//                         ]
//                         .into(),
//                     ),
//                     BareVal::Array(
//                         vec![
//                             BareVal::Int(1),
//                             BareVal::Int(1),
//                             BareVal::Int(2),
//                             BareVal::Int(3),
//                         ]
//                         .into(),
//                     ),
//                     BareVal::Int(1),
//                     BareVal::Array(vec![BareVal::Int(1)].into()),
//                     BareVal::Array(vec![BareVal::Int(2), BareVal::Int(3)].into()),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_not {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed a = !2;
//                     mixed b = !!4;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(2),
//                     BareVal::Int(0),
//                     BareVal::Int(4),
//                     BareVal::Int(0),
//                     BareVal::Int(1),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_or {
//             use super::*;
//             use crate::interpreter::asm_interpreter::tests::BareVal::Int;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed a = 15 | 27;
//                     mixed b = 0 | a;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![Int(0), Int(31), Int(0), Int(31), Int(31)];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_oror {
//             use super::*;
//             use crate::interpreter::asm_interpreter::tests::BareVal::Int;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed a = 123 || 333;
//                     mixed b = 0;
//                     mixed c = b || a;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     Int(0),
//                     Int(123),
//                     Int(123),
//                     Int(0),
//                     Int(0),
//                     Int(0),
//                     Int(123),
//                     Int(123),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_range {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed a = ({ 1, 2, 3 })[1..];
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(1),
//                     BareVal::Int(2),
//                     BareVal::Int(3),
//                     BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)].into()),
//                     BareVal::Int(1),
//                     BareVal::Int(-1),
//                     BareVal::Array(vec![BareVal::Int(2), BareVal::Int(3)].into()),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_regcopy {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed a = 4;
//                     mixed b = a;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(4),
//                     BareVal::Int(4),
//                     BareVal::Int(4),
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_ret {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     int create() { return 666; }
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![
//                     BareVal::Int(666), // return value from create()
//                     BareVal::Int(666), // The copy of the call return value into its own register
//                 ];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_store {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     void create() {
//                         mixed a = ({ 1, 2, 3 });
//                         a[2] = 678;
//
//                         // Store a snapshot, so we can test this even though this stack
//                         // frame would otherwise have been popped off into the aether.
//                         debug("in_memory_snapshot");
//                     }
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let stack = interpreter.snapshot.unwrap().stack;
//
//                 // The top of the stack in the snapshot is the object initialization frame,
//                 // which is not what we care about here, so we get the second-to-top frame instead.
//                 let registers = &stack[stack.len() - 2].registers;
//
//                 let expected = vec![
//                     BareVal::Int(0),
//                     BareVal::Int(1),
//                     BareVal::Int(2),
//                     BareVal::Int(3),
//                     BareVal::Array(
//                         vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(678)].into(),
//                     ),
//                     BareVal::Int(678),
//                     BareVal::Int(2),
//                     BareVal::String("in_memory_snapshot".into()),
//                     BareVal::Int(0),
//                 ];
//
//                 assert_eq!(&expected, registers);
//             }
//         }
//
//         mod test_sconst {
//             use super::*;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     string foo = "lolwut";
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![BareVal::Int(0), BareVal::String("lolwut".into())];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_xor {
//             use super::*;
//             use crate::interpreter::asm_interpreter::tests::BareVal::Int;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed a = 15 ^ 27;
//                     mixed b = 0 ^ a;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![Int(0), Int(20), Int(0), Int(20), Int(20)];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_shl {
//             use super::*;
//             use crate::interpreter::asm_interpreter::tests::BareVal::Int;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed a = 12345 << 6;
//                     mixed b = 0 << a;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![Int(0), Int(790080), Int(0), Int(790080), Int(0)];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//
//         mod test_shr {
//             use super::*;
//             use crate::interpreter::asm_interpreter::tests::BareVal::Int;
//
//             #[test]
//             fn stores_the_value() {
//                 let code = indoc! { r##"
//                     mixed a = 12345 >> 6;
//                     mixed b = 0 >> a;
//                 "##};
//
//                 let interpreter = run_prog(code);
//                 let registers = interpreter.popped_frame.unwrap().registers;
//
//                 let expected = vec![Int(0), Int(192), Int(0), Int(192), Int(0)];
//
//                 assert_eq!(&expected, &registers);
//             }
//         }
//     }
//
//     mod test_limits {
//         use super::*;
//
//         #[test]
//         fn errors_on_stack_overflow() {
//             let code = indoc! { r##"
//                 int kab00m = marf();
//
//                 int marf() {
//                     return marf();
//                 }
//             "##};
//
//             let config = Config::default().with_max_call_stack_size(Some(10));
//             let mut interpreter = AsmInterpreter::new(config.into());
//             let program = compile_prog(code);
//             let r = interpreter.init_master(program);
//
//             assert_eq!(r.unwrap_err().to_string(), "Runtime Error: Stack overflow");
//         }
//
//         #[test]
//         fn errors_on_too_long_evaluation() {
//             let code = indoc! { r##"
//                 void create() {
//                     while(1) {}
//                 }
//             "##};
//
//             let config = Config::default().with_max_task_instructions(Some(10));
//             let mut interpreter = AsmInterpreter::new(config.into());
//             let program = compile_prog(code);
//             let r = interpreter.init_master(program);
//
//             assert_eq!(
//                 r.unwrap_err().to_string(),
//                 "Runtime Error: Evaluation limit has been reached."
//             );
//         }

    }
}