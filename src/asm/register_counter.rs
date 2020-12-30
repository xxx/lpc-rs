use lazy_static::lazy_static;
use crate::asm::register::Register;
use std::sync::Mutex;

pub struct RegisterCounter;

lazy_static! {
    static ref REGISTER_COUNTER: Mutex<u128> = Mutex::new(0);
}

impl RegisterCounter {
    pub fn reset() {
        let mut counter = REGISTER_COUNTER.lock().unwrap();
        *counter = 0;
    }

    pub fn next() -> Register {
        let mut counter = REGISTER_COUNTER.lock().unwrap();
        *counter += 1;
        Register(format!("r{}", *counter))
    }
}