extern crate cranelift;
extern crate cranelift_module;
extern crate cranelift_simplejit;

use std::env::args;
use std::error::Error;
use std::fs::read_to_string;
use std::mem;
use std::process::exit;

mod frontend;
mod jit;

fn run() -> Result<(), Box<dyn Error>> {
    let args = args().collect::<Vec<String>>();
    if args.len() == 2 {
        // Create the JIT instance, which manages all generated functions and data.
        let mut jit = jit::JIT::new();

        let code = read_to_string(&args[1])?;
        let main = jit.compile(&code)?;
        // Cast the raw pointer to a typed function pointer. This is unsafe, because
        // this is the critical point where you have to trust that the generated code
        // is safe to be called.
        let main = unsafe { mem::transmute::<_, fn() -> isize>(main) };
        exit(main() as i32);
    } else {
        eprintln!("Usage: {} [path]", args[0]);
        exit(64);
    }
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        exit(1);
    }
}
