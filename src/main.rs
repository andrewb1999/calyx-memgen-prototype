mod gendecls;

use clap::Parser;
use gendecls::{HardwarePrimitive, MemoryType, Memory, ReadEnableType};
use calyx::ir::Printer;
use std::io;


#[derive(Parser, Debug)]
pub struct Args {
    #[clap(short='i', long="impl", arg_enum, default_value_t = HardwarePrimitive::Auto)]
    primitive: HardwarePrimitive,
    #[clap(short, long, value_parser, default_value_t = 1)]
    dims: u32,
    #[clap(short, long, arg_enum, default_value_t = MemoryType::RamSp)]
    memory_type: MemoryType,
    #[clap(short, long, arg_enum, default_value_t = ReadEnableType::ContentEnable)]
    read_enable_type: ReadEnableType,
}

fn construct_memory(args: Args) -> Memory {
    Memory {primitive: args.primitive, 
            dims: args.dims,
            mem_type: args.memory_type,
            read_enable_type: args.read_enable_type}
}

fn main() -> io::Result<()> {
    let args = Args::parse();
    let memory = construct_memory(args);
    let prim = memory.create_calyx_primitive();
    let module = memory.create_verilog_module();
    Printer::write_primitive(&prim, 0, &mut io::stdout())?;
    println!();
    println!("{}", module);
    Ok(())
}
