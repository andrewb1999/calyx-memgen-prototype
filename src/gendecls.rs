use std::{sync::atomic::{AtomicU32, Ordering}, fmt::Display};

use calyx::ir::{Primitive, Id, PortDef, Attributes, Width, Direction};
use clap::ArgEnum;
use strum_macros::Display;
use vast::v05::ast::{Module, Decl, Ty, Attribute, Stmt, Sequential, Expr, Parallel, ParallelProcess, SequentialIfElse};

#[derive(ArgEnum, Clone, Debug, Display, PartialEq)]
#[strum(serialize_all = "snake_case")]
pub enum MemoryType {
    FIFO,
    RamSp,
    RamSdp,
    RamTdp,
    RomSp,
    RomDp,
}

#[derive(Display, ArgEnum, Clone, Debug, PartialEq)]
#[strum(serialize_all = "snake_case")]
pub enum HardwarePrimitive {
    Bram,
    Ultraram,
    Lutram,
    Auto
}

#[derive(ArgEnum, Clone, Debug, PartialEq)]
pub enum ReadEnableType {
    ReadEnable,
    ContentEnable,
    NoEnable,
}

impl Display for ReadEnableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> { 
        match self {
            ReadEnableType::ContentEnable => write!(f, "ce"),
            ReadEnableType::ReadEnable => write!(f, "re"),
            ReadEnableType::NoEnable => write!(f, ""),
        }
    }
}

pub struct Memory {
    pub primitive: HardwarePrimitive,
    pub dims: u32,
    pub mem_type: MemoryType,
    pub read_enable_type: ReadEnableType,
}

#[derive(PartialEq)]
enum PortType {
    R,
    W,
    RW,
    FIFO
}

impl Memory {
    pub fn get_mangled_name(&self) -> String {
        "mem_".to_string() + &self.primitive.to_string() + 
            "_d" + &self.dims.to_string() +
            "_" + &self.mem_type.to_string() +
            "_" + &self.read_enable_type.to_string()
    }

    fn add_address_ports(&self, ports: &mut Vec<PortDef<Width>>, params: &mut Vec<Id>, dims: u32, port_num: u32) {
        let mut prefix = "".to_string();
        if self.mem_type != MemoryType::RamSp && self.mem_type != MemoryType::RomSp {
            prefix = format!("PORT{}_", port_num);
        }

        for i in 0..dims {
            let size_param;
            let idx_size_param;
            if dims == 1 {
                size_param = Id::new(prefix.clone() + "SIZE", None);
                idx_size_param = Id::new(prefix.clone() + "IDX_SIZE", None);
            } else {
                size_param = Id::new(prefix.clone() + &format!("D{}_SIZE", i), None);
                idx_size_param = Id::new(prefix.clone() + &format!("D{}_IDX_SIZE", i), None);
            }
            let idx_size = Width::Param {value: idx_size_param.clone()};
            let addr = PortDef {
                name: Id::new(prefix.clone().to_lowercase() + &format!("addr{}", i), None),
                width: idx_size.clone(),
                direction: Direction::Input,
                attributes: Attributes::default(),
            };
            params.append(&mut vec![size_param, idx_size_param]);
            ports.push(addr);
        }
    }

    fn add_mem_port(&self, ports: &mut Vec<PortDef<Width>>, params: &mut Vec<Id>,
        rw: PortType, width_param: Id) {
        static PORT_COUNTER: AtomicU32 = AtomicU32::new(0);
        let width = Width::Param {value: width_param};
        // let idx_size = Width::Param {value: idx_size_param};
        let width_one = Width::Const {value: 1};
        let port_num = PORT_COUNTER.load(Ordering::Relaxed);
        self.add_address_ports(ports, params, self.dims, port_num);
        let mut prefix = "".to_string();
        if self.mem_type != MemoryType::RamSp && self.mem_type != MemoryType::RomSp {
            prefix = format!("port{}_", port_num);
        }
        if vec![PortType::RW, PortType::W].contains(&rw) {
            let write_en = PortDef {
                name: Id::new(prefix.clone() + "write_en", None),
                width: width_one.clone(),
                direction: Direction::Input,
                attributes: Attributes::default(),
            };
            let write_data = PortDef {
                name: Id::new(prefix.clone() + "write_data", None),
                width: width.clone(),
                direction: Direction::Input,
                attributes: Attributes::default(),
            };
            let write_done = PortDef {
                name: Id::new(prefix.clone() + "write_done", None),
                width: width_one.clone(),
                direction: Direction::Output,
                attributes: Attributes::default(),
            };
            ports.append(&mut vec![write_data, write_en, write_done]);
        }
        if vec![PortType::RW, PortType::R].contains(&rw) {
            if self.read_enable_type == ReadEnableType::ContentEnable {
                let content_enable = PortDef {
                    name: Id::new(prefix.clone() + "content_en", None),
                    width: width_one.clone(),
                    direction: Direction::Input,
                    attributes: Attributes::default(),
                };
                ports.push(content_enable);
            }
            if self.read_enable_type == ReadEnableType::ReadEnable {
                let read_enable = PortDef {
                    name: Id::new(prefix.clone() + "read_en", None),
                    width: width_one.clone(),
                    direction: Direction::Input,
                    attributes: Attributes::default(),
                };
                ports.push(read_enable);
            }
            let read_data = PortDef {
                name: Id::new(prefix.clone() + "read_data", None),
                width: width.clone(),
                direction: Direction::Output,
                attributes: Attributes::default(),
            };
            let done = PortDef {
                name: Id::new(prefix.clone() + "read_done", None),
                width: width_one,
                direction: Direction::Output,
                attributes: Attributes::default(),
            };
            ports.append(&mut vec![read_data, done]);
        }
        PORT_COUNTER.fetch_add(1, Ordering::Relaxed);
    }

    fn create_primitive_signature(&self, params: &mut Vec<Id>, width_param: Id) -> Vec<PortDef<Width>> {
        let width_one = Width::Const {value: 1};
        let mut clk_attr = Attributes::default();
        clk_attr.insert("clk", 1);
        let clk = PortDef {
            name: Id::new("clk", None),
            width: width_one.clone(),
            direction: Direction::Input,
            attributes: clk_attr,
        };
        let mut ports = vec![clk];
        match &self.mem_type {
            MemoryType::RamSp => self.add_mem_port(&mut ports, params, PortType::RW, width_param),
            MemoryType::RamSdp => {
                self.add_mem_port(&mut ports, params, PortType::R, width_param.clone());
                self.add_mem_port(&mut ports, params, PortType::W, width_param);
            },
            MemoryType::RamTdp => {
                self.add_mem_port(&mut ports, params, PortType::RW, width_param.clone());
                self.add_mem_port(&mut ports, params, PortType::RW, width_param);
            },
            _ => todo!(),
        };
        ports
    }

    pub fn create_calyx_primitive(&self) -> Primitive {
        let width_param = Id::new("WIDTH", None);
        let mut params = vec![
            width_param.clone(),
        ];
        let signature = self.create_primitive_signature(&mut params, width_param);
        Primitive {
            name: Id::new(self.get_mangled_name(), None),
            params,
            signature,
            attributes: Attributes::default(),
            is_comb: false,
        }
    }

    fn add_verilog_addr_ports(&self, module: &mut Module, dims: u32, port_num: u32) -> Vec<String> {
        let mut prefix = "".to_string();
        if self.mem_type != MemoryType::RamSp && self.mem_type != MemoryType::RomSp {
            prefix = format!("PORT{}_", port_num);
        }

        let mut addr_names = vec![];

        for i in 0..dims {
            let size_param;
            let idx_size_param;
            if dims == 1 {
                size_param = prefix.clone() + "SIZE";
                idx_size_param = prefix.clone() + "IDX_SIZE";
            } else {
                size_param = prefix.clone() + &format!("D{}_SIZE", i);
                idx_size_param = prefix.clone() + &format!("D{}_IDX_SIZE", i);
            }
            module.add_param_uint(&size_param, 1024);
            module.add_param_uint(&idx_size_param, 10);
            let addr = prefix.clone().to_lowercase() + &format!("addr{}", i);
            module.add_input_parameterized(&addr, &idx_size_param);
            addr_names.push(addr);
        }
        addr_names
    }
    
    fn add_verilog_mem_port(&self, module: &mut Module, port_type: PortType, memory_name: &String) {
        static PORT_COUNTER: AtomicU32 = AtomicU32::new(0);
        let mut prefix = "".to_string();
        let port_num = PORT_COUNTER.load(Ordering::Relaxed);
        if self.mem_type != MemoryType::RamSp && self.mem_type != MemoryType::RomSp {
            prefix = format!("port{}_", port_num);
        }
        let addr_names = self.add_verilog_addr_ports(module, self.dims, port_num);
        if vec![PortType::W, PortType::RW].contains(&port_type) {
            let write_en = &(prefix.clone() + "write_en");
            module.add_input(write_en, 1);
            let write_data = &(prefix.clone() + "write_data");
            module.add_input_parameterized(write_data, "WIDTH");
            let write_done = &(prefix.clone() + "write_done");
            module.add_output_reg(write_done, 1);

            let event = Sequential::new_posedge("clk");
            let addr = Expr::new_ref(addr_names[0].clone());
            let ram = Expr::new_index_expr(&memory_name, addr);
            let seq_assign = Sequential::new_nonblk_assign(ram, Expr::new_ref(write_data));
            let mut always = ParallelProcess::new_always();
            always.set_event(event);

            if self.read_enable_type == ReadEnableType::ContentEnable && port_type == PortType::RW {
                let content_en = &(prefix.clone() + "content_en");
                let content_en_expr = Expr::new_ref(content_en);
                let write_en_expr = Expr::new_ref(write_en);
                let en_expr = Expr::new_logical_and(content_en_expr, write_en_expr);
                let mut seq_if = SequentialIfElse::new(en_expr);
                let write_done_one = Sequential::new_nonblk_assign(Expr::new_ref(write_done), Expr::Int(1));
                let write_done_zero = Sequential::new_nonblk_assign(Expr::new_ref(write_done), Expr::Int(0));
                seq_if.add_seq(seq_assign);
                seq_if.add_seq(write_done_one);
                seq_if.set_else(write_done_zero);
                let seq = Sequential::IfElse(seq_if);
                always.add_seq(seq);
            } else {
                let mut seq_if = SequentialIfElse::new(Expr::new_ref(write_en));
                let write_done_one = Sequential::new_nonblk_assign(Expr::new_ref(write_done), Expr::Int(1));
                let write_done_zero = Sequential::new_nonblk_assign(Expr::new_ref(write_done), Expr::Int(0));
                seq_if.add_seq(seq_assign);
                seq_if.add_seq(write_done_one);
                seq_if.set_else(write_done_zero);
                let seq = Sequential::IfElse(seq_if);
                always.add_seq(seq);
            }
            module.add_stmt(Stmt::new_parallel(always));
        }
        if vec![PortType::R, PortType::RW].contains(&port_type) {
            let read_data = &(prefix.clone() + "read_data");
            module.add_input_parameterized(read_data, "WIDTH");
            let read_done = &(prefix.clone() + "read_done");
            module.add_output_reg(read_done, 1);

            let event = Sequential::new_posedge("clk");
            let addr = Expr::new_ref(addr_names[0].clone());
            let ram = Expr::new_index_expr(&memory_name, addr);
            let seq_assign = Sequential::new_nonblk_assign(Expr::new_ref(read_data), ram);
            let mut always = ParallelProcess::new_always();
            always.set_event(event);

            if self.read_enable_type == ReadEnableType::ContentEnable {
                let content_en = &(prefix.clone() + "content_en");
                module.add_input(content_en, 1);
                let mut seq_if = SequentialIfElse::new(Expr::new_ref(content_en));
                let read_done_one = Sequential::new_nonblk_assign(Expr::new_ref(read_done), Expr::Int(1));
                let read_done_zero = Sequential::new_nonblk_assign(Expr::new_ref(read_done), Expr::Int(0));
                seq_if.add_seq(seq_assign);
                seq_if.add_seq(read_done_one);
                seq_if.set_else(read_done_zero);
                let seq = Sequential::IfElse(seq_if);
                always.add_seq(seq);
            } else if self.read_enable_type == ReadEnableType::ReadEnable {
                let read_en = &(prefix.clone() + "read_en");
                module.add_input(read_en, 1);
                let mut seq_if = SequentialIfElse::new(Expr::new_ref(read_en));
                let read_done_one = Sequential::new_nonblk_assign(Expr::new_ref(read_done), Expr::Int(1));
                let read_done_zero = Sequential::new_nonblk_assign(Expr::new_ref(read_done), Expr::Int(0));
                seq_if.add_seq(seq_assign);
                seq_if.add_seq(read_done_one);
                seq_if.set_else(read_done_zero);
                let seq = Sequential::IfElse(seq_if);
                always.add_seq(seq);
            } else {
                always.add_seq(seq_assign);
                let read_done_one = Sequential::new_nonblk_assign(Expr::new_ref("read_done"), Expr::Int(1));
                always.add_seq(read_done_one);
            }
            module.add_stmt(Stmt::new_parallel(always));
        }
        PORT_COUNTER.fetch_add(1, Ordering::Relaxed);
    }

    fn add_verilog_mem_core(&self, module : &mut Module, memory_name: &String) {
        let arr = Decl::new_array_parameterized(memory_name.as_str(), Ty::new_param("WIDTH"), 
            Ty::new_param("DEPTH"));
        let mut attr = Attribute::default();
        match self.primitive {
            HardwarePrimitive::Bram => attr.add_stmt("ram_style", "block"),
            HardwarePrimitive::Ultraram => attr.add_stmt("ram_style", "ultra"),
            HardwarePrimitive::Lutram => attr.add_stmt("ram_style", "distributed"),
            HardwarePrimitive::Auto => attr.add_stmt("ram_style", "auto"),
        }
        let attr_decl = Decl::new_attribute_decl(attr, arr);
        module.add_decl(attr_decl)
    }

    pub fn create_verilog_module(&self) -> Module {
        let mut module = Module::new(&self.get_mangled_name());
        module.params.push(Decl::new_param_uint("WIDTH", 32));
        let memory_name = "ram".to_string();
        self.add_verilog_mem_core(&mut module, &memory_name);
        module.add_input("clk", 1);
        match &self.mem_type {
            MemoryType::RamSp => self.add_verilog_mem_port(&mut module, PortType::RW, &memory_name),
            MemoryType::RamSdp => {
                self.add_verilog_mem_port(&mut module, PortType::R, &memory_name);
                self.add_verilog_mem_port(&mut module, PortType::W, &memory_name);
            },
            MemoryType::RamTdp => {
                self.add_verilog_mem_port(&mut module, PortType::RW, &memory_name);
                self.add_verilog_mem_port(&mut module, PortType::RW, &memory_name);
            },
            _ => todo!(),
        };
        module
    }
}
