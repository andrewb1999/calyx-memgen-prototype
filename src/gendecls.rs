use std::{sync::atomic::{AtomicU32, Ordering}, fmt::Display};

use calyx::ir::{Primitive, Id, PortDef, Attributes, Width, Direction};
use clap::ArgEnum;
use strum_macros::Display;
use vast::v17::ast::{Module, Decl};

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

    

    pub fn create_sv_module(&self) -> Module {
        let mut module = Module::new(&self.get_mangled_name());
        module.params.push(Decl::new_param_uint("WIDTH", 32));
        module
    }
}
