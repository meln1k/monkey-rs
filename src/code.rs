use std::vec;
use byteorder::{BigEndian, WriteBytesExt};


type Instructions = Vec<u8>;

#[repr(u8)]
#[derive(Debug)]
pub enum Opcode {
    OpConstant = 42
}

struct Definition {
    name: String,
    operand_widths: Vec<usize>
}

fn lookup(op: &Opcode) -> Definition {
    use Opcode::*;
    match op {
        OpConstant => Definition {
            name: "OpConstant".to_owned(),
            operand_widths: vec![2]
        }
    }
}

fn make(op: Opcode, operands: Vec<usize>) -> Result<Vec<u8>, String> {
    let def = lookup(&op);
    let mut instruction_len:usize = 1; // 1 for opcode
    for &width in &def.operand_widths {
        instruction_len += width;
    }
    
    let mut instruction: Vec<u8> = Vec::with_capacity(instruction_len);

    instruction.push(op as u8);

    for (i, &o) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => {
                instruction.write_u16::<BigEndian>(o as u16).unwrap();
            }
            _ => Err(format!("Width of {} is not supported", width))?
        }
    }

    Ok(instruction)
}

#[cfg(test)]
mod test {
    use crate::code::Opcode;
    use super::*;

    #[test]
    fn test_make() {

        struct Test(Opcode, Vec<usize>, Vec<u8>);

        let tests = vec![
            Test(Opcode::OpConstant, vec![65534], vec![Opcode::OpConstant as u8, 255, 254])
        ];

        for Test(op, operands, expected) in tests {
            let instruction = make(op, operands).unwrap();

            assert_eq!(
                instruction.len(), 
                expected.len(), 
                "instruction has wrong length. want={}, got={}", 
                expected.len(), 
                instruction.len()
            );

            for (i, b) in expected.iter().enumerate() {
                assert_eq!(instruction[i], expected[i], "wrong byte at pos {}. want={}, got={}", i, b, instruction[i]);
            }
        }

    }
}