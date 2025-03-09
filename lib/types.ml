type size = 
  | Byte
  | Word
  | Dword
  | Qword

let size_to_bits = function
  | Byte -> 8
  | Word -> 16
  | Dword -> 32
  | Qword -> 64

let size_to_bytes = function
  | Byte -> 1
  | Word -> 2
  | Dword -> 4
  | Qword -> 8

type segment = 
  | CS
  | DS
  | ES
  | FS
  | GS
  | SS
[@@deriving show, eq, ord]

type condition_code =
  | O    
  | NO  
  | B    
  | NB  
  | E   
  | NE   
  | BE   
  | NBE  
  | S   
  | NS   
  | P   
  | NP   
  | L    
  | NL   
  | LE   
  | NLE 
[@@deriving show, eq, ord]

let condition_code_to_opcode = function
  | O -> 0x0
  | NO -> 0x1
  | B -> 0x2
  | NB -> 0x3
  | E -> 0x4
  | NE -> 0x5
  | BE -> 0x6
  | NBE -> 0x7
  | S -> 0x8
  | NS -> 0x9
  | P -> 0xA
  | NP -> 0xB
  | L -> 0xC
  | NL -> 0xD
  | LE -> 0xE
  | NLE -> 0xF

type scale = One | Two | Four | Eight

let scale_to_bits = function
  | One -> 0b00
  | Two -> 0b01
  | Four -> 0b10
  | Eight -> 0b11

type index_reg = 
  | Reg_idx of int
  | No_idx
[@@deriving show, eq, ord]

type sib_byte = {
  scale : scale;
  index : index_reg;
  base : int;
}

type rex_byte = {
  w : bool;  
  r : bool;  
  x : bool;  
  b : bool;  
}

type modrm_byte = {
  mode : int;
  reg : int;  
  rm : int;   
}

type addr_mode =
  | Direct 
  | Indirect of int option * index_reg option * scale option  
  | RIP_relative of int  
  | Absolute of int64   
[@@deriving show]

type displacement = 
  | NoDisp
  | Disp8 of int
  | Disp32 of int32
[@@deriving show, eq, ord]

type prefix = 
  | Lock
  | Rep
  | Repne
  | Segment of segment
  | OperandSize
  | AddressSize
[@@deriving show, eq, ord]

type label = string
[@@deriving show, eq, ord]

type relocation = {
  offset: int;
  name: string;
  addend: int;
}

type section_type = 
  | Text
  | Data
  | Bss
  | Rodata
[@@deriving show, eq, ord]