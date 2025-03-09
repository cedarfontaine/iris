open Types
open Register

type operand =
  | Imm of int64
  | Reg of register
  | Mem of memory_ref
  | Label of label
  | RelAddr of int

and memory_ref = {
  segment : segment option;
  base : register option;
  index : register option;
  scale : scale option;
  disp : displacement;
  size : size;
}
[@@deriving show]

let mem ?(segment=None) ?(base=None) ?(index=None) ?(scale=None) ?(disp=NoDisp) size =
  Mem { segment; base; index; scale; disp; size }

let mem_reg ?(disp=NoDisp) size reg =
  Mem { segment = None; base = Some reg; index = None; scale = None; disp; size }

let imm8 i = Imm (Int64.of_int i)
let imm16 i = Imm (Int64.of_int i)
let imm32 i = Imm (Int64.of_int32 i)
let imm64 i = Imm i

let disp8 i = Disp8 i
let disp32 i = Disp32 i

type operand_spec =
  | OpImm
  | OpReg
  | OpMem
  | OpLabel
  | OpRegMem
  | OpAny

type instruction_variant =
  | NoOp
  | Op1 of operand
  | Op2 of operand * operand
  | Op3 of operand * operand * operand
  | Op4 of operand * operand * operand * operand
  | CondJump of condition_code * label

type instruction_type =
  | MOV
  | MOVSX
  | MOVZX
  | LEA
  | PUSH
  | POP
  | AND
  | OR
  | XOR
  | NOT
  | ADD
  | ADC
  | SUB
  | SBB
  | IMUL
  | IDIV
  | DIV
  | INC
  | DEC
  | NEG
  | CMP
  | TEST
  | SHL
  | SHR
  | SAR
  | ROL
  | ROR
  | CALL
  | RET
  | JMP
  | JCC
  | SETZ
  | SETNZ
  | SETS
  | SETNS
  | SETL
  | SETLE
  | SETG
  | SETGE
  | SETB
  | SETBE
  | SETA
  | SETAE
  | NOP
  | INT
  | SYSCALL
[@@deriving show, eq, ord]

type instruction = {
  mnemonic : instruction_type;
  variant : instruction_variant;
  prefixes : prefix list;
}
[@@deriving show]

let mov ?(prefixes=[]) dst src = { 
  mnemonic = MOV; 
  variant = Op2 (dst, src); 
  prefixes 
}

let movsx ?(prefixes=[]) dst src = { 
  mnemonic = MOVSX; 
  variant = Op2 (dst, src); 
  prefixes 
}

let movzx ?(prefixes=[]) dst src = { 
  mnemonic = MOVZX; 
  variant = Op2 (dst, src); 
  prefixes 
}

let lea ?(prefixes=[]) dst src = { 
  mnemonic = LEA; 
  variant = Op2 (dst, src); 
  prefixes 
}

let push ?(prefixes=[]) src = { 
  mnemonic = PUSH; 
  variant = Op1 src; 
  prefixes 
}

let pop ?(prefixes=[]) dst = { 
  mnemonic = POP; 
  variant = Op1 dst; 
  prefixes 
}

let and_ ?(prefixes=[]) dst src = { 
  mnemonic = AND; 
  variant = Op2 (dst, src); 
  prefixes 
}

let or_ ?(prefixes=[]) dst src = { 
  mnemonic = OR; 
  variant = Op2 (dst, src); 
  prefixes 
}

let xor ?(prefixes=[]) dst src = { 
  mnemonic = XOR; 
  variant = Op2 (dst, src); 
  prefixes 
}

let not_ ?(prefixes=[]) dst = { 
  mnemonic = NOT; 
  variant = Op1 dst; 
  prefixes 
}

let add ?(prefixes=[]) dst src = { 
  mnemonic = ADD; 
  variant = Op2 (dst, src); 
  prefixes 
}

let adc ?(prefixes=[]) dst src = { 
  mnemonic = ADC; 
  variant = Op2 (dst, src); 
  prefixes 
}

let sub ?(prefixes=[]) dst src = { 
  mnemonic = SUB; 
  variant = Op2 (dst, src); 
  prefixes 
}

let sbb ?(prefixes=[]) dst src = { 
  mnemonic = SBB; 
  variant = Op2 (dst, src); 
  prefixes 
}

let imul ?(prefixes=[]) dst src = { 
  mnemonic = IMUL; 
  variant = Op2 (dst, src); 
  prefixes 
}

let imul3 ?(prefixes=[]) dst src imm = { 
  mnemonic = IMUL; 
  variant = Op3 (dst, src, imm); 
  prefixes 
}

let idiv ?(prefixes=[]) src = { 
  mnemonic = IDIV; 
  variant = Op1 src; 
  prefixes 
}

let div ?(prefixes=[]) src = { 
  mnemonic = DIV; 
  variant = Op1 src; 
  prefixes 
}

let inc ?(prefixes=[]) dst = { 
  mnemonic = INC; 
  variant = Op1 dst; 
  prefixes 
}

let dec ?(prefixes=[]) dst = { 
  mnemonic = DEC; 
  variant = Op1 dst; 
  prefixes 
}

let neg ?(prefixes=[]) dst = { 
  mnemonic = NEG; 
  variant = Op1 dst; 
  prefixes 
}

let cmp ?(prefixes=[]) left right = { 
  mnemonic = CMP; 
  variant = Op2 (left, right); 
  prefixes 
}

let test ?(prefixes=[]) left right = { 
  mnemonic = TEST; 
  variant = Op2 (left, right); 
  prefixes 
}

let shl ?(prefixes=[]) dst count = { 
  mnemonic = SHL; 
  variant = Op2 (dst, count); 
  prefixes 
}

let shr ?(prefixes=[]) dst count = { 
  mnemonic = SHR; 
  variant = Op2 (dst, count); 
  prefixes 
}

let sar ?(prefixes=[]) dst count = { 
  mnemonic = SAR; 
  variant = Op2 (dst, count); 
  prefixes 
}

let rol ?(prefixes=[]) dst count = { 
  mnemonic = ROL; 
  variant = Op2 (dst, count); 
  prefixes 
}

let ror ?(prefixes=[]) dst count = { 
  mnemonic = ROR; 
  variant = Op2 (dst, count); 
  prefixes 
}

let call ?(prefixes=[]) target = { 
  mnemonic = CALL; 
  variant = Op1 target; 
  prefixes 
}

let ret ?(prefixes=[]) = { 
  mnemonic = RET; 
  variant = NoOp; 
  prefixes 
}

let jmp ?(prefixes=[]) target = { 
  mnemonic = JMP; 
  variant = Op1 target; 
  prefixes 
}

let jcc ?(prefixes=[]) condition target = { 
  mnemonic = JCC; 
  variant = CondJump (condition, target); 
  prefixes 
}

let je = jcc E
let jne = jcc NE
let jl = jcc L
let jle = jcc LE
let jg = jcc NLE
let jge = jcc NL
let jb = jcc B
let jbe = jcc BE
let ja = jcc NBE
let jae = jcc NB
let js = jcc S
let jns = jcc NS
let jo = jcc O
let jno = jcc NO

let setz ?(prefixes=[]) dst = { 
  mnemonic = SETZ; 
  variant = Op1 dst; 
  prefixes 
}

let setnz ?(prefixes=[]) dst = { 
  mnemonic = SETNZ; 
  variant = Op1 dst; 
  prefixes 
}

let sets ?(prefixes=[]) dst = { 
  mnemonic = SETS; 
  variant = Op1 dst; 
  prefixes 
}

let setns ?(prefixes=[]) dst = { 
  mnemonic = SETNS; 
  variant = Op1 dst; 
  prefixes 
}

let setl ?(prefixes=[]) dst = { 
  mnemonic = SETL; 
  variant = Op1 dst; 
  prefixes 
}

let setle ?(prefixes=[]) dst = { 
  mnemonic = SETLE; 
  variant = Op1 dst; 
  prefixes 
}

let setg ?(prefixes=[]) dst = { 
  mnemonic = SETG; 
  variant = Op1 dst; 
  prefixes 
}

let setge ?(prefixes=[]) dst = { 
  mnemonic = SETGE; 
  variant = Op1 dst; 
  prefixes 
}

let setb ?(prefixes=[]) dst = { 
  mnemonic = SETB; 
  variant = Op1 dst; 
  prefixes 
}

let setbe ?(prefixes=[]) dst = { 
  mnemonic = SETBE; 
  variant = Op1 dst; 
  prefixes 
}

let seta ?(prefixes=[]) dst = { 
  mnemonic = SETA; 
  variant = Op1 dst; 
  prefixes 
}

let setae ?(prefixes=[]) dst = { 
  mnemonic = SETAE; 
  variant = Op1 dst; 
  prefixes 
}

let nop ?(prefixes=[]) = { 
  mnemonic = NOP; 
  variant = NoOp; 
  prefixes 
}

let int ?(prefixes=[]) interrupt = { 
  mnemonic = INT; 
  variant = Op1 interrupt; 
  prefixes 
}

let syscall ?(prefixes=[]) = { 
  mnemonic = SYSCALL; 
  variant = NoOp; 
  prefixes 
}

let operand_size = function
  | Imm i -> 
      if Int64.compare i (Int64.of_int32 Int32.max_int) > 0 || 
         Int64.compare i (Int64.of_int32 Int32.min_int) < 0 then Qword
      else if Int64.compare i (Int64.of_int 32767) > 0 || 
              Int64.compare i (Int64.of_int (-32768)) < 0 then Dword
      else if Int64.compare i (Int64.of_int 127) > 0 || 
              Int64.compare i (Int64.of_int (-128)) < 0 then Word
      else Byte
  | Reg r -> register_size r
  | Mem m -> m.size
  | Label _ -> Qword  
  | RelAddr _ -> Dword