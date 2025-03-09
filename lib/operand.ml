open Types
open Register
open Instruction

let is_reg = function
  | Reg _ -> true
  | _ -> false

let is_mem = function
  | Mem _ -> true
  | _ -> false

let is_imm = function
  | Imm _ -> true
  | _ -> false

let is_label = function
  | Label _ -> true
  | _ -> false

let is_rel_addr = function
  | RelAddr _ -> true
  | _ -> false

let get_reg = function
  | Reg r -> r
  | _ -> failwith "Not a register operand"

let get_mem = function
  | Mem m -> m
  | _ -> failwith "Not a memory operand"

let get_imm = function
  | Imm i -> i
  | _ -> failwith "Not an immediate operand"

let get_label = function
  | Label l -> l
  | _ -> failwith "Not a label operand"

let get_rel_addr = function
  | RelAddr a -> a
  | _ -> failwith "Not a relative address operand"

let reg_to_operand r = Reg r

let imm_to_operand i = Imm i

let label_to_operand l = Label l

let qword_ptr ?(segment=None) ?(index=None) ?(scale=None) ?(disp=NoDisp) reg =
  Mem { segment; base = Some reg; index; scale; disp; size = Qword }

let dword_ptr ?(segment=None) ?(index=None) ?(scale=None) ?(disp=NoDisp) reg =
  Mem { segment; base = Some reg; index; scale; disp; size = Dword }

let word_ptr ?(segment=None) ?(index=None) ?(scale=None) ?(disp=NoDisp) reg =
  Mem { segment; base = Some reg; index; scale; disp; size = Word }

let byte_ptr ?(segment=None) ?(index=None) ?(scale=None) ?(disp=NoDisp) reg =
  Mem { segment; base = Some reg; index; scale; disp; size = Byte }

let mem_index ?(segment=None) ?(disp=NoDisp) size base index scale =
  Mem { segment; base = Some base; index = Some index; scale = Some scale; disp; size }

let mem_disp ?(segment=None) size disp =
  Mem { segment; base = None; index = None; scale = None; disp; size }

let mem_rip size disp =
  match disp with
  | Disp32 d -> RelAddr (Int32.to_int d)
  | Disp8 d -> RelAddr (d)
  | NoDisp -> failwith "RIP-relative addressing requires a displacement"

let is_compatible_size op1 op2 =
  let size1 = operand_size op1 in
  let size2 = operand_size op2 in
  size1 = size2

let operand_to_string = function
  | Reg r -> show_register r
  | Imm i -> Printf.sprintf "0x%Lx" i
  | Mem m -> 
      let seg_str = match m.segment with
        | Some s -> show_segment s ^ ":"
        | None -> ""
      in
      let base_str = match m.base with
        | Some b -> show_register b
        | None -> ""
      in
      let idx_scale_str = match m.index, m.scale with
        | Some i, Some s -> 
            let scale_str = match s with
              | One -> "1"
              | Two -> "2"
              | Four -> "4"
              | Eight -> "8"
            in
            "+" ^ show_register i ^ "*" ^ scale_str
        | _ -> ""
      in
      let disp_str = match m.disp with
        | NoDisp -> ""
        | Disp8 d -> if d >= 0 then "+" ^ string_of_int d else string_of_int d
        | Disp32 d -> 
            if Int32.compare d Int32.zero >= 0 
            then "+" ^ Int32.to_string d 
            else Int32.to_string d
      in
      let size_str = match m.size with
        | Byte -> "BYTE PTR "
        | Word -> "WORD PTR "
        | Dword -> "DWORD PTR "
        | Qword -> "QWORD PTR "
      in
      Printf.sprintf "%s%s[%s%s%s]" size_str seg_str base_str idx_scale_str disp_str
  | Label l -> l
  | RelAddr a -> Printf.sprintf "[rip+%d]" a