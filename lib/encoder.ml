open Types
open Register
open Instruction
open Operand

type encoded_instruction = {
  prefixes: int list;
  rex: int option;
  opcode: int list;
  modrm: int option;
  sib: int option;
  displacement: int list;
  immediate: int list;
  rel_addr: int option;
}

let empty_encoded_instr = {
  prefixes = [];
  rex = None;
  opcode = [];
  modrm = None;
  sib = None;
  displacement = [];
  immediate = [];
  rel_addr = None;
}

let combine_rex w r x b =
  let bits = [|b; x; r; w|] in
  let rex_value = 0x40 in
  let rec build_rex i acc =
    if i >= 4 then acc
    else build_rex (i + 1) (acc lor ((if bits.(i) then 1 else 0) lsl i))
  in
  build_rex 0 rex_value

let needs_rex_w = function
  | GPR r -> begin match r with
    | RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI
    | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15 -> true
    | _ -> false
    end
  | _ -> false

let encode_modrm mode reg rm =
  ((mode land 0x3) lsl 6) lor ((reg land 0x7) lsl 3) lor (rm land 0x7)

let encode_sib scale index base =
  ((scale land 0x3) lsl 6) lor ((index land 0x7) lsl 3) lor (base land 0x7)

let encode_displacement = function
  | NoDisp -> []
  | Disp8 d -> [d land 0xff]
  | Disp32 d -> 
      let d_int = Int32.to_int d in
      [d_int land 0xff; (d_int lsr 8) land 0xff; (d_int lsr 16) land 0xff; (d_int lsr 24) land 0xff]

let encode_immediate size imm =
  match size with
  | Byte -> [Int64.to_int imm land 0xff]
  | Word -> 
      let i = Int64.to_int imm in
      [i land 0xff; (i lsr 8) land 0xff]
  | Dword | Qword -> 
      (* For 64-bit mode, most immediates are still 32-bit sign-extended *)
      let i = Int64.to_int imm in
      [i land 0xff; (i lsr 8) land 0xff; (i lsr 16) land 0xff; (i lsr 24) land 0xff]

let encode_rel_addr addr size =
  match size with
  | Byte -> [addr land 0xff]
  | Dword -> 
      [addr land 0xff; (addr lsr 8) land 0xff; (addr lsr 16) land 0xff; (addr lsr 24) land 0xff]
  | _ -> failwith "Invalid size for relative address"

let encode_register_operand op =
  match op with
  | Reg r -> 
      let code = get_reg_code r in
      let rex_needed = needs_rex r in
      let rex_r = (code land 0x8) <> 0 in
      let reg_code = code land 0x7 in
      let rex_w = needs_rex_w r in
      (rex_needed, rex_w, rex_r, false, reg_code)
  | _ -> failwith "Expected register operand"

let encode_rm_operand op =
  match op with
  | Reg r -> 
      let code = get_reg_code r in
      let rex_needed = needs_rex r in
      let rex_b = (code land 0x8) <> 0 in
      let rm_code = code land 0x7 in
      (rex_needed, rex_b, false, None, encode_modrm 0b11 0 rm_code, None, [])
  | Mem m -> 
      let base_reg_code, rex_b = match m.base with
        | Some r -> 
            let code = get_reg_code r in
            let rex_b = (code land 0x8) <> 0 in
            (code land 0x7, rex_b)
        | None -> (0, false)
      in

      let index_reg_code, rex_x = match m.index with
        | Some r -> 
            let code = get_reg_code r in
            let rex_x = (code land 0x8) <> 0 in
            (code land 0x7, rex_x)
        | None -> (4, false) (* 4 is a special value meaning "no index register" *)
      in

      let scale_bits = match m.scale with
        | Some s -> scale_to_bits s
        | None -> 0
      in

      let (mod_bits, sib, displacement) = match m.disp with
        | NoDisp -> 
            if m.base = None || 
               (match m.base with Some (GPR RBP) | Some (GPR R13) -> true | _ -> false) 
            then (0b01, true, [0]) (* Force disp8=0 for RBP/R13 *)
            else (0b00, m.index <> None || (match m.base with Some (GPR RSP) | Some (GPR R12) -> true | _ -> false), [])
        | Disp8 _ -> (0b01, m.index <> None || (match m.base with Some (GPR RSP) | Some (GPR R12) -> true | _ -> false), encode_displacement m.disp)
        | Disp32 _ -> (0b10, m.index <> None || (match m.base with Some (GPR RSP) | Some (GPR R12) -> true | _ -> false), encode_displacement m.disp)
      in

      let (modrm, sib_byte) = 
        if sib then
          (encode_modrm mod_bits 0 0b100, Some (encode_sib scale_bits index_reg_code base_reg_code))
        else
          (encode_modrm mod_bits 0 base_reg_code, None)
      in

      let rex_needed = rex_b || rex_x || 
                      (match m.base with Some r -> needs_rex r | None -> false) ||
                      (match m.index with Some r -> needs_rex r | None -> false) in

      (rex_needed, rex_b, rex_x, m.segment, modrm, sib_byte, displacement)
  | _ -> failwith "Expected register or memory operand for rm encoding"

let encode_reg_rm reg rm =
  let (rex_needed_reg, rex_w, rex_r, _, reg_code) = encode_register_operand reg in
  let (rex_needed_rm, rex_b, rex_x, segment, modrm, sib, displacement) = encode_rm_operand rm in
  
  let rex_needed = rex_needed_reg || rex_needed_rm in
  let rex = if rex_needed || rex_w then Some (combine_rex rex_w rex_r rex_x rex_b) else None in
  
  let modrm_byte = modrm lor (reg_code lsl 3) in
  
  let prefix = match segment with
    | Some CS -> [0x2E]
    | Some DS -> [] (* Default segment, no prefix needed *)
    | Some ES -> [0x26]
    | Some FS -> [0x64]
    | Some GS -> [0x65]
    | Some SS -> [0x36]
    | None -> []
  in
  
  (prefix, rex, modrm_byte, sib, displacement)

let encode_mov instr =
  match instr.variant with
  | Op2 (dst, src) -> 
      let encoded = empty_encoded_instr in
      
      match dst, src with
      | Reg reg_dst, Reg reg_src ->
          let (rex_needed_dst, rex_w_dst, _, _, _) = encode_register_operand dst in
          let (rex_needed_src, _, rex_r, _, reg_code) = encode_register_operand src in
          
          let rex_needed = rex_needed_dst || rex_needed_src in
          let rex = if rex_needed || rex_w_dst then 
                     Some (combine_rex rex_w_dst rex_r false (get_rex_rb reg_dst))
                   else 
                     None in
          
          let size_dst = register_size reg_dst in
          let opcode = match size_dst with
                       | Byte -> [0x88]
                       | _ -> [0x89]
          in
          
          let modrm = encode_modrm 0b11 reg_code (get_base_reg reg_dst) in
          
          { encoded with 
            rex; 
            opcode; 
            modrm = Some modrm 
          }
      
      | Reg reg_dst, Mem mem_src ->
          let size_dst = register_size reg_dst in
          let opcode = match size_dst with
                       | Byte -> [0x8A]
                       | _ -> [0x8B]
          in
          
          let (prefix, rex, modrm, sib, disp) = encode_reg_rm dst src in
          
          { encoded with 
            prefixes = prefix;
            rex;
            opcode;
            modrm = Some modrm;
            sib;
            displacement = disp
          }
      
      | Mem mem_dst, Reg reg_src ->
          let size_src = register_size reg_src in
          let opcode = match size_src with
                       | Byte -> [0x88]
                       | _ -> [0x89]
          in
          
          let (prefix, rex, modrm, sib, disp) = encode_reg_rm src dst in
          
          { encoded with 
            prefixes = prefix;
            rex;
            opcode;
            modrm = Some modrm;
            sib;
            displacement = disp
          }
      
      | Reg reg_dst, Imm imm ->
          let size_dst = register_size reg_dst in
          let opcode_base = match size_dst with
                            | Byte -> 0xB0
                            | _ -> 0xB8
          in
          
          let reg_code = get_reg_code reg_dst in
          let opcode = [opcode_base + (reg_code land 0x7)] in
          
          let rex = if needs_rex reg_dst || (size_dst = Qword) then
                     Some (combine_rex (size_dst = Qword) false false (reg_code land 0x8 <> 0))
                   else
                     None
          in
          
          let imm_bytes = encode_immediate size_dst imm in
          
          { encoded with 
            rex;
            opcode;
            immediate = imm_bytes
          }
      
      | Mem mem_dst, Imm imm ->
          let size_dst = mem_dst.size in
          let opcode = match size_dst with
                       | Byte -> [0xC6]
                       | _ -> [0xC7]
          in
          
          let (rex_needed, rex_b, rex_x, segment, modrm, sib, disp) = encode_rm_operand dst in
          
          let rex = if rex_needed || (size_dst = Qword) then
                     Some (combine_rex (size_dst = Qword) false rex_x rex_b)
                   else
                     None
          in
          
          let prefix = match segment with
                       | Some CS -> [0x2E]
                       | Some DS -> [] (* Default segment, no prefix needed *)
                       | Some ES -> [0x26]
                       | Some FS -> [0x64]
                       | Some GS -> [0x65]
                       | Some SS -> [0x36]
                       | None -> []
          in
          
          let imm_bytes = encode_immediate size_dst imm in
          
          { encoded with 
            prefixes = prefix;
            rex;
            opcode;
            modrm = Some modrm;
            sib;
            displacement = disp;
            immediate = imm_bytes
          }
      
      | _ -> failwith "Unsupported operand combination for MOV"
  | _ -> failwith "MOV requires two operands"

let encode_lea instr =
  match instr.variant with
  | Op2 (dst, src) ->
      let encoded = empty_encoded_instr in
      
      match dst, src with
      | Reg reg_dst, Mem mem_src ->
          let (prefix, rex, modrm, sib, disp) = encode_reg_rm dst src in
          
          { encoded with 
            prefixes = prefix;
            rex;
            opcode = [0x8D];
            modrm = Some modrm;
            sib;
            displacement = disp
          }
      | _ -> failwith "LEA requires register destination and memory source"
  | _ -> failwith "LEA requires two operands"

let encode_push instr =
  match instr.variant with
  | Op1 src ->
      let encoded = empty_encoded_instr in
      
      match src with
      | Reg reg ->
          let reg_code = get_reg_code reg in
          let rex_needed = needs_rex reg in
          let rex = if rex_needed then
                     Some (combine_rex false false false (reg_code land 0x8 <> 0))
                   else
                     None
          in
          
          let opcode = [0x50 + (reg_code land 0x7)] in
          
          { encoded with rex; opcode }
      
      | Mem mem ->
          let (rex_needed, rex_b, rex_x, segment, modrm, sib, disp) = encode_rm_operand src in
          
          let rex = if rex_needed then
                     Some (combine_rex false false rex_x rex_b)
                   else
                     None
          in
          
          let prefix = match segment with
                       | Some CS -> [0x2E]
                       | Some DS -> [] (* Default segment, no prefix needed *)
                       | Some ES -> [0x26]
                       | Some FS -> [0x64]
                       | Some GS -> [0x65]
                       | Some SS -> [0x36]
                       | None -> []
          in
          
          { encoded with 
            prefixes = prefix;
            rex;
            opcode = [0xFF];
            modrm = Some (modrm lor (6 lsl 3)); (* /6 for PUSH *)
            sib;
            displacement = disp
          }
      
      | Imm imm ->
          let imm_val = Int64.to_int imm in
          
          if imm_val >= -128 && imm_val <= 127 then
            { encoded with
              opcode = [0x6A];
              immediate = [imm_val land 0xFF]
            }
          else
            { encoded with
              opcode = [0x68];
              immediate = [imm_val land 0xFF; 
                          (imm_val lsr 8) land 0xFF; 
                          (imm_val lsr 16) land 0xFF; 
                          (imm_val lsr 24) land 0xFF]
            }
      
      | _ -> failwith "Unsupported operand for PUSH"
  | _ -> failwith "PUSH requires one operand"

let encode_binary_reg_rm op opcode_reg_rm opcode_imm8_rm opcode_imm_rm opcode_imm_rax instr =
  match instr.variant with
  | Op2 (dst, src) ->
      let encoded = empty_encoded_instr in
      
      match dst, src with
      | Reg reg_dst, Reg reg_src ->
          let (prefix, rex, modrm, sib, disp) = encode_reg_rm src dst in
          
          { encoded with 
            prefixes = prefix;
            rex;
            opcode = opcode_reg_rm;
            modrm = Some modrm;
            sib;
            displacement = disp
          }
      
      | Mem mem_dst, Reg reg_src ->
          let (prefix, rex, modrm, sib, disp) = encode_reg_rm src dst in
          
          { encoded with 
            prefixes = prefix;
            rex;
            opcode = opcode_reg_rm;
            modrm = Some modrm;
            sib;
            displacement = disp
          }
      
      | Reg reg_dst, Mem mem_src ->
          let (prefix, rex, modrm, sib, disp) = encode_reg_rm dst src in
          
          { encoded with 
            prefixes = prefix;
            rex;
            opcode = [opcode_reg_rm.[0] lor 0x02]; (* Toggle direction bit *)
            modrm = Some modrm;
            sib;
            displacement = disp
          }
      
      | Reg reg_dst, Imm imm ->
          let size_dst = register_size reg_dst in
          let imm_size = operand_size src in
          let imm_val = Int64.to_int imm in
          
          if size_dst = Byte || (imm_val >= -128 && imm_val <= 127) then
            (* Use the short form with 8-bit immediate *)
            let opcode = opcode_imm8_rm in
            let reg_op = op lsl 3 in (* Encode operation in reg field of ModR/M *)
            
            let reg_code = get_reg_code reg_dst in
            let rex_needed = needs_rex reg_dst in
            let rex_w = needs_rex_w reg_dst in
            let rex = if rex_needed || rex_w then
                       Some (combine_rex rex_w false false (reg_code land 0x8 <> 0))
                     else
                       None
            in
            
            let modrm = encode_modrm 0b11 reg_op (reg_code land 0x7) in
            
            { encoded with 
              rex;
              opcode;
              modrm = Some modrm;
              immediate = [imm_val land 0xFF]
            }
          else
            (* Use regular form with full immediate *)
            if get_reg_code reg_dst = 0 && (size_dst = Dword || size_dst = Qword) then
              (* Special shorter encoding for RAX/EAX *)
              { encoded with
                rex = if size_dst = Qword then Some (combine_rex true false false false) else None;
                opcode = opcode_imm_rax;
                immediate = encode_immediate size_dst imm
              }
            else
              (* Regular encoding for other registers *)
              let opcode = opcode_imm_rm in
              let reg_op = op lsl 3 in
              
              let reg_code = get_reg_code reg_dst in
              let rex_needed = needs_rex reg_dst in
              let rex_w = needs_rex_w reg_dst in
              let rex = if rex_needed || rex_w then
                         Some (combine_rex rex_w false false (reg_code land 0x8 <> 0))
                       else
                         None
              in
              
              let modrm = encode_modrm 0b11 reg_op (reg_code land 0x7) in
              
              { encoded with 
                rex;
                opcode;
                modrm = Some modrm;
                immediate = encode_immediate (if size_dst = Qword then Dword else size_dst) imm
              }
      
      | Mem mem_dst, Imm imm ->
          let size_dst = mem_dst.size in
          let imm_size = operand_size src in
          let imm_val = Int64.to_int imm in
          
          let (rex_needed, rex_b, rex_x, segment, modrm_base, sib, disp) = encode_rm_operand dst in
          
          let rex_w = size_dst = Qword in
          let rex = if rex_needed || rex_w then
                     Some (combine_rex rex_w false rex_x rex_b)
                   else
                     None
          in
          
          let prefix = match segment with
                       | Some CS -> [0x2E]
                       | Some DS -> [] (* Default segment, no prefix needed *)
                       | Some ES -> [0x26]
                       | Some FS -> [0x64]
                       | Some GS -> [0x65]
                       | Some SS -> [0x36]
                       | None -> []
          in
          
          if size_dst = Byte || (imm_val >= -128 && imm_val <= 127) then
            (* Use the short form with 8-bit immediate *)
            let opcode = opcode_imm8_rm in
            let reg_op = op lsl 3 in
            let modrm = modrm_base lor (reg_op lsl 3) in
            
            { encoded with 
              prefixes = prefix;
              rex;
              opcode;
              modrm = Some modrm;
              sib;
              displacement = disp;
              immediate = [imm_val land 0xFF]
            }
          else
            (* Use regular form with full immediate *)
            let opcode = opcode_imm_rm in
            let reg_op = op lsl 3 in
            let modrm = modrm_base lor (reg_op lsl 3) in
            
            { encoded with 
              prefixes = prefix;
              rex;
              opcode;
              modrm = Some modrm;
              sib;
              displacement = disp;
              immediate = encode_immediate (if size_dst = Qword then Dword else size_dst) imm
            }
      
      | _ -> failwith (Printf.sprintf "Unsupported operand combination for %s" (show_instruction_type op))
  | _ -> failwith (Printf.sprintf "%s requires two operands" (show_instruction_type op))

let encode_add = encode_binary_reg_rm 0 [0x01] [0x83] [0x81] [0x05]
let encode_adc = encode_binary_reg_rm 2 [0x11] [0x83] [0x81] [0x15]
let encode_sub = encode_binary_reg_rm 5 [0x29] [0x83] [0x81] [0x2D]
let encode_sbb = encode_binary_reg_rm 3 [0x19] [0x83] [0x81] [0x1D]
let encode_and = encode_binary_reg_rm 4 [0x21] [0x83] [0x81] [0x25]
let encode_or  = encode_binary_reg_rm 1 [0x09] [0x83] [0x81] [0x0D]
let encode_xor = encode_binary_reg_rm 6 [0x31] [0x83] [0x81] [0x35]
let encode_cmp = encode_binary_reg_rm 7 [0x39] [0x83] [0x81] [0x3D]

let encode_unary_rm op opcode instr =
  match instr.variant with
  | Op1 dst ->
      let encoded = empty_encoded_instr in
      
      match dst with
      | Reg reg_dst ->
          let size_dst = register_size reg_dst in
          let opcode_byte = match size_dst with
                            | Byte -> opcode
                            | _ -> opcode lor 1
          in
          
          let reg_code = get_reg_code reg_dst in
          let rex_needed = needs_rex reg_dst in
          let rex_w = needs_rex_w reg_dst in
          let rex = if rex_needed || rex_w then
                     Some (combine_rex rex_w false false (reg_code land 0x8 <> 0))
                   else
                     None
          in
          
          let modrm = encode_modrm 0b11 op (reg_code land 0x7) in
          
          { encoded with 
            rex;
            opcode = [opcode_byte];
            modrm = Some modrm
          }
      
      | Mem mem_dst ->
          let size_dst = mem_dst.size in
          let opcode_byte = match size_dst with
                            | Byte -> opcode
                            | _ -> opcode lor 1
          in
          
          let (rex_needed, rex_b, rex_x, segment, modrm_base, sib, disp) = encode_rm_operand dst in
          
          let rex_w = size_dst = Qword in
          let rex = if rex_needed || rex_w then
                     Some (combine_rex rex_w false rex_x rex_b)
                   else
                     None
          in
          
          let prefix = match segment with
                       | Some CS -> [0x2E]
                       | Some DS -> [] (* Default segment, no prefix needed *)
                       | Some ES -> [0x26]
                       | Some FS -> [0x64]
                       | Some GS -> [0x65]
                       | Some SS -> [0x36]
                       | None -> []
          in
          
          let reg_op = op lsl 3 in
          let modrm = modrm_base lor reg_op in
          
          { encoded with 
            prefixes = prefix;
            rex;
            opcode = [opcode_byte];
            modrm = Some modrm;
            sib;
            displacement = disp
          }
      
      | _ -> failwith (Printf.sprintf "Unsupported operand for %s" (show_instruction_type instr.mnemonic))
  | _ -> failwith (Printf.sprintf "%s requires one operand" (show_instruction_type instr.mnemonic))

let encode_not = encode_unary_rm 2 0xF6
let encode_neg = encode_unary_rm 3 0xF6
let encode_inc = encode_unary_rm 0 0xFE
let encode_dec = encode_unary_rm 1 0xFE

let encode_jmp instr =
  match instr.variant with
  | Op1 target ->
      let encoded = empty_encoded_instr in
      
      match target with
      | Label lbl ->
          { encoded with
            opcode = [0xE9];
            rel_addr = Some 0 (* Will be resolved later *)
          }
      
      | RelAddr offset ->
          if offset >= -128 && offset <= 127 then
            { encoded with
              opcode = [0xEB];
              immediate = [offset land 0xFF]
            }
          else
            { encoded with
              opcode = [0xE9];
              immediate = encode_rel_addr offset Dword
            }
      
      | Reg reg ->
          let reg_code = get_reg_code reg in
          let rex_needed = needs_rex reg in
          let rex = if rex_needed then
                     Some (combine_rex false false false (reg_code land 0x8 <> 0))
                   else
                     None
          in
          
          let modrm = encode_modrm 0b11 4 (reg_code land 0x7) in
          
          { encoded with
            rex;
            opcode = [0xFF];
            modrm = Some modrm
          }
      
      | Mem mem ->
          let (rex_needed, rex_b, rex_x, segment, modrm_base, sib, disp) = encode_rm_operand target in
          
          let rex = if rex_needed then
                     Some (combine_rex false false rex_x rex_b)
                   else
                     None
          in
          
          let prefix = match segment with
                       | Some CS -> [0x2E]
                       | Some DS -> [] (* Default segment, no prefix needed *)
                       | Some ES -> [0x26]
                       | Some FS -> [0x64]
                       | Some GS -> [0x65]
                       | Some SS -> [0x36]
                       | None -> []
          in
          
          let modrm = modrm_base lor (4 lsl 3) in
          
          { encoded with
            prefixes = prefix;
            rex;
            opcode = [0xFF];
            modrm = Some modrm;
            sib;
            displacement = disp
          }
      
      | _ -> failwith "Unsupported operand for JMP"
  | _ -> failwith "JMP requires one operand"

let encode_jcc instr =
  match instr.variant with
  | CondJump (cond, lbl) ->
      let encoded = empty_encoded_instr in
      
      let cond_code = condition_code_to_opcode cond in
      
      { encoded with
        opcode = [0x0F; 0x80 lor cond_code];
        rel_addr = Some 0 (* Will be resolved later *)
      }
  | _ -> failwith "JCC requires a condition code and label"

let encode_call instr =
  match instr.variant with
  | Op1 target ->
      let encoded = empty_encoded_instr in
      
      match target with
      | Label lbl ->
          { encoded with
            opcode = [0xE8];
            rel_addr = Some 0 (* Will be resolved later *)
          }
      
      | RelAddr offset ->
          { encoded with
            opcode = [0xE8];
            immediate = encode_rel_addr offset Dword
          }
      
      | Reg reg ->
          let reg_code = get_reg_code reg in
          let rex_needed = needs_rex reg in
          let rex = if rex_needed then
                     Some (combine_rex false false false (reg_code land 0x8 <> 0))
                   else
                     None
          in
          
          let modrm = encode_modrm 0b11 2 (reg_code land 0x7) in
          
          { encoded with
            rex;
            opcode = [0xFF];
            modrm = Some modrm
          }
      
      | Mem mem ->
          let (rex_needed, rex_b, rex_x, segment, modrm_base, sib, disp) = encode_rm_operand target in
          
          let rex = if rex_needed then
                     Some (combine_rex false false rex_x rex_b)
                   else
                     None
          in
          
          let prefix = match segment with
                       | Some CS -> [0x2E]
                       | Some DS -> [] (* Default segment, no prefix needed *)
                       | Some ES -> [0x26]
                       | Some FS -> [0x64]
                       | Some GS -> [0x65]
                       | Some SS -> [0x36]
                       | None -> []
          in
          
          let modrm = modrm_base lor (2 lsl 3) in
          
          { encoded with
            prefixes = prefix;
            rex;
            opcode = [0xFF];
            modrm = Some modrm;
            sib;
            displacement = disp
          }
      
      | _ -> failwith "Unsupported operand for CALL"
  | _ -> failwith "CALL requires one operand"

let encode_ret instr =
  match instr.variant with
  | NoOp ->
      let encoded = empty_encoded_instr in
      { encoded with opcode = [0xC3] }
  | _ -> failwith "RET does not take operands"

let encode_instruction instr =
  match instr.mnemonic with
  | MOV -> encode_mov instr
  | LEA -> encode_lea instr
  | PUSH -> encode_push instr
  | ADD -> encode_add instr
  | ADC -> encode_adc instr
  | SUB -> encode_sub instr
  | SBB -> encode_sbb instr
  | AND -> encode_and instr
  | OR -> encode_or instr
  | XOR -> encode_xor instr
  | NOT -> encode_not instr
  | NEG -> encode_neg instr
  | INC -> encode_inc instr
  | DEC -> encode_dec instr
  | CMP -> encode_cmp instr
  | JMP -> encode_jmp instr
  | JCC -> encode_jcc instr
  | CALL -> encode_call instr
  | RET -> encode_ret instr
  | _ -> failwith (Printf.sprintf "Encoding for %s is not implemented yet" (show_instruction_type instr.mnemonic))

let encoded_to_bytes encoded =
  List.concat [
    encoded.prefixes;
    (match encoded.rex with Some r -> [r] | None -> []);
    encoded.opcode;
    (match encoded.modrm with Some m -> [m] | None -> []);
    (match encoded.sib with Some s -> [s] | None -> []);
    encoded.displacement;
    encoded.immediate;
  ]