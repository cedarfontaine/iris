module Types = Types
module Register = Register
module Instruction = Instruction
module Operand = Operand
module Encoder = Encoder
module Assembler = Assembler
module Emitter = Emitter

open Types
open Register
open Instruction
open Operand
open Assembler
open Emitter

type program = (string option * instruction) list

let create_program () = []

let add_instruction prog instr =
  prog @ [(None, instr)]

let add_labeled_instruction prog label instr =
  prog @ [(Some label, instr)]

let add_label prog label =
  prog @ [(Some label, nop ())]

let assemble prog =
  assemble_program prog

let emit filename prog =
  let code = assemble prog in
  emit_executable filename code

let print_instruction instr =
  let mnemonic = show_instruction_type instr.mnemonic in
  
  let operands_str = match instr.variant with
    | NoOp -> ""
    | Op1 op -> operand_to_string op
    | Op2 (op1, op2) -> Printf.sprintf "%s, %s" (operand_to_string op1) (operand_to_string op2)
    | Op3 (op1, op2, op3) -> Printf.sprintf "%s, %s, %s" 
                             (operand_to_string op1) 
                             (operand_to_string op2)
                             (operand_to_string op3)
    | Op4 (op1, op2, op3, op4) -> Printf.sprintf "%s, %s, %s, %s"
                                 (operand_to_string op1)
                                 (operand_to_string op2)
                                 (operand_to_string op3)
                                 (operand_to_string op4)
    | CondJump (cond, lbl) -> Printf.sprintf "%s" lbl
  in
  
  Printf.sprintf "%s %s" mnemonic operands_str

let print_program prog =
  let print_line (label, instr) =
    match label with
    | Some l -> Printf.sprintf "%s:\n    %s" l (print_instruction instr)
    | None -> Printf.sprintf "    %s" (print_instruction instr)
  in
  
  List.map print_line prog |> String.concat "\n"

let print_hex_dump bytes =
  let rec print_bytes offset bytes =
    match bytes with
    | [] -> []
    | _ ->
        let line_bytes, rest = 
          if List.length bytes <= 16 then (bytes, [])
          else (List.filteri (fun i _ -> i < 16) bytes, 
                List.filteri (fun i _ -> i >= 16) bytes)
        in
        
        let hex_str = List.map (Printf.sprintf "%02x") line_bytes |> String.concat " " in
        let padding = String.make (3 * (16 - List.length line_bytes)) ' ' in
        
        let char_str = List.map (fun b -> 
          if b >= 32 && b <= 126 then Char.chr b else '.'
        ) line_bytes |> List.to_seq |> String.of_seq in
        
        let line = Printf.sprintf "%08x  %s%s  |%s|" 
                   offset hex_str padding char_str
        in
        
        line :: print_bytes (offset + 16) rest
  in
  
  print_bytes 0 bytes |> String.concat "\n"

let encode prog =
  let instrs = List.map snd prog in
  List.map encode_instruction instrs