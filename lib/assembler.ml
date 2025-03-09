open Types
open Register
open Instruction
open Operand
open Encoder

type label_reference = {
  offset: int;
  size: size;
  addend: int;
}

type assembler_state = {
  code: int list;
  offset: int;
  labels: (string, int) Hashtbl.t;
  unresolved_labels: (string, label_reference list) Hashtbl.t;
}

let init_state () = {
  code = [];
  offset = 0;
  labels = Hashtbl.create 32;
  unresolved_labels = Hashtbl.create 32;
}

let append_bytes state bytes =
  { state with
    code = state.code @ bytes;
    offset = state.offset + List.length bytes
  }

let define_label state name =
  Hashtbl.replace state.labels name state.offset;
  
  if Hashtbl.mem state.unresolved_labels name then
    let refs = Hashtbl.find state.unresolved_labels name in
    Hashtbl.remove state.unresolved_labels name;
    
    let rec patch_refs state = function
      | [] -> state
      | ref :: rest ->
          let target_addr = state.offset in
          let ref_addr = ref.offset + ref.size_to_bytes ref.size;
          let rel_addr = target_addr - ref_addr + ref.addend in
          
          let bytes = match ref.size with
            | Byte -> [rel_addr land 0xFF]
            | Dword -> [
                rel_addr land 0xFF;
                (rel_addr lsr 8) land 0xFF;
                (rel_addr lsr 16) land 0xFF;
                (rel_addr lsr 24) land 0xFF
              ]
            | _ -> failwith "Unsupported relocation size"
          in
          
          let code_before = List.filteri (fun i _ -> i < ref.offset) state.code in
          let code_after = List.filteri (fun i _ -> i >= ref.offset + List.length bytes) state.code in
          let new_code = code_before @ bytes @ code_after in
          
          patch_refs { state with code = new_code } rest
    in
    
    patch_refs state refs
  else
    state

let add_unresolved_label state name ref =
  let refs = try Hashtbl.find state.unresolved_labels name
             with Not_found -> [] in
  Hashtbl.replace state.unresolved_labels name (ref :: refs);
  state

let assemble_instruction state instr =
  let encoded = encode_instruction instr in
  let bytes = encoded_to_bytes encoded in
  
  let state = append_bytes state bytes in
  
  match instr.variant with
  | CondJump (_, lbl) ->
      let ref = {
        offset = state.offset - 4;
        size = Dword;
        addend = 0
      } in
      add_unresolved_label state lbl ref
  
  | Op1 (Label lbl) ->
      let ref = {
        offset = state.offset - 4;
        size = Dword;
        addend = 0
      } in
      add_unresolved_label state lbl ref
  
  | _ -> state

let assemble_program instrs =
  let state = init_state () in
  
  let rec process_instrs state = function
    | [] -> state
    | (Some lbl, instr) :: rest ->
        let state = define_label state lbl in
        let state = assemble_instruction state instr in
        process_instrs state rest
    | (None, instr) :: rest ->
        let state = assemble_instruction state instr in
        process_instrs state rest
  in
  
  let final_state = process_instrs state instrs in
  
  if Hashtbl.length final_state.unresolved_labels > 0 then
    let unresolved = Hashtbl.fold (fun name _ acc -> name :: acc) final_state.unresolved_labels [] in
    failwith (Printf.sprintf "Unresolved labels: %s" (String.concat ", " unresolved))
  else
    final_state.code