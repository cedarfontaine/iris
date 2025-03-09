open Iris
open Cmdliner

let generate_example_program () =
  let open Iris.X86 in
  let open Iris.Register in
  let open Iris.Instruction in
  
  let prog = create_program () in
  let prog = add_label prog "global_start" in
  let prog = add_instruction prog (mov rax (imm64 42L)) in
  let prog = add_instruction prog (mov rdi rax) in
  let prog = add_instruction prog (mov rax (imm64 60L)) in  (* sys_exit *)
  let prog = add_instruction prog (syscall ()) in
  
  prog

let generate_fibonacci_example () =
  let open Iris.X86 in
  let open Iris.Register in
  let open Iris.Instruction in
  
  let prog = create_program () in
  let prog = add_label prog "global_start" in
  
  let prog = add_instruction prog (mov rax (imm64 0L)) in     (* First number *)
  let prog = add_instruction prog (mov rbx (imm64 1L)) in     (* Second number *)
  let prog = add_instruction prog (mov rcx (imm64 10L)) in    (* Counter *)
  
  let prog = add_label prog "loop" in
  let prog = add_instruction prog (mov rdx rax) in            (* Save first number *)
  let prog = add_instruction prog (add rax rbx) in            (* Add second number to first *)
  let prog = add_instruction prog (mov rbx rdx) in            (* Move old first to second *)
  let prog = add_instruction prog (dec rcx) in                (* Decrement counter *)
  let prog = add_instruction prog (cmp rcx (imm64 0L)) in     (* Check if counter is zero *)
  let prog = add_instruction prog (jne "loop") in             (* If not, loop *)
  
  (* Exit with Fibonacci number as return code *)
  let prog = add_instruction prog (mov rdi rax) in            (* Move result to rdi *)
  let prog = add_instruction prog (mov rax (imm64 60L)) in    (* sys_exit *)
  let prog = add_instruction prog (syscall ()) in
  
  prog

let generate_memory_example () =
  let open Iris.X86 in
  let open Iris.Register in
  let open Iris.Instruction in
  let open Iris.Operand in
  
  let prog = create_program () in
  let prog = add_label prog "global_start" in
  let prog = add_instruction prog (sub rsp (imm64 16L)) in
  let prog = add_instruction prog (mov (qword_ptr rsp) (imm64 123L)) in
  let prog = add_instruction prog (mov (qword_ptr ~disp:(disp8 8) rsp) (imm64 456L)) in
  let prog = add_instruction prog (mov rax (mem_reg Qword rsp)) in
  let prog = add_instruction prog (mov rbx (mem_reg ~disp:(disp8 8) Qword rsp)) in
  let prog = add_instruction prog (add rax rbx) in
  let prog = add_instruction prog (mov rdi rax) in
  let prog = add_instruction prog (mov rax (imm64 60L)) in (* sys_exit *)
  let prog = add_instruction prog (syscall ()) in
  
  prog

let print_example examples output =
  let example = 
    match examples with
    | `Basic -> generate_example_program ()
    | `Fibonacci -> generate_fibonacci_example ()
    | `Memory -> generate_memory_example ()
  in
  let asm_output = Iris.X86.print_program example in
  
  match output with
  | None -> print_endline asm_output
  | Some file ->
      let oc = open_out file in
      output_string oc asm_output;
      close_out oc;
      Printf.printf "Assembly written to %s\n" file

let compile_example examples output =
  let example = 
    match examples with
    | `Basic -> generate_example_program ()
    | `Fibonacci -> generate_fibonacci_example ()
    | `Memory -> generate_memory_example ()
  in
  
  let output_file = 
    match output with
    | None -> "output.o"
    | Some file -> file
  in
  
  Iris.X86.emit output_file example;
  Printf.printf "Binary written to %s\n" output_file

let disassemble_example examples =
  let example = 
    match examples with
    | `Basic -> generate_example_program ()
    | `Fibonacci -> generate_fibonacci_example ()
    | `Memory -> generate_memory_example ()
  in
  
  let encoded = Iris.X86.encode example in
  let bytes = List.map Iris.Encoder.encoded_to_bytes encoded |> List.concat in
  let hex_dump = Iris.X86.print_hex_dump bytes in
  
  print_endline "Code disassembly:";
  print_endline hex_dump

let example_enum = [
  ("basic", `Basic);
  ("fibonacci", `Fibonacci);
  ("memory", `Memory);
]

let example_arg =
  let doc = "Example program to generate." in
  Arg.(value & opt (enum example_enum) `Basic & 
        info ["e"; "example"] ~docv:"EXAMPLE" ~doc)

let output_arg =
  let doc = "Output file." in
  Arg.(value & opt (some string) None & 
        info ["o"; "output"] ~docv:"FILE" ~doc)

let print_cmd =
  let doc = "Print assembly code for an example program" in
  let info = Cmd.info "print" ~doc in
  Cmd.v info Term.(const print_example $ example_arg $ output_arg)

let compile_cmd =
  let doc = "Compile an example program to a binary file" in
  let info = Cmd.info "compile" ~doc in
  Cmd.v info Term.(const compile_example $ example_arg $ output_arg)

let disassemble_cmd =
  let doc = "Disassemble an example program" in
  let info = Cmd.info "disassemble" ~doc in
  Cmd.v info Term.(const disassemble_example $ example_arg)

let default_cmd =
  let doc = "x86 code generation library in OCaml" in
  let info = Cmd.info "iris" ~version:"0.1.0" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.v info default

let cmd =
  Cmd.group (Cmd.info "iris" ~version:"0.1.0") 
    [print_cmd; compile_cmd; disassemble_cmd]

let () = exit (Cmd.eval cmd)