open Types

type elf_section = {
  name: string;
  typ: section_type;
  data: int list;
  addr: int64;
  flags: int64;
  link: int;
  info: int;
  addralign: int64;
  entsize: int64;
}

type elf_symbol = {
  name: string;
  value: int64;
  size: int64;
  section: int;
  info: int;
  other: int;
}

type elf_relocation = {
  offset: int64;
  info: int64;
  addend: int64;
}

type elf_file = {
  sections: elf_section list;
  symbols: elf_symbol list;
  relocations: (int * elf_relocation list) list;
}

let init_elf () = {
  sections = [];
  symbols = [];
  relocations = [];
}

let add_section elf name typ data addr flags =
  let section = {
    name;
    typ;
    data;
    addr;
    flags;
    link = 0;
    info = 0;
    addralign = 16L;
    entsize = 0L;
  } in
  { elf with sections = elf.sections @ [section] }

let add_symbol elf name value size section info other =
  let symbol = {
    name;
    value;
    size;
    section;
    info;
    other;
  } in
  { elf with symbols = elf.symbols @ [symbol] }

let add_relocation elf section_idx rel =
  let rels = 
    try List.assoc section_idx elf.relocations
    with Not_found -> []
  in
  let relocations = 
    (section_idx, rels @ [rel]) ::
    List.filter (fun (idx, _) -> idx <> section_idx) elf.relocations
  in
  { elf with relocations }

let write_int8 ch byte =
  output_byte ch (byte land 0xFF)

let write_int16 ch value =
  write_int8 ch value;
  write_int8 ch (value lsr 8)

let write_int32 ch value =
  write_int8 ch value;
  write_int8 ch (value lsr 8);
  write_int8 ch (value lsr 16);
  write_int8 ch (value lsr 24)

let write_int64 ch value =
  write_int32 ch (Int64.to_int value);
  write_int32 ch (Int64.to_int (Int64.shift_right value 32))

let write_string ch str =
  String.iter (fun c -> output_char ch c) str;
  output_char ch '\000'

let write_section_data ch data =
  List.iter (fun byte -> write_int8 ch byte) data

let write_elf_header ch elf =
  write_int8 ch 0x7F;
  write_int8 ch (int_of_char 'E');
  write_int8 ch (int_of_char 'L');
  write_int8 ch (int_of_char 'F');

  write_int8 ch 2;
  write_int8 ch 1;
  write_int8 ch 1;
  write_int8 ch 0;
  write_int8 ch 0;
  
  for i = 0 to 6 do
    write_int8 ch 0
  done;
 
  write_int16 ch 1;
  write_int16 ch 62;
  write_int32 ch 1;
  write_int64 ch 0L;
  write_int64 ch 0L;
  
  let section_header_pos = pos_out ch in
  write_int64 ch 0L;
  write_int32 ch 0;
  write_int16 ch 64;
  write_int16 ch 0;
  write_int16 ch 0;
  write_int16 ch 64;
  
  let section_count_pos = pos_out ch in
  write_int16 ch 0;

  let shstrtab_index_pos = pos_out ch in
  write_int16 ch 0;
  
  (section_header_pos, section_count_pos, shstrtab_index_pos)

let write_section_header ch name_offset typ flags offset size link info addralign entsize =
  write_int32 ch name_offset;
  write_int32 ch typ;
  write_int64 ch flags;
  write_int64 ch 0L;
  write_int64 ch (Int64.of_int offset);
  write_int64 ch (Int64.of_int size);
  write_int32 ch link;
  write_int32 ch info;
  write_int64 ch addralign;
  write_int64 ch entsize

let create_shstrtab elf =
  let buf = Buffer.create 256 in
  Buffer.add_char buf '\000';  (* Empty string at index 0 *)
  
  let offsets = ref [] in
  
  let add_name name =
    let offset = Buffer.length buf in
    offsets := (name, offset) :: !offsets;
    Buffer.add_string buf name;
    Buffer.add_char buf '\000'
  in
  
  add_name ".shstrtab";
  add_name ".strtab";
  add_name ".symtab";
  add_name ".text";
  add_name ".data";
  add_name ".bss";
  add_name ".rodata";
  
  List.iter (fun section ->
    if not (List.exists (fun (name, _) -> name = section.name) !offsets) then
      add_name section.name
  ) elf.sections;
  
  List.iter (fun (section_idx, _) ->
    let section = List.nth elf.sections (section_idx - 1) in
    let rel_name = ".rel" ^ section.name in
    if not (List.exists (fun (name, _) -> name = rel_name) !offsets) then
      add_name rel_name
  ) elf.relocations;
  
  (Buffer.contents buf |> String.to_seq |> List.of_seq |> List.map int_of_char, !offsets)

let create_strtab elf =
  let buf = Buffer.create 256 in
  Buffer.add_char buf '\000';  (* Empty string at index 0 *)
  
  let offsets = ref [] in
  
  List.iter (fun sym ->
    if not (List.exists (fun (name, _) -> name = sym.name) !offsets) then
      let offset = Buffer.length buf in
      offsets := (sym.name, offset) :: !offsets;
      Buffer.add_string buf sym.name;
      Buffer.add_char buf '\000'
  ) elf.symbols;
  
  (Buffer.contents buf |> String.to_seq |> List.of_seq |> List.map int_of_char, !offsets)

let get_section_type = function
  | Text -> 1      (* SHT_PROGBITS *)
  | Data -> 1      (* SHT_PROGBITS *)
  | Bss -> 8       (* SHT_NOBITS *)
  | Rodata -> 1    (* SHT_PROGBITS *)

let get_section_flags = function
  | Text -> 0x6L    (* SHF_ALLOC | SHF_EXECINSTR *)
  | Data -> 0x3L    (* SHF_ALLOC | SHF_WRITE *)
  | Bss -> 0x3L     (* SHF_ALLOC | SHF_WRITE *)
  | Rodata -> 0x2L  (* SHF_ALLOC *)

let write_elf_file filename code =
  let elf = init_elf () in
  let elf = add_section elf ".text" Text code 0L (get_section_flags Text) in
  
  let out_ch = open_out_bin filename in
  
  let (section_header_pos, section_count_pos, shstrtab_index_pos) = write_elf_header out_ch elf in
  
  let section_offsets = ref [] in
  let current_offset = ref (pos_out out_ch) in
  
  let text_offset = !current_offset in
  section_offsets := (0, text_offset) :: !section_offsets;
  write_section_data out_ch code;
  current_offset := pos_out out_ch;
  
  while !current_offset mod 8 <> 0 do
    write_int8 out_ch 0;
    incr current_offset
  done;
  
  let (shstrtab_data, shstrtab_offsets) = create_shstrtab elf in
  let shstrtab_offset = !current_offset in
  let shstrtab_index = 1 in  (* Index in the section headers *)
  section_offsets := (shstrtab_index, shstrtab_offset) :: !section_offsets;
  write_section_data out_ch shstrtab_data;
  current_offset := pos_out out_ch;
  
  while !current_offset mod 8 <> 0 do
    write_int8 out_ch 0;
    incr current_offset
  done;
  
  let section_header_offset = !current_offset in
  
  let section_count = 2 in  
  seek_out out_ch section_header_pos;
  write_int64 out_ch (Int64.of_int section_header_offset);
  
  seek_out out_ch section_count_pos;
  write_int16 out_ch section_count;
  
  seek_out out_ch shstrtab_index_pos;
  write_int16 out_ch shstrtab_index;
  
  seek_out out_ch section_header_offset;
  
  for i = 0 to 15 do
    write_int32 out_ch 0
  done;
  
  let shstrtab_name_offset = List.assoc ".shstrtab" shstrtab_offsets in
  write_section_header out_ch shstrtab_name_offset 3 0L shstrtab_offset (List.length shstrtab_data) 0 0 1L 0L;
  
  let text_name_offset = List.assoc ".text" shstrtab_offsets in
  let text_type = get_section_type Text in
  let text_flags = get_section_flags Text in
  write_section_header out_ch text_name_offset text_type text_flags text_offset (List.length code) 0 0 16L 0L;
  
  close_out out_ch

let emit_executable filename code =
  write_elf_file filename code