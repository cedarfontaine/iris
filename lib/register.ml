open Types

type gpr = 
  | RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI
  | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15
  | EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI
  | R8D | R9D | R10D| R11D| R12D| R13D| R14D| R15D
  | AX  | CX  | DX  | BX  | SP  | BP  | SI  | DI
  | R8W | R9W | R10W| R11W| R12W| R13W| R14W| R15W
  | AL  | CL  | DL  | BL  | SPL | BPL | SIL | DIL
  | R8B | R9B | R10B| R11B| R12B| R13B| R14B| R15B
  | AH  | CH  | DH  | BH
[@@deriving show, eq, ord]

type xmm_reg = XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7
              | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15
[@@deriving show, eq, ord]

type ymm_reg = YMM0 | YMM1 | YMM2 | YMM3 | YMM4 | YMM5 | YMM6 | YMM7
              | YMM8 | YMM9 | YMM10 | YMM11 | YMM12 | YMM13 | YMM14 | YMM15
[@@deriving show, eq, ord]

type register = 
  | GPR of gpr
  | XMM of xmm_reg
  | YMM of ymm_reg
[@@deriving show, eq, ord]

let register_size = function
  | GPR r -> begin match r with
    | RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI
    | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15 -> Qword
    | EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI
    | R8D | R9D | R10D| R11D| R12D| R13D| R14D| R15D -> Dword
    | AX  | CX  | DX  | BX  | SP  | BP  | SI  | DI
    | R8W | R9W | R10W| R11W| R12W| R13W| R14W| R15W -> Word
    | AL  | CL  | DL  | BL  | SPL | BPL | SIL | DIL
    | R8B | R9B | R10B| R11B| R12B| R13B| R14B| R15B
    | AH  | CH  | DH  | BH -> Byte
    end
  | XMM _ -> Qword
  | YMM _ -> Qword

let get_reg_code = function
  | GPR r -> begin match r with
    | RAX | EAX | AX | AL -> 0
    | RCX | ECX | CX | CL -> 1
    | RDX | EDX | DX | DL -> 2
    | RBX | EBX | BX | BL -> 3
    | RSP | ESP | SP | SPL -> 4
    | RBP | EBP | BP | BPL -> 5
    | RSI | ESI | SI | SIL -> 6
    | RDI | EDI | DI | DIL -> 7
    | R8  | R8D | R8W | R8B -> 8
    | R9  | R9D | R9W | R9B -> 9
    | R10 | R10D| R10W| R10B -> 10
    | R11 | R11D| R11W| R11B -> 11
    | R12 | R12D| R12W| R12B -> 12
    | R13 | R13D| R13W| R13B -> 13
    | R14 | R14D| R14W| R14B -> 14
    | R15 | R15D| R15W| R15B -> 15
    | AH -> 4
    | CH -> 5
    | DH -> 6
    | BH -> 7
    end
  | XMM reg -> begin match reg with
    | XMM0 -> 0 | XMM1 -> 1 | XMM2 -> 2 | XMM3 -> 3
    | XMM4 -> 4 | XMM5 -> 5 | XMM6 -> 6 | XMM7 -> 7
    | XMM8 -> 8 | XMM9 -> 9 | XMM10 -> 10 | XMM11 -> 11
    | XMM12 -> 12 | XMM13 -> 13 | XMM14 -> 14 | XMM15 -> 15
    end
  | YMM reg -> begin match reg with
    | YMM0 -> 0 | YMM1 -> 1 | YMM2 -> 2 | YMM3 -> 3
    | YMM4 -> 4 | YMM5 -> 5 | YMM6 -> 6 | YMM7 -> 7
    | YMM8 -> 8 | YMM9 -> 9 | YMM10 -> 10 | YMM11 -> 11
    | YMM12 -> 12 | YMM13 -> 13 | YMM14 -> 14 | YMM15 -> 15
    end

let needs_rex reg =
  match reg with
  | GPR r -> begin match r with
    | SPL | BPL | SIL | DIL -> true
    | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15
    | R8D | R9D | R10D| R11D| R12D| R13D| R14D| R15D
    | R8W | R9W | R10W| R11W| R12W| R13W| R14W| R15W
    | R8B | R9B | R10B| R11B| R12B| R13B| R14B| R15B -> true
    | _ -> false
    end
  | XMM reg -> begin match reg with
    | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15 -> true
    | _ -> false
    end
  | YMM reg -> begin match reg with
    | YMM8 | YMM9 | YMM10 | YMM11 | YMM12 | YMM13 | YMM14 | YMM15 -> true
    | _ -> false
    end

let rax = GPR RAX
let rcx = GPR RCX
let rdx = GPR RDX
let rbx = GPR RBX
let rsp = GPR RSP
let rbp = GPR RBP
let rsi = GPR RSI
let rdi = GPR RDI
let r8  = GPR R8
let r9  = GPR R9
let r10 = GPR R10
let r11 = GPR R11
let r12 = GPR R12
let r13 = GPR R13
let r14 = GPR R14
let r15 = GPR R15

let eax = GPR EAX
let ecx = GPR ECX
let edx = GPR EDX
let ebx = GPR EBX
let esp = GPR ESP
let ebp = GPR EBP
let esi = GPR ESI
let edi = GPR EDI
let r8d  = GPR R8D
let r9d  = GPR R9D
let r10d = GPR R10D
let r11d = GPR R11D
let r12d = GPR R12D
let r13d = GPR R13D
let r14d = GPR R14D
let r15d = GPR R15D

let ax = GPR AX
let cx = GPR CX
let dx = GPR DX
let bx = GPR BX
let sp = GPR SP
let bp = GPR BP
let si = GPR SI
let di = GPR DI
let r8w  = GPR R8W
let r9w  = GPR R9W
let r10w = GPR R10W
let r11w = GPR R11W
let r12w = GPR R12W
let r13w = GPR R13W
let r14w = GPR R14W
let r15w = GPR R15W

let al = GPR AL
let cl = GPR CL
let dl = GPR DL
let bl = GPR BL
let spl = GPR SPL
let bpl = GPR BPL
let sil = GPR SIL
let dil = GPR DIL
let r8b  = GPR R8B
let r9b  = GPR R9B
let r10b = GPR R10B
let r11b = GPR R11B
let r12b = GPR R12B
let r13b = GPR R13B
let r14b = GPR R14B
let r15b = GPR R15B

let ah = GPR AH
let ch = GPR CH
let dh = GPR DH
let bh = GPR BH

let xmm0 = XMM XMM0
let xmm1 = XMM XMM1
let xmm2 = XMM XMM2
let xmm3 = XMM XMM3
let xmm4 = XMM XMM4
let xmm5 = XMM XMM5
let xmm6 = XMM XMM6
let xmm7 = XMM XMM7
let xmm8 = XMM XMM8
let xmm9 = XMM XMM9
let xmm10 = XMM XMM10
let xmm11 = XMM XMM11
let xmm12 = XMM XMM12
let xmm13 = XMM XMM13
let xmm14 = XMM XMM14
let xmm15 = XMM XMM15

let ymm0 = YMM YMM0
let ymm1 = YMM YMM1
let ymm2 = YMM YMM2
let ymm3 = YMM YMM3
let ymm4 = YMM YMM4
let ymm5 = YMM YMM5
let ymm6 = YMM YMM6
let ymm7 = YMM YMM7
let ymm8 = YMM YMM8
let ymm9 = YMM YMM9
let ymm10 = YMM YMM10
let ymm11 = YMM YMM11
let ymm12 = YMM YMM12
let ymm13 = YMM YMM13
let ymm14 = YMM YMM14
let ymm15 = YMM YMM15

let get_rex_rb reg =
  let code = get_reg_code reg in
  (code land 0x8) <> 0

let get_base_reg = function
  | GPR r -> begin match r with
    | RAX | EAX | AX | AL -> 0
    | RCX | ECX | CX | CL -> 1
    | RDX | EDX | DX | DL -> 2
    | RBX | EBX | BX | BL -> 3
    | RSP | ESP | SP | SPL -> 4
    | RBP | EBP | BP | BPL -> 5
    | RSI | ESI | SI | SIL -> 6
    | RDI | EDI | DI | DIL -> 7
    | R8  | R8D | R8W | R8B -> 0
    | R9  | R9D | R9W | R9B -> 1
    | R10 | R10D| R10W| R10B -> 2
    | R11 | R11D| R11W| R11B -> 3
    | R12 | R12D| R12W| R12B -> 4
    | R13 | R13D| R13W| R13B -> 5
    | R14 | R14D| R14W| R14B -> 6
    | R15 | R15D| R15W| R15B -> 7
    | AH -> 4
    | CH -> 5
    | DH -> 6
    | BH -> 7
    end
  | XMM reg -> begin match reg with
    | XMM0 | XMM8 -> 0
    | XMM1 | XMM9 -> 1
    | XMM2 | XMM10 -> 2
    | XMM3 | XMM11 -> 3
    | XMM4 | XMM12 -> 4
    | XMM5 | XMM13 -> 5
    | XMM6 | XMM14 -> 6
    | XMM7 | XMM15 -> 7
    end
  | YMM reg -> begin match reg with
    | YMM0 | YMM8 -> 0
    | YMM1 | YMM9 -> 1
    | YMM2 | YMM10 -> 2
    | YMM3 | YMM11 -> 3
    | YMM4 | YMM12 -> 4
    | YMM5 | YMM13 -> 5
    | YMM6 | YMM14 -> 6
    | YMM7 | YMM15 -> 7
    end