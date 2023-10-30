type tp =
  (* only for function ret*)
  | TVoid (** void *)
  (* primitive types*)
  | TFloat (** float *)
  | TInteger of int (** i1, i2 ... in *)
  | TPointer (** ptr *)
  (* first class types*)
  | TVector of int * tp (** <int x primitive_type> *)
  | TArr of int * tp (** [int x type] *)
  | TStruct of tp list (** \{type1, type2...\} *)
  (* additional types*)
  | TLabel (** label *)
  | TFunc of tp * tp list (** <returntype> (<parameter list>) *)


type variable =
  | LocalVar of string (** %name *)
  | GlobalVar of string (** \@name *)

type pointer_const =
  | PointerGlob of variable
  | PointerInt of int

type align = int (* just for better reading*)

type const =
  | CVoid
  | CInteger of int * int (** size and value*)
  | CFloat of float
  | CPointer of pointer_const
  | CVector of const list (** <const, const, ...> *)
  | CArr of const list (** [const, const, ...] *)
  | CStruct of const list (** \{const, const, ...\} *)
  | CLabel of basic_block
  | CFunc of func

and value =
  | FromVariable of variable * tp
  | Const of const

(* ############ Instructions Start ########### *)
and terminator_instruction =
  | Ret of tp * value (** ret <type> <value> *)
  | Br of value (** br label <dest> *)
  | BrCond of value * value * value (** br i1 <cond>, label <iftrue>, label <iffalse> *)

and binary_operation_body =
  variable * tp * value * value (* <result> = <bin_op> <ty> <val1>, <val2> *)

and binary_operation =
  | Mul of binary_operation_body
  | Sub of binary_operation_body

and other_operation =
  | Icmp of variable * string * tp * value * value
  (** <result> = icmp <cond> <ty> <op1>, <op2> *)
  | Call of variable * tp * value * value list
  (** <result> = call <ty> <fnptrval>(<function args>) *)

and memory_address_inst =
  | Alloca of variable * tp * value * align
  (** <result> = alloca <type> [, <ty> <NumElements>] [, align <alignment>] *)
  | Store of tp * value * value * align
  (** store <ty> <value>, ptr <pointer>[, align <alignment>] *)
  | Load of variable * tp * value * align
  (** <result> = load <ty>, ptr <pointer>[, align <alignment>]*)

and instruction =
  | Terminator of terminator_instruction
  | Binary of binary_operation
  | Other of other_operation
  | MemoryAddress of memory_address_inst

and basic_block = instruction list [@@deriving show { with_path = false }]

and func =
  { parameters : variable list
  ; basic_blocks : (variable * const) list
  }

type glob_list = (tp * variable * const) list [@@deriving show { with_path = false }]
