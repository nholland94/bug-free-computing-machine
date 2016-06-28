(* temp *)
type view_definition = string
type arguments =
    Typed of (string * view_definition) list
  | Untyped of string list

type function_signature = string * arguments

type expression = 
    BlockExp of expression list
  | FunctionCall of string * expression list
  | VariableReference of string
  | ConstantInteger of int
  | ConstantString of string
  | ArrayExp of expression list
  | ListExp of expression list
  | DirectAssignment of string * expression
  | ArrayMatchingAssignment of expression * expression
  | ListMatchingAssignment of expression * expression
  | ConstantMatchingAssignment of expression * expression
  | WhileExp of expression * expression
  | IfExp of expression * expression * expression
  | VoidExp

type t =
    FunctionSignature      of function_signature
  | FunctionImplementation of function_signature * expression

val string_of_ast : t -> string
val string_of_expression : expression -> string
val string_of_arguments : arguments -> string
