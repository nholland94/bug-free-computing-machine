type t =
    FunctionSignature      of (string, arguments)
  | FunctionImplementation of (FunctionSignature, expression)

type expression = 
    BlockExp of expression list
  | FunctionCall of (string, expression list)
  | VariableReference of string
  | ConstantInteger of int
  | ConstantString of string
  | ArrayExp of expression list
  | ListExp of expression list
  | VoidExp

type arguments = (string, view_definition) list | string list
