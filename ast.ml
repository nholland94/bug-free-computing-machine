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

let string_of_arguments(args) =
  let arg_strings = match args with
      Typed ls ->
        List.map (fun(a, b) -> a ^ b) ls
    | Untyped ls ->
        ls
  in
  String.concat ", " arg_strings

let rec string_of_expression(exp) =
  let wrap_parens(value) = "(" ^ value ^ ")" in
  let wrap_list_parens(ls) = wrap_parens @@ String.concat ", " ls in
  match exp with
      BlockExp expressions ->
        let expression_strings = List.map string_of_expression expressions in
        "Block" ^ (wrap_list_parens expression_strings)
    | FunctionCall (name, expressions) ->
        let expression_strings = List.map string_of_expression expressions in
        "FunctionCall" ^ (wrap_list_parens (name :: expression_strings))
    | VariableReference name ->
        "VariableReference" ^ (wrap_parens name)
    | ConstantInteger n ->
        "ConstantInteger" ^ (wrap_parens (string_of_int n))
    | ConstantString str ->
        "ConstantString" ^ (wrap_parens str)
    | ArrayExp expressions ->
        let expression_strings = List.map string_of_expression expressions in
        "ArrayExp" ^ (wrap_list_parens expression_strings)
    | ListExp expressions ->
        let expression_strings = List.map string_of_expression expressions in
        "ListExp" ^ (wrap_list_parens expression_strings)
    | DirectAssignment (name, assignment_exp) ->
        "DirectAssignment" ^ (wrap_list_parens [name; string_of_expression assignment_exp])
    | ArrayMatchingAssignment (left, right) ->
        "ArrayMatchingAssignment" ^ (wrap_list_parens (List.map string_of_expression [left; right]))
    | ListMatchingAssignment (left, right) ->
        "ListMatchingAssignment" ^ (wrap_list_parens (List.map string_of_expression [left; right]))
    | ConstantMatchingAssignment (left, right) ->
        "ConstantMatchingAssignment" ^ (wrap_list_parens (List.map string_of_expression [left; right]))
    | WhileExp (matching_exp, body_exp) ->
        "WhileExp" ^ (wrap_list_parens (List.map string_of_expression [matching_exp; body_exp]))
    | IfExp (condition_exp, true_exp, false_exp) ->
        "IfExp" ^ (wrap_list_parens (List.map string_of_expression [condition_exp; true_exp; false_exp]))
    | VoidExp ->
        "VOID"

let rec string_of_ast(ast) =
  match ast with
      FunctionSignature(name, args) ->
        let args_string = string_of_arguments args in
        String.concat "\n" ["[Signature]"; name; args_string]
    | FunctionImplementation(fn_sig, exp) ->
        (string_of_ast (FunctionSignature fn_sig)) ^ "[Implementation]\n" ^ (string_of_expression exp)
