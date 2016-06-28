let read_ast_from_channel(input) =
  try
    let lexbuf = Lexing.from_channel input in
    Parser.root Lexer.token lexbuf
  with
    | Lexer.Eof -> begin
      try
        while true do
          print_char @@ input_char stdin
        done
      with
        End_of_file -> exit 0;
      VoidExp
    end


let _ =
  let ast = read_ast_from_channel stdin in
  print_string @@ Ast.string_of_ast ast
