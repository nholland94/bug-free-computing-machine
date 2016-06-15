{
  open Parser
  exception Eof
}

rule token = parse
  (* whitespace and comments *)
    [' ' '\t' '\n'] { token lexbuf }
  | "//" [^ '\n']*  { token lexbuf }
  | "/*" .* "*/"    { token lexbuf }

  (* symbols *)
  | '('  { LEFT_PAREN }
  | ')'  { RIGHT_PAREN }
  | '{'  { LEFT_BRACE }
  | '}'  { RIGHT_BRACE }
  | ':'  { COLON }
  | "::" { DOUBLE_COLON }
  | ';'  { SEMICOLON }
  | ','  { COMMA }
  | '='  { EQUALS }
  | '|'  { VERTICAL_BAR }

  | ['+' '-' '/' '*' '&' '^' '%']
  | ['=' '+' '-' '/' '*' '|' '&' '^' '%']
    ['=' '+' '-' '/' '*' '|' '&' '^' '%']
        { INFIX_OPERATOR(Lexing.lexeme lexbuf) }

  (* keywords *)
  | "fn" | "fun" { KEYWORD_FUN }
  | "val"        { KEYWORD_VAL }
  | "let"        { KEYWORD_LET }
  | "in"         { KEYWORD_IN }
  | "if"         { KEYWORD_IF }
  | "then"       { KEYWORD_THEN }
  | "else"       { KEYWORD_ELSE }
  | "while"      { KEYWORD_WHILE }
  | "do"         { KEYWORD_DO }
  | "begin"      { KEYWORD_BEGIN }
  | "end"        { KEYWORD_END }
  | "extern"     { KEYWORD_EXTERN }
  | "implement"  { KEYWORD_IMPLEMENT }

  (* non static lexical components *)
  | '-' [0-9]*                                       { INTEGER(Lexing.lexeme lexbuf) }
  | '"' ( ('\' . | [^'"']) as string_body ) '"'      { STRING(string_body)
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* { ATOM(Lexing.lexeme lexbuf) }
