%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token COLON
%token DOUBLE_COLON
%token SEMICOLON
%token COMMA
%token EQUALS
%token VERTICAL_BAR

%token KEYWORD_FUN
%token KEYWORD_VAL
%token KEYWORD_LET
%token KEYWORD_IN
%token KEYWORD_IF
%token KEYWORD_THEN
%token KEYWORD_ELSE
%token KEYWORD_WHILE
%token KEYWORD_DO
%token KEYWORD_BEGIN
%token KEYWORD_END
%token KEYWORD_EXTERN
%token KEYWORD_IMPLEMENT

%token <string> ATOM
%token <string> INTEGER
%token <string> STRING
%token <string> INFIX_OPERATOR

%start root
%type <Ast.t> root

%%

root:
    fn_impl       { $1 }
  | fn_def        { $1 }
  | fn_def_impl   { $1 }
//   | fn_extern_def { $1 }
;

untyped_args:
    ATOM COMMA untyped_args { $1 :: $3 }
  | ATOM                    { [$1] }
;

// temp
view_def: ATOM { $1 }

typed_args:
    ATOM COLON view_def COMMA typed_args { ($1, $3) :: $5 }
  | ATOM COLON view_def                  { [($1, $3)] }
;

// optionally_typed_args:
//     untyped_args { $1 }
//   | typed_args   { $1 }
// ;

infix_operator:
    INFIX_OPERATOR { $1 }
  | VERTICAL_BAR { "|" }
;

exp:
    function_call { $1 }
  | constant_exp  { $1 }
  | array_exp     { $1 }
  | list_exp      { $1 }
  | assignment    { $1 }
  | while_exp     { $1 }
  | if_exp        { $1 }
  | LEFT_BRACE exp_sequence RIGHT_BRACE { Ast.BlockExp $2 }
  | LEFT_PAREN RIGHT_PAREN              { Ast.VoidExp }
  | ATOM                                { Ast.VariableReference $1 }
;

exp_sequence:
    exp SEMICOLON exp_sequence { $1 :: $3 }
  | exp SEMICOLON              { $1 :: [Ast.VoidExp] }
  | exp                        { [$1] }
;

exp_list:
    exp COMMA exp_list { $1 :: $3 }
  | exp                { [$1] }
;

bar_joined_exp_list:
    exp_list VERTICAL_BAR bar_joined_exp_list { $1 :: $3 }
  | exp_list                                   { [$1] }
;

double_colon_joined_exp_list:
    exp_list DOUBLE_COLON double_colon_joined_exp_list { $1 :: $3 }
  | exp_list                                            { [$1] }
;

function_call:
    ATOM LEFT_PAREN exp_list RIGHT_PAREN { Ast.FunctionCall ($1, $3) }
  | exp infix_operator exp               { Ast.FunctionCall ($2, $1 :: [$3]) }
;

constant_exp:
    INTEGER { Ast.ConstantInteger (int_of_string $1) }
  | STRING  { Ast.ConstantString  $1 }
;

array_exp:
    LEFT_BRACE RIGHT_BRACE                  { Ast.ArrayExp [] }
  | LEFT_BRACE exp_list RIGHT_BRACE         { Ast.ArrayExp $2 }
  | LEFT_BRACE bar_joined_exp_list RIGHT_BRACE
      { Ast.ArrayExp (List.map (fun ls -> Ast.ArrayExp ls) $2) }
;

list_exp:
    LEFT_BRACKET RIGHT_BRACKET                              { Ast.ListExp [] }
  | LEFT_BRACKET exp_list RIGHT_BRACKET                     { Ast.ListExp $2 }
  | LEFT_BRACKET double_colon_joined_exp_list RIGHT_BRACKET
      { Ast.ListExp (List.map (fun ls -> Ast.ArrayExp ls) $2) }
;

pattern_matching_assignment:
    array_exp EQUALS exp    { Ast.ArrayMatchingAssignment ($1, $3) }
  | list_exp EQUALS exp     { Ast.ListMatchingAssignment ($1, $3) }
  | constant_exp EQUALS exp { Ast.ConstantMatchingAssignment ($1, $3) }
;

assignment:
    ATOM EQUALS exp             { Ast.DirectAssignment ($1, $3) }
  | pattern_matching_assignment { $1 }
;


while_exp:
    KEYWORD_WHILE pattern_matching_assignment KEYWORD_DO exp { Ast.WhileExp ($2, $4) }
;

if_exp:
    KEYWORD_IF exp KEYWORD_THEN exp KEYWORD_ELSE exp
        { Ast.IfExp ($2, $4, $6) }
  | KEYWORD_IF exp KEYWORD_THEN exp
        { Ast.IfExp ($2, $4, Ast.VoidExp) }
;

fn_impl:
    KEYWORD_IMPLEMENT ATOM LEFT_PAREN untyped_args RIGHT_PAREN EQUALS exp
        { Ast.FunctionImplementation (($2, Ast.Untyped $4), $7) }

  | KEYWORD_IMPLEMENT ATOM LEFT_PAREN typed_args RIGHT_PAREN EQUALS exp
        { Ast.FunctionImplementation (($2, Ast.Typed $4), $7) }
;

fn_def:
    KEYWORD_FUN ATOM LEFT_PAREN typed_args RIGHT_PAREN { Ast.FunctionSignature ($2, Ast.Typed $4) }
;

fn_def_impl:
    KEYWORD_FUN ATOM LEFT_PAREN typed_args RIGHT_PAREN EQUALS exp
        { Ast.FunctionImplementation (($2, Ast.Typed $4), $7) }
;
