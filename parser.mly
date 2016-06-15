%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token COLON
%token DOUBLE_COLON
%token SEMICOLON
%token COMMA
%token EQUALS
%token VERTICAL_BAR
%token INFIX_OPERATOR

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

%start root
%type <Ast> root

%%

root:
    fn_impl       { $1 }
  | fn_def        { $1 }
  | fn_def_impl   { $1 }
  | fn_extern_def { $1 }
;

untyped_args:
    ATOM COMMA untyped_args { $1 :: $3 }
  | ATOM                    { $1 }
;

typed_args:
    ATOM COLON view_def COMMA typed_args { ($1, $3) :: $5 }
  | ATOM COLON view_def                  { ($1, $3) }
;

optionally_typed_args:
    untyped_args { $1 }
  | typed_args   { $1 }
;

infix_operator:
    INFIX_OPERATOR | VERTICAL_BAR { $1 }
;

exp:
    function_call | constant_exp
  | array_exp | list_exp | assignment
  | while_exp | if_exp                  { $1 }
  | LEFT_BRACE exp_sequence RIGHT_BRACE { BlockExp $2 }
  | LEFT_PAREN RIGHT_PAREN              { VoidExp }
  | ATOM                                { VariableReference $1 }
;

exp_sequence:
    exp SEMICOLON exp_sequence { $1 :: $3 }
  | exp SEMICOLON              { $1 :: VoidExp }
  | exp                        { $1 }
;

exp_list:
    exp COMMA exp_list { $1 :: $3 }
  | exp                { $1 }
;

bar_joined_exp_lists:
    exp_list VERTICAL_BAR joined_exp_lists { $1 :: $3 }
  | exp_list                               { $1 }
;

double_colon_joined_exp_list:
    exp_list DOUBLE_COLON joined_exp_lists { $1 :: $3 }
  | exp_list                               { $1 }
;

function_call:
    ATOM LEFT_PAREN exp_list RIGHT_PAREN { FunctionCall ($1, $3) }
  | exp infix_operator exp               { FunctionCall ((string_of_infix_operator $2), $1 :: $3) }
;

constant_exp:
    INTEGER { ConstantInteger $1 }
  | STRING  { ConstantString  $1 }
;

array_exp:
    LEFT_BRACE RIGHT_BRACE                  { ArrayExp [] }
  | LEFT_BRACE exp_list RIGHT_BRACE         { ArrayExp $2 }
  | LEFT_BRACE bar_joined_exp_lists RIGHT_BRACE { ArrayExp $2 }
;

list_exp:
    LEFT_BRACKET RIGHT_BRACKET                              { ListExp [] }
  | LEFT_BRACKET exp_list RIGHT_BRACKET                     { ListExp $2 }
  | LEFT_BRACKET double_colon_joined_exp_list RIGHT_BRACKET { ListExp $2 }
;

pattern_matching_assignment:
    array_exp EQUALS exp    { ArrayMatchingAssignment ($1, $3) }
  | list_exp EQUALS exp     { ListMatchingAssignment ($1, $3) }
  | constant_exp EQUALS exp { ConstantMatchingAssignment ($1, $3) }
;

assignment:
    ATOM EQUALS exp             { DirectAssignment ($1, $3) }
  | pattern_matching_assignment { $1 }
;


while_exp:
    KEYWORD_WHILE pattern_matching_assignment KEYWORD_DO exp { WhileExp ($2, $4) }
;

if_exp:
    KEYWORD_IF exp KEYWORD_THEN exp KEYWORD_ELSE exp
        { IfExp ($2, $4, $6) }
  | KEYWORD_IF exp KEYWORD_THEN exp
        { IfExp ($2, $4, VoidExp) }
;

fn_impl:
    KEYWORD_IMPLEMENT ATOM LEFT_PAREN optionally_typed_args RIGHT_PAREN EQUALS exp
        { FunctionImplementation (FunctionSignature ($2, $4), $6) }
;

fn_def:
    KEYWORD_FUN ATOM LEFT_PAREN typed_args RIGHT_PAREN { FunctionSignature ($2, $4) }
;

fn_def_impl:
    KEYWORD_FUN ATOM LEFT_PAREN typed_args RIGHT_PAREN EQUALS exp
        { FunctionImplementation (FunctionSignature ($2, $4), $6) }
;
