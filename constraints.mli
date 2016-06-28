type 'a pair = 'a * 'a

val add : int pair -> int

type t = {
  vars: constraint_var list,
  conds: constraint_condition list }

type constraint_var = string * sort

type constraint_value =
    Reference of int (* index of var *)
  | Constant of int

type constraint_condition =
    Equal of constraint_value * constraint_value
  | LessThan of constraint_value * constraint_value
  | GreaterThan of constraint_value * constraint_value
  | LessThanEqualTo of constraint_value * constraint_value
  | GreaterThanEqualTo of constraint_value * constraint_value
  | Not of constraint_condition
  | And of constraint_condition * constraint_condition
  | Or of constraint_condition * constraint_condition

type constraint_range = {
    mutable min: number_range_cap;
    mutable max: number_range_cap }
  and number_range_cap = NonDeterminate | Determinate of int

type constraint_range_tree =
    Not   of range_tree
  | And   of range_tree * range_tree
  | Or    of range_tree * range_tree
  | Range of constraint_range

val constraint_range_tree_of_constraint_condition : constraint_condition -> constraint_range_tree
val variable_constraint_range_trees_of_constraint : t -> constraint_range_tree list
val constraint_is_constant : t -> bool
val categorize_constraints : t -> t list * t list
