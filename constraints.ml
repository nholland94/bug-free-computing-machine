type t = {
    vars: constraint_var list,
    conds: constraint_condition list }
  and constraint_var = (string, sort)
  and constraint_condition =
      Equal of (int, int)
    | LessThan of (int, int)
    | GreaterThan of (int, int)
    | LessThanEqualTo of (int, int)
    | GreaterThanEqualTo of (int, int)
    | Not of constraint_condition
    | And of (constraint_condition, constraint_condition)
    | Or of (constraint_condition, constraint_condition)

type constraint_state =
      
  and number_range = {
    mutable min: number_range_cap;
    mutable max: number_range_cap }
  and number_range_cap = NonDeterminate | Determinate of int

let apply_constraint(range, cons) =
  match cons with
      ConstantConstraint op ->
        match op with
            Equal n -> begin
                
              {min: n; max: n}
          |
    |


let ranges_of_constraints(constraints) =
  let default_range_of_sort(s) =
    match s with
        Int  -> {min: NonDeterminate; max: NonDeterminate}
        (* TODO size cap to machine's max address size? *)
      | Addr -> {min: Determinate 0; max: NonDeterminate} 
      | Bool -> {min: Determinate 0; max: Determinate 1}
      | Char -> {min: Determinate 0; max: Determinate 255}

  let ranges = List.map(default_range_of_sort, constraints.vars)

  let check_condition(condition) =
    match condition with
        Equal (a, b) ->
      | LessThan (a, b) ->
      | GreaterThan (a, b) ->
      | LessThanEqualTo (a, b) ->
      | GreaterThanEqualTo (a, b) ->
      | Not (sub_condition) ->
      | And (cond_a, cond_b) ->
      | Or (cond_a, cond_b) ->

  let rec check_conditions(conditions) =
    match conditions with
        []                -> []
      | condition :: tail ->
          if !condition_can_be_checked(condition) then
            condition :: check_conditions(tail)
          else begin
            check_condition(condition)
            check_conditions(tail)
          end

  let rec apply_until_empty(fn, ls) =
    match fn(ls) with
        []     -> ()
      | new_ls -> apply_until_empty(fn, new_ls)

  apply_until_empty(check_conditions, constraints.conds)
