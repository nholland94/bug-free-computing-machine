let constraint_range_tree_of_constraint_condition cond =
  let lefthand_variable cond =
    match cond with
        Equal (Constant a, Reference b) ->
          Equal (Reference b, Constant a)
      | LessThan (Constant a, Reference b) ->
          LessThan (Reference b, Constant a)
      | GreaterThan (Constant a, Reference b) ->
          GreaterThan (Reference b, Constant a)
      | LessThanEqualTo (Constant a, Reference b) ->
          LessThanEqualTo (Reference b, Constant a)
      | GreaterThanEqualTo (Constant a, Reference b) ->
          GreaterThanEqualTo (Reference b, Constant a)
      | other_cond -> other_cond
  in

  let max_range_cap a, b =
    match (a, b) with
        (NonDeterminate _, Determinate r)
      | (Determinate r, NonDeterminate _) -> Determinate r
      | (Determinate a, Determinate b)    -> Determinate (max a b)
      | (NonDeterminate, NonDeterminate)  -> NonDeterminate
  in

  let min_range_cap a, b =
    match (a, b) with
        (NonDeterminate _, Determinate r)
      | (Determinate r, NonDeterminate _) -> Determinate r
      | (Determinate a, Determinate b)    -> Determinate (min a b)
      | (NonDeterminate, NonDeterminate)  -> NonDeterminate
  in

  match lefthand_variable cond with
      Not inner_cond     ->
        Not (range_tree_of_range inner_cond)
    | And (cond1, cond2) ->
        And (range_tree_of_range cond1, range_tree_of_range cond2)
    | Or (cond1, cond2)  ->
        Or (range_tree_of_range cond1, range_tree_of_range cond2)

    | Equal (_, const)       ->
        if in_range(range, const) then
          {min: Determinate const; max: Determinate const}
        else
          raise (InvalidConstraint cond)

    | LessThan (_, const)    ->
        let new_range = {
          min: range.min;
          max: min_range_cap range.max (Determinate (const - 1))}
        in
        if range_is_valid(new_range) then new_range else
          raise (InvalidConstraint cond)

    | GreaterThan (_, const) ->
        let new_range = {
          min: max_range_cap range.min (Determinate (const + 1));
          max: range.max}
        in
        if range_is_valid(new_range) then new_range else
          raise (InvalidConstraint cond)

    | LessThanEqualTo (_, const) ->
        let new_range = {
          min: range.min;
          max: min_range_cap range.max (Determinate (const))}
        in
        if range_is_valid(new_range) then new_range else
          raise (InvalidConstraint cond)

    | GreaterThanEqualTo (_, const) ->
        let new_range = {
          min: max_range_cap range.min (Determinate (const));
          max: range.max}
        in
        if range_is_valid(new_range) then new_range else
          raise (InvalidConstraint cond)

let variable_constraint_range_trees_of_constraint constr =
  let 
  let base_trees = List.map 

let constraint_condition_is_constant cond =
  let only_one(fn: 'a -> bool, ls: 'a list): bool =
    let loop(ls, one_matched) =
      match ls with
          head :: tail ->
            if fn head then !one_matched && loop tail, true else loop tail, false
        | []           ->
            one_matched
    in loop ls, false
  in

  let constraint_value_is_constant(value) =
      Reference _ -> false
    | Constant _  -> true
  in

  match cond with
      Equal (a, b)
    | LessThan (a, b)
    | GreaterThan (a, b)
    | LessThanEqualTo (a, b)
    | GreaterThanEqualTo (a, b) ->
        only_one constraint_value_is_constant [a; b]

    | Not c                    ->
        constraint_condition_is_constant c

    | And (c1, c2)
    | Or (c1, c2)               ->
        (constraint_condition_is_constant c1) && (constraint_condition_is_constant c2)
        (* List.fold_left (&&) (List.map constraint_is_constant [c1, c2]) *)

let categorize_constraints(constraints) =
  let loop(constraints, constant_constraints, variable_constraints) =
    match constraints with
        constr :: tail ->
          if constraint_is_constant(constr) then
            loop(constraints, constr :: constant_constraints, variable_constraints)
          else
            loop(constraints, constant_constraints, constr :: variable_constraints)
      | [] -> (constant_constraints, variable_constraints)
  in loop(constraints, [], [])

let (constraints) =
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
      | Not sub_condition ->
      | And (cond_a, cond_b) ->
      | Or (cond_a, cond_b) ->

  let rec check_conditions(conditions) =
    match conditions with
        condition :: tail ->
          if !condition_can_be_checked(condition) then
            condition :: check_conditions(tail)
          else begin
            check_condition(condition);
            check_conditions(tail)
          end
      | []                -> []

  let rec apply_until_empty(fn, ls) =
    match fn(ls) with
        []     -> ()
      | new_ls -> apply_until_empty(fn, new_ls)

  apply_until_empty(check_conditions, constraints.conds)
