type boolExpr =
  | Var(string)
  | Not(boolExpr)
  | And(boolExpr, boolExpr)
  | Or(boolExpr, boolExpr);

let rec eval = (expr, env) =>
  switch expr {
  | Var(var_name) => List.assoc(var_name, env)
  | Not(sub_expr) => !eval(sub_expr, env)
  | And(sub_expr1, sub_expr2) => eval(sub_expr1, env) && eval(sub_expr2, env)
  | Or(sub_expr1, sub_expr2) => eval(sub_expr1, env) || eval(sub_expr2, env)
  };

let table2 = (var1, var2, expr) => {
  let rec create_input_table = (n, input_table) =>
    switch n {
    | 1 => input_table
    | n =>
      create_input_table(
        n - 1,
        List.append(
          List.map(
            (vals) => [true, ...vals],
            input_table
          ),
          List.map(
            (vals) => [false, ...vals],
            input_table
          )
        )
      )
    };
  let create_env = (labels, vals) => List.combine(labels, vals);
  List.map(
    (vals) =>
      (
        List.nth(vals, 0),
        List.nth(vals, 1),
        eval(expr, create_env([var1, var2], vals))
      ),
    create_input_table(2, [[true], [false]])
  );
};

let () = {
  table2("a", "b", And(Var("a"), Or(Var("a"), Var("b")))) |> Js.log;
};
