include Mult_tree;

let rec count_nodes = (tree) => {
  switch tree {
  | T(_, []) => 1
  | T(_, children) =>
    List.fold_left(
      (a, b) => a + b,
      1,
      List.map(
        (child) => count_nodes(child),
        children));
  };
};

let () = {
  count_nodes(T('a', [T('f',[])])) |> Js.log;
};
