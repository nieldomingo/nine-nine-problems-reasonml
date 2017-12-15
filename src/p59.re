include Binary_tree;

let gen_trees = (left_sub_trees, right_sub_trees) => {
  List.fold_left(
    (accum, left_sub_tree) => {
      List.fold_left(
        (accum, right_sub_tree) => {
          [Node('x', left_sub_tree, right_sub_tree), ...accum];
        },
        accum,
        right_sub_trees);
    },
    [],
    left_sub_trees);
};

let rec hbal_tree = (n) => {
  switch n {
  | 0 => [Empty]
  | 1 => [Node('x', Empty, Empty)]
  | n => 
    if (n < 0) {
      raise(Arg.Bad("Invalid argument value."));
    } else {
      let n = n - 1;
      List.flatten(
        [
          gen_trees(
            hbal_tree(n),
            hbal_tree(n)),
          gen_trees(
            hbal_tree(n),
            hbal_tree(n-1)),
          gen_trees(
            hbal_tree(n-1),
            hbal_tree(n)),
        ]);
    }
  };
};

let () = {
  hbal_tree(0) |> Js.log;
  hbal_tree(1) |> Js.log;
  List.length(hbal_tree(3)) |> Js.log;
};
