include Binary_tree;

let at_level = (tree, desired_level) => {
  let rec traverse = (tree, level, accum) => {
    switch tree {
    | Empty =>  accum
    | Node(v, left, right) =>
      if (level == desired_level) {
        [v, ...accum];
      } else {
        traverse(left, level + 1, traverse(right, level + 1, accum));
      }
    };
  };
  traverse(tree, 1, []);
};

let () = {
  let example_tree =
    Node("a", Node("b", Node("d", Empty, Empty), Node("e", Empty, Empty)),
      Node("c", Empty, Node("f", Node("g", Empty, Empty), Empty)));
  at_level(example_tree, 2) |> Js.log;
};
