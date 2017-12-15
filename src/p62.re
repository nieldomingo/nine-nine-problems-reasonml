include Binary_tree;

let internals = (tree) => {
  let rec traverse = (tree, accum) => {
    switch tree {
    | Empty => accum
    | Node(_, Empty, Empty) => accum
    | Node(v, left, right) => traverse(left, traverse(right, [v, ...accum]))
    };
  };
  traverse(tree, []);
};

let () = {
  let example_tree =
    Node("a", Node("b", Node("d", Empty, Empty), Node("e", Empty, Empty)),
      Node("c", Empty, Node("f", Node("g", Empty, Empty), Empty)));
  internals(example_tree) |> Js.log;
};
