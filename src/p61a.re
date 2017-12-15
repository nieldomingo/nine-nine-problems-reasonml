include Binary_tree;

let leaves = (tree) => {
  let rec traverse = (tree, accum) => {
    switch tree {
    | Empty => accum
    | Node(v, Empty, Empty) => [v, ...accum]
    | Node(_, left, right) => traverse(left, traverse(right, accum))
    };
  };
  traverse(tree, []);
};

let () = {
  let complex_balanced_tree = 
    Node(
      "x",
      Node(
        "x",
        Node("x", Empty, Empty),
        Empty),
      Node(
        "x",
        Empty,
        Node("x", Empty, Empty))
    );
  leaves(complex_balanced_tree) |> Js.log;
};
