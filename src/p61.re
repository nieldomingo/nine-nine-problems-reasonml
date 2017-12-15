include Binary_tree;

let rec count_leaves = (tree) => {
  switch tree {
  | Empty => 0
  | Node(_, Empty, Empty) => 1
  | Node(_, left, right) => count_leaves(left) + count_leaves(right)
  };
};

let () = {
  let complex_balanced_tree = 
    Node(
      'x',
      Node(
        'x',
        Node('x', Empty, Empty),
        Empty),
      Node(
        'x',
        Empty,
        Node('x', Empty, Empty))
    );
  count_leaves(complex_balanced_tree) |> Js.log;
};
