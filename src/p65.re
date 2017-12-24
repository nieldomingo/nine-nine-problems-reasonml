include Binary_tree;

let layout_binary_tree_2 = (tree) => {
  let rec depth = (tree) => {
    switch tree {
    | Empty => 0
    | Node(_, Empty, Empty) => 0
    | Node(_, left, right) => 1 + max(depth(left), depth(right))
    };
  };
  let rec left_most_level = (tree) => {
    switch tree {
    | Empty =>  0
    | Node(_, Empty, _) => 0
    | Node(_, left, _) => 1 + left_most_level(left)
    };
  };
  let rec aux = (tree, xpos, spacing, level) => {
    switch tree {
      | Empty => Empty
      | Node(v, left, right) => Node((v, xpos, level),
          aux(left, xpos - spacing, spacing / 2, level + 1),
          aux(right, xpos + spacing, spacing / 2, level + 1))
    };
  };
  let tree_depth = depth(tree);
  let offset = tree_depth - left_most_level(tree);
  aux(tree, (1 lsl tree_depth) - offset, 1 lsl (tree_depth - 1), 1);
};

let () = {
  let leaf = (x) => Node(x, Empty, Empty);
  let example_layout_tree = Node("n", Node("k", Node("c", leaf("a"),
          Node("e", leaf("d"), leaf("g"))),
        leaf("m")),
      Node("u", Node("p", Empty, leaf("q")), Empty));
  layout_binary_tree_2(example_layout_tree) |> Js.log;
};
