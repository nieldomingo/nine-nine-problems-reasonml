include Binary_tree;

let layout_binary_tree_1 = (tree) => {
  let rec aux = (tree, index, level) => {
    switch tree {
    | Empty => (index, Empty)
    | Node(v, left, right) => {
        let (left_current_index, new_left) = aux(left, index, level + 1);
        let (right_current_index, new_right) = aux(
          right, left_current_index + 1, level + 1);
        (
          right_current_index,
          Node(
            (v, left_current_index + 1, level),
            new_left,
            new_right
          )
        );
      }
    };
  };
  let (_, new_tree) = aux(tree, 0, 1);
  new_tree;
};

let () = {
  let leaf = (x) => Node(x, Empty, Empty);
  let example_layout_tree = Node("n",
    Node("k",
      Node("c",
        leaf("a"),
        Node("h",
          Node("g",
            leaf("e"),
            Empty
          ),
          Empty
        )
      ),
      leaf("m")
    ),
    Node("u",
      Node("p",
        Empty,
        Node("s",
          leaf("q"),
          Empty
        )
      ),
      Empty
    )
  );
  layout_binary_tree_1(example_layout_tree) |> Js.log;
};
