include Binary_tree;

let rec is_mirror = (left, right) => {
  switch (left, right) {
  | (Empty, Empty) => true
  | (
      Node(_, left_left, left_right),
      Node(_, right_left, right_right)
    ) => is_mirror(left_left, right_right) &&
      is_mirror(left_right, right_left)
  | _ => false
  };
};

let is_symmetric = (t) => {
  switch t {
  | Empty => true
  | Node(_, left, right) => is_mirror(left, right)
  };
};

let () = {
  let simple_balanced_tree = 
    Node('x',
      Node('x', Empty, Empty),
      Node('x', Empty, Empty)
    );

  is_symmetric(simple_balanced_tree) |> Js.log;
  
  let simple_unbalanced_tree = 
    Node('x',
      Node('x', Empty, Empty),
      Empty
    );
  
  is_symmetric(simple_unbalanced_tree) |> Js.log;
  
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
  is_symmetric(complex_balanced_tree) |> Js.log;
  
  let complex_unbalanced_tree = 
    Node(
      'x',
      Node(
        'x',
        Node('x', Empty, Empty),
        Empty),
      Node(
        'x',
        Node('x', Empty, Empty),
        Node('x', Empty, Empty))
    );
  is_symmetric(complex_unbalanced_tree) |> Js.log;
};
