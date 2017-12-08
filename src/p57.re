include Binary_tree;

let construct = (nums) => {
  let rec insert = (tree, num) => {
    switch tree {
    | Empty =>  Node(num, Empty, Empty)
    | Node(v, l, r) => 
      if (num < v) {
        Node(v, insert(l, num), r)
      } else {
        Node(v, l, insert(r, num))
      }
    };
  };
  List.fold_left(
    (tree, num) => insert(tree, num),
    Empty,
    nums);
};

let () = {
  construct([3, 2, 5, 7, 1]) |> Js.log;
  construct([5, 3, 18, 1, 4, 12, 21]) |> Js.log;
};
