include Binary_tree;

let rec skip = (l, n) => {
  if (n == 1) {
    l;
  } else {
    switch l {
    | [] => []
    | [_, ...t] => skip(t, n - 1)
    };
  }
};

let complete_binary_tree = (elements) => {
  let rec aux = (elements, n) => {
    switch elements {
    | [] => Empty
    | [h, ..._] => {
      let left_children = skip(elements, 2 * n - n + 1);
      let right_children = skip(elements, 2 * n - n + 2);
      Node(
        h,
        aux(left_children, n * 2),
        aux(right_children, n * 2 + 1)
      )
    }
    };
  };
  aux(elements, 1);
};

let () = {
  complete_binary_tree([1, 2, 3, 4, 5, 6]) |> Js.log;
}
