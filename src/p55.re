include Binary_tree;

let child_counts = (n) => {
  if (n mod 2 == 0) {
    (n / 2, n / 2);
  } else {
    (n / 2 + 1, n / 2);
  }
};

let rec cbal_tree = (n) => {
  if (n == 0) {
    [Empty];
  } else {
    let (cnt1, cnt2) = child_counts(n - 1);
    if (cnt1 == cnt2) {
      sub_trees(cnt1, cnt1);
    } else {
      List.append(
        sub_trees(cnt1, cnt2),
        sub_trees(cnt2, cnt1)
      );
    }
  }
}
and sub_trees = (cnt1, cnt2) => {
  List.concat(
    List.map(
      (left) => {
        List.map(
          (right) => Node("x", left, right),
          cbal_tree(cnt2)
        );
      },
      cbal_tree(cnt1)
    )
  )
};

let () = {
  cbal_tree(4) |> Js.log;
  List.length(cbal_tree(4)) |> Js.log;
};
