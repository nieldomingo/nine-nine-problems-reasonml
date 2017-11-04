let rev = (l) => {
  let rec rev_inner = (l, acc) =>
    switch l {
    | [] => acc
    | [a, ...rest] => rev_inner(rest, [a, ...acc])
    };
  rev_inner(l, [])
};

let is_palindrome = (l) => {
  let rec is_equal = (l, r) =>
    switch (l, r) {
    | ([], []) => true
    | ([_, ..._], []) => false
    | ([], [_, ..._]) => false
    | ([a, ...arest], [b, ...brest]) => a == b ? is_equal(arest, brest) : false
    };
  is_equal(l, rev(l))
};

let () = {
  is_palindrome([1, 2, 3, 4, 3, 2, 1]) |> Js.log;
  is_palindrome([1, 2, 3]) |> Js.log;
  is_palindrome([]) |> Js.log
};
