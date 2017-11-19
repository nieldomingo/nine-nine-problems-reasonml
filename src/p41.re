let all_primes = (a, b) => {
  let rec not_divisible_by = (n, divisors) => {
    switch divisors {
    | [] => true
    | [h, ...t] => (n mod h == 0) ? false : not_divisible_by(n, t)
    };
  };
  let rec loop = (n, accum) => {
    if (n > b) {
      accum;
    } else if (n == 2) {
      loop(n + 1, [n, ...accum]);
    } else if (n == 3) {
      loop(n + 2, [n, ...accum]);
    } else {
      if (not_divisible_by(n, accum)) {
        loop(n + 2, [n, ...accum]);
      } else {
        loop(n + 2, accum);
      }
    }
  };
  let rec filter = (l, accum) =>
    switch l {
    | [] => accum
    | [h, ...t] =>
      if (a <= h && h <= b) {
        filter(t, [h, ...accum]);
      } else {
        filter(t, accum);
      }
    };
  filter(loop(2, []), []);
};

let goldbach_list = (lo, hi) => {
  let rec find_pair = (ascending, descending, n) => {
    switch (ascending, descending) {
    | ([], _) => raise(Not_found)
    | (_, []) => raise(Not_found)
    | ([ha, ...ta], [hd, ...td]) =>
      if (ha + hd == n) {
        (ha, hd);
      } else {
        if (ha > hd) {
          raise(Not_found);
        } else {
          if (ha + hd < n) {
            find_pair(ta, descending, n);
          } else {
            find_pair(ascending, td, n);
          }
        }
      }
    };
  };
  let ascending = all_primes(2, hi);
  let descending = List.rev(ascending);
  let rec loop = (hi, accum) => {
    if (hi < lo) {
      accum;
    } else {
      if ((hi mod 2 == 0) && (hi > 2)) {
        loop(
          hi - 1,
          [(hi, find_pair(ascending, descending, hi)), ...accum]
        );
      } else {
        loop(
          hi - 1,
          accum
        );
      }
    }
  };
  loop(hi, []);
};

let goldbach_limit = (lo, hi, limit) => {
  List.filter(
    (e) => {
      let (n, (a, b)) = e;
      if ((a > limit) && (b > limit)) {
        true;
      } else {
        false;
      }
    },
    goldbach_list(lo, hi)
  );
};

let () = {
  goldbach_list(9, 20) |> Js.log;
  goldbach_list(1, 2000) |> Js.log;
  goldbach_limit(1, 2000, 50) |> Js.log;
}
