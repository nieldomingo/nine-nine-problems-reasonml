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

let goldbach = (n) => {
  let rec loop = (ascending, descending) => {
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
            loop(ta, descending);
          } else {
            loop(ascending, td);
          }
        }
      }
    };
  };
  let primes = all_primes(2, n);
  loop(primes, List.rev(primes));
};

let () = {
  goldbach(28) |> Js.log;
}
