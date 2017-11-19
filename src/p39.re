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

let () = {
  List.length(all_primes(2, 7920)) |> Js.log;
};
