let is_prime = (n) => {
  let rec loop = (d) =>
    if (d * d < n) {
      if (n mod d == 0) {
        false;
      } else {
        loop(d+2);
      }
    } else {
      true;
    };

  if (n > 2) {
    loop(2);
  } else {
    true;
  }
};

let factors = (x) => {
  let next_prime = (p) => {
    let rec loop = (c) =>
      if (is_prime(c)) {
        c;
      } else {
        loop(c + 1);
      };
    loop(p + 1);
  };
  let rec aux = (x, p, cnt, accum) => {
    if (x == 1) {
      [(p, cnt), ...accum];
    } else {
      if (x mod p == 0) {
        aux(x / p, p, cnt + 1, accum);
      } else {
        aux(x, next_prime(p), 0, (cnt > 0) ? [(p, cnt), ...accum] : accum);
      }
    }
  };
  aux(x, 2, 0, []);
};

let pow = (x, y) => {
  let rec aux = (x, y, accum) =>
    if (y == 0) {
      accum;
    } else {
      if (y mod 2 == 0) {
        aux(x * x, y / 2, accum);
      } else {
        aux(x * x, y / 2, accum * x);
      }
    };
  aux(x, y, 1);
};

let phi_improved = (x) => {
  let rec loop = (l, accum) => {
    switch l {
    | [] => accum
    | [h, ...t] => {
        let (p, n) = h;
        loop(t, (p - 1) * pow(p, (n - 1)) * accum);
      }
    };
  };
  loop(factors(x), 1);
};

let () = {
  phi_improved(10) |> Js.log;
  phi_improved(13) |> Js.log;
};

