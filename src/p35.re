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
  let rec aux = (x, p, accum) => {
    if (x == 1) {
      accum;
    } else {
      if (x mod p == 0) {
        aux(x / p, p, [p, ...accum]);
      } else {
        aux(x, next_prime(p), accum);
      }
    }
  };
  List.rev(aux(x, 2, []));
};

let () = {
  factors(315) |> Js.log;
};
