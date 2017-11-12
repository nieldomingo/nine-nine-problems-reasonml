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

let () = {
  is_prime(1) |> Js.log;
  is_prime(2) |> Js.log;
  is_prime(7) |> Js.log;
  is_prime(8) |> Js.log;
  is_prime(31) |> Js.log;
};
