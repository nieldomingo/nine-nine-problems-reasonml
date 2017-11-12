let rec gcd = (x, y) => 
  switch y {
  | 0 => x
  | y => gcd(y, x mod y)
  };

let phi = (x) => {
  let rec aux = (r, cnt) =>
    switch r {
    | 1 => cnt + 1
    | r =>
      if (gcd(x, r) == 1) {
        aux(r - 1, cnt + 1);
      } else {
        aux(r - 1, cnt);
      }
    };
  aux(x - 1, 0);
};

let () = {
  phi(10) |> Js.log;
  phi(13) |> Js.log;
};

