let rec gcd = (x, y) => 
  switch y {
  | 0 => x
  | y => gcd(y, x mod y)
  };

let coprime = (x, y) =>
  gcd(x, y) == 1;

let () = {
  coprime(13, 27) |> Js.log;
  coprime(20536, 7826) |> Js.log;
  coprime(105, 25) |> Js.log;
};

