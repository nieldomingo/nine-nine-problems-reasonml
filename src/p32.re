let rec gcd = (x, y) => 
  switch y {
  | 0 => x
  | y => gcd(y, x mod y)
  };

let () = {
  gcd(13, 27) |> Js.log;
  gcd(20536, 7826) |> Js.log;
  gcd(105, 25) |> Js.log;
};
