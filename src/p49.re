let rec gray = (n) => {
  if (n == 1) {
    ["0", "1"];
  } else {
    let base_bits = gray(n - 1);
    List.append(
      List.map(
        (pat) => "0" ++ pat,
        base_bits
      ),
      List.rev_map(
        (pat) => "1" ++ pat,
        base_bits
      )
    );
  }
};

let () = {
  gray(1) |> Js.log;
  gray(2) |> Js.log;
  gray(3) |> Js.log;
};
