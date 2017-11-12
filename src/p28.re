let my_sort = (cmp, l) => {
  let rec insert = (e, sorted_list) => {
    switch sorted_list {
    | [] => [e]
    | [h, ...t] =>
      if (cmp(e, h) > 0) {
        [h, ...insert(e, t)];
      } else {
        [e, ...sorted_list];
      }
    }
  };
  let rec loop = (l, accum) => {
    switch l {
    | [] => accum
    | [h, ...t] => loop(t, insert(h, accum))
    }
  };
  loop(l, []);
};

let length_sort = my_sort((a, b) => List.length(a) - List.length(b));

let calc_freq = (l) => {
  let rec aux = (l, cnt, accum) =>
    switch l {
    | [] => accum
    | [a] => [(a, cnt), ...accum]
    | [h, ...[n, ...t]] =>
      if (h == n) {
        aux([n, ...t], cnt + 1, accum);
      } else {
        aux([n, ...t], 0, [(h, cnt), ...accum]);
      }
    };
  aux(l, 0, []);
};

let frequency_sort = (l) => {
  let lengths = my_sort(
    (a, b) => a - b,
    List.map(List.length, l));
  let freqs = calc_freq(lengths);
  let cmp = (a, b) =>
    List.assoc(List.length(a), freqs) -
    List.assoc(List.length(b), freqs);
  my_sort(cmp, l);
};

let () = {
  my_sort((a, b) => a - b, [1, 5, 2, 9]) |> Js.log;
  length_sort([["a", "b", "c"], ["d", "e"], ["f", "g", "h"], ["d", "e"], 
              ["i", "j", "k", "l"], ["m", "n"], ["o"]]) |> Js.log;
  frequency_sort([["a", "b", "c"], ["d", "e"], ["f", "g", "h"], ["d", "e"], 
                 ["i", "j", "k", "l"], ["m", "n"], ["o"]]) |> Js.log;
}
