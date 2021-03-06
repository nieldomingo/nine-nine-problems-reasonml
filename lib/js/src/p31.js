// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");

function is_prime(n) {
  if (n > 2) {
    var _d = 2;
    while(true) {
      var d = _d;
      if (Caml_int32.imul(d, d) < n) {
        if (Caml_int32.mod_(n, d)) {
          _d = d + 2 | 0;
          continue ;
          
        } else {
          return /* false */0;
        }
      } else {
        return /* true */1;
      }
    };
  } else {
    return /* true */1;
  }
}

console.log(is_prime(1));

console.log(is_prime(2));

console.log(is_prime(7));

console.log(is_prime(8));

console.log(is_prime(31));

exports.is_prime = is_prime;
/*  Not a pure module */
