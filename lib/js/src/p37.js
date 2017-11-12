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

function factors(x) {
  var next_prime = function (p) {
    var _c = p + 1 | 0;
    while(true) {
      var c = _c;
      if (is_prime(c)) {
        return c;
      } else {
        _c = c + 1 | 0;
        continue ;
        
      }
    };
  };
  var _x = x;
  var _p = 2;
  var _cnt = 0;
  var _accum = /* [] */0;
  while(true) {
    var accum = _accum;
    var cnt = _cnt;
    var p = _p;
    var x$1 = _x;
    if (x$1 === 1) {
      return /* :: */[
              /* tuple */[
                p,
                cnt
              ],
              accum
            ];
    } else if (Caml_int32.mod_(x$1, p)) {
      var match = +(cnt > 0);
      _accum = match !== 0 ? /* :: */[
          /* tuple */[
            p,
            cnt
          ],
          accum
        ] : accum;
      _cnt = 0;
      _p = next_prime(p);
      continue ;
      
    } else {
      _cnt = cnt + 1 | 0;
      _x = Caml_int32.div(x$1, p);
      continue ;
      
    }
  };
}

function pow(x, y) {
  var _x = x;
  var _y = y;
  var _accum = 1;
  while(true) {
    var accum = _accum;
    var y$1 = _y;
    var x$1 = _x;
    if (y$1) {
      if (y$1 % 2) {
        _accum = Caml_int32.imul(accum, x$1);
        _y = y$1 / 2 | 0;
        _x = Caml_int32.imul(x$1, x$1);
        continue ;
        
      } else {
        _y = y$1 / 2 | 0;
        _x = Caml_int32.imul(x$1, x$1);
        continue ;
        
      }
    } else {
      return accum;
    }
  };
}

function phi_improved(x) {
  var _l = factors(x);
  var _accum = 1;
  while(true) {
    var accum = _accum;
    var l = _l;
    if (l) {
      var h = l[0];
      var p = h[0];
      _accum = Caml_int32.imul(Caml_int32.imul(p - 1 | 0, pow(p, h[1] - 1 | 0)), accum);
      _l = l[1];
      continue ;
      
    } else {
      return accum;
    }
  };
}

console.log(phi_improved(10));

console.log(phi_improved(13));

exports.is_prime     = is_prime;
exports.factors      = factors;
exports.pow          = pow;
exports.phi_improved = phi_improved;
/*  Not a pure module */