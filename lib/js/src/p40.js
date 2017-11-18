// Generated by BUCKLESCRIPT VERSION 2.0.0, PLEASE EDIT WITH CARE
'use strict';

var List                    = require("bs-platform/lib/js/list.js");
var Caml_int32              = require("bs-platform/lib/js/caml_int32.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function all_primes(a, b) {
  var not_divisible_by = function (n, _divisors) {
    while(true) {
      var divisors = _divisors;
      if (divisors) {
        var match = +(Caml_int32.mod_(n, divisors[0]) === 0);
        if (match !== 0) {
          return /* false */0;
        } else {
          _divisors = divisors[1];
          continue ;
          
        }
      } else {
        return /* true */1;
      }
    };
  };
  var loop = function (_n, _accum) {
    while(true) {
      var accum = _accum;
      var n = _n;
      if (n > b) {
        return accum;
      } else if (n === 2) {
        _accum = /* :: */[
          n,
          accum
        ];
        _n = n + 1 | 0;
        continue ;
        
      } else if (n === 3) {
        _accum = /* :: */[
          n,
          accum
        ];
        _n = n + 2 | 0;
        continue ;
        
      } else if (not_divisible_by(n, accum)) {
        _accum = /* :: */[
          n,
          accum
        ];
        _n = n + 2 | 0;
        continue ;
        
      } else {
        _n = n + 2 | 0;
        continue ;
        
      }
    };
  };
  var _l = loop(2, /* [] */0);
  var _accum = /* [] */0;
  while(true) {
    var accum = _accum;
    var l = _l;
    if (l) {
      var t = l[1];
      var h = l[0];
      if (a <= h && h <= b) {
        _accum = /* :: */[
          h,
          accum
        ];
        _l = t;
        continue ;
        
      } else {
        _l = t;
        continue ;
        
      }
    } else {
      return accum;
    }
  };
}

function goldbach(n) {
  var primes = all_primes(2, n);
  var _ascending = primes;
  var _descending = List.rev(primes);
  while(true) {
    var descending = _descending;
    var ascending = _ascending;
    if (ascending) {
      if (descending) {
        var hd = descending[0];
        var ha = ascending[0];
        if ((ha + hd | 0) === n) {
          return /* tuple */[
                  ha,
                  hd
                ];
        } else if (ha > hd) {
          throw Caml_builtin_exceptions.not_found;
        } else if ((ha + hd | 0) < n) {
          _ascending = ascending[1];
          continue ;
          
        } else {
          _descending = descending[1];
          continue ;
          
        }
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

console.log(goldbach(28));

exports.all_primes = all_primes;
exports.goldbach   = goldbach;
/*  Not a pure module */