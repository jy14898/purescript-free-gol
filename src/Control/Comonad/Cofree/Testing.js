"use strict";

//exports.mkCofree = (function(){
//    var haveLogged = false;
//    
//    return function (v) {
//        if (!haveLogged) {
//            haveLogged = true;
//            console.log(v);
//        }
//        return v;
//    };
//})();

exports.mkCofree = function (v) {
    return v;
};

exports.unCofree = function (v) {
    return v;
};

exports["refTabulate"] = function (defer) {
    return function(f) {
        var map = new WeakMap();

        return function(x) {
            var ret = map.get(x);

            if (ret === undefined) {
                ret = defer(function() {
                    return f(x);
                });
                map.set(x, ret);
            }

            return ret;
        };
    };
};

// Add a memoization strategy parameter
// precompute = map all before hand
// lazy = compute as needed
// bounded-lazy = fixed size lookup

// exports["myMemoize"] = function(f) {
//     var map = new WeakMap();
// 
//     return function (r) {
//         var ret = map.get(r.ys);
// 
//         if (ret === undefined) {
//             ret = r.ys.map(function (ignore, i) {
//                 return f({
//                     index: i,
//                     ys: r.ys
//                 });
//             });
// 
//             map.set(r.ys, ret);
//         }
// 
//         return ret[r.index];
//     };
// };

exports["myMemoize"] = function(f) {
    var map = new WeakMap();

    return function (r) {
        var lookup = map.get(r.ys);

        if (lookup === undefined) {
            lookup = new Array(r.ys.length)

            map.set(r.ys, lookup);
        }

        if (lookup[r.index] === undefined) {
            lookup[r.index] = f(r);
        }

        return lookup[r.index];
    };
};
