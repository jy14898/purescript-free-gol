"use strict";

exports.mapUncofree = function (x) {
    return x
};

exports.refOnce = function(f) {
    var set = new WeakSet();

    return function(start) {
        var stack = [start];

        while (stack.length !== 0) {
            let x = stack.pop();
            let ys = f(x);

            ys.forEach(function (y) {
                if (!set.has(y)) {
                    set.add(y);
                    stack.push(y);
                }
            });
        }

        return {};
    };
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

exports.refEq = function (a) {
    return function (b) {
        return a === b;
    };
};
