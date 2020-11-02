"use strict";

exports.mapUncofree = function (x) {
    return x
};

exports.refOnce = function(f) {
    var set = new WeakSet();

    return function(x) {
        if (!set.has(x)) {
            set.add(x);
            f(x);
        }

        return {};
    };
};

// f returns a list of things to work on
exports.refOnce = function(f) {
    var set = new WeakSet();

    var total = 0;

    return function(start) {
        var stack = [start];
        total += 1;

        while (stack.length !== 0) {
            let x = stack.pop();

            let ys = f(x);

            ys.forEach(function (y) {
                if (!set.has(y)) {
                    total += 1;
                    set.add(y);
                    stack.push(y);
                }
            });
        }

        console.log(total);

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

