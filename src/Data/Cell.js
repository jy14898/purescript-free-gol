"use strict";

exports.safeIndex = function (xs) {
    return function (index) {
        return xs[((index % xs.length) + xs.length) % xs.length];
    };
};
