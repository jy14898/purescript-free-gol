"use strict";

exports.mkCofree = (function(){
    var haveLogged = true;
    
    return function (v) {
        if (!haveLogged) {
            haveLogged = true;
            console.log(v);
        }
        return v;
    };
})();

exports.unCofree = function (v) {
    return v;
};
