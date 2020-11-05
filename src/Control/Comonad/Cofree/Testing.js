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
