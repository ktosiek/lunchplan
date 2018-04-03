'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

var UUID = require('uuid-js');
var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

var userID = localStorage.getItem("userID") || UUID.create();
localStorage.setItem("userID", userID);

var userName = sessionStorage.getItem("userName") || prompt("Przedstaw się", "Piesio Grzesio");
sessionStorage.setItem("userName", userName)

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
var app = Elm.Main.embed(mountNode, {
    userID,
    userName,
});
