'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

const UUID = require('uuid-js');
const Elm = require('./Main.elm');
const io = require('socket.io-client');
const IO_URL = 'http://' + window.location.hostname + ':5000/';

const userID = localStorage.getItem("userID") || UUID.create().hex;
localStorage.setItem("userID", userID);

const userName = sessionStorage.getItem("userName") || prompt("Przedstaw się", "Piesio Grzesio");
sessionStorage.setItem("userName", userName)

const mountNode = document.getElementById('main');
const app = Elm.Main.embed(mountNode, {
    userID,
    userName,
});


// Przygotowanie portów do komunikacji z serwerem
const connection = io(IO_URL)
connection
    .on('connect', (data) => {
        connection.emit('login', { id: userID, name: userName })
    })
    .on('sync', (data) => {
        app.ports.sync.send(data)
    })
    .on('user', (data) => {
        app.ports.user.send(data)
    })
    .on('position', (data) => {
        app.ports.position.send(data)
    })
    .on('order', (data) => {
        app.ports.order.send(data)
    })

app.ports.updateOrder.subscribe(
    (data) => { console.log('update order', data); connection.emit('update order', data) })
app.ports.updatePosition.subscribe(
    (data) => { connection.emit('update position', data) })
