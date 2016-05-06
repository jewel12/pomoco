var app = require('app');
var BrowserWindow = require('browser-window');
var Tray = require('tray');
var Menu = require('menu');
var ipc = require('ipc');

var mainWindow = null;

app.on('window-all-closed', function() {
    app.quit();
});

app.on('ready', function() {
    mainWindow = new BrowserWindow({
        width: 300,
        height: 250,
    });
    mainWindow.loadUrl('file://' + __dirname + '/index.html');
    mainWindow.on('closed', function() {
        mainWindow = null;
    });
    var iconPath = __dirname + '/tokei.png';
    var appIcon = new Tray(iconPath.toString());

    ipc.on('time-message', function(event, arg) {
        appIcon.setTitle(arg)
    });
});
