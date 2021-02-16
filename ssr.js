global.XMLHttpRequest = require('xhr2');

exports.handler = require("/dce-output/Serverless.Main").handler;
