global.window = Object.create( { fetch: function() { console.log("FOOBAR"); return; } } );

exports.handler = require("/dce-output/Main").handler;
