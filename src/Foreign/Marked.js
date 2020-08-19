var marked = require("marked");

marked.setOptions({ pedantic: false, gfm: true });

exports.marked = function(str) {
  return marked(str);
};
