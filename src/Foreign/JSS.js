"use strict";

var jssGlobal = require("jss");
var preset = require("jss-preset-default");

exports._createInstance = function (p) {
  return jssGlobal.create(p());
};

exports.preset = preset.default;

exports._createStyleSheet = function (jss, styles) {
  return jss.createStyleSheet(styles);
};

exports._globalAttachStyleSheet = function (stylesheet) {
  return stylesheet.attach();
};

exports.toStringStyleSheet = function (stylesheet) {
  return stylesheet.toString();
};
