"use strict";

var jssGlobal = require("jss");
var preset = require("jss-preset-default");

exports.createInstance_ = function (p) {
  return jssGlobal.create(p());
};

exports.preset = preset.default;

exports.createStyleSheet_ = function (jss, styles) {
  return jss.createStyleSheet(styles);
};

exports.globalAttachStyleSheet_ = function (stylesheet) {
  return stylesheet.attach();
};

exports.toStringStyleSheet = function (stylesheet) {
  return stylesheet.toString();
};
