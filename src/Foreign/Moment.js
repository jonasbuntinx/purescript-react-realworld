"use strict";

var moment = require("moment");

exports._fromUTCString = function (nothing, just, str) {
  var m = moment.utc(str).local();
  return m.isValid() ? just(m) : nothing;
};

exports.toUTCString = function (m) {
  return m.clone().utc().format();
};

exports.fromMilliseconds = function (ms) {
  return moment(ms).local();
};

exports.toMilliseconds = function (m) {
  return m.valueOf();
};

exports.now = function () {
  return moment().local();
};

exports.format = function (format) {
  return function (m) {
    return m.format(format);
  };
};
