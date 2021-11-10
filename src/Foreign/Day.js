var dayjs = require("dayjs");

dayjs.extend(require("dayjs/plugin/advancedFormat"));

exports._fromUTCString = function (nothing, just, str) {
  var d = dayjs(str);
  return d.isValid() ? just(d) : nothing;
};

exports.toUTCString = function (d) {
  return d.format();
};

exports.fromMilliseconds = function (ms) {
  return dayjs(ms);
};

exports.toMilliseconds = function (d) {
  return d.valueOf();
};

exports.now = function () {
  return dayjs();
};

exports.format = function (format) {
  return function (d) {
    return d.format(format);
  };
};
