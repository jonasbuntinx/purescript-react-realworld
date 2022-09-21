import * as dayjs from "dayjs";
import * as advancedFormat from "dayjs/plugin/advancedFormat";

dayjs.extend(advancedFormat);

export const _fromUTCString = (nothing, just, str) => {
  var d = dayjs(str);
  return d.isValid() ? just(d) : nothing;
};

export const toUTCString = (d) => {
  return d.format();
};

export const fromMilliseconds = (ms) => {
  return dayjs(ms);
};

export const toMilliseconds = (d) => {
  return d.valueOf();
};

export const now = () => {
  return dayjs();
};

export const format = (format) => {
  return function (d) {
    return d.format(format);
  };
};
