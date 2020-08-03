"use strict";

function atobImpl (Left, Right, str) {
  var result;

  try {
    result = Right(atob(str));
  }
  catch (error) {
    result = Left(error);
  }

  return result;
};

function btoaImpl (Left, Right, str) {
  var result;

  try {
    result = Right(btoa(str));
  }
  catch (error) {
    result = Left(error);
  }

  return result;
};

exports.atobImpl = atobImpl;
exports.btoaImpl = btoaImpl;
