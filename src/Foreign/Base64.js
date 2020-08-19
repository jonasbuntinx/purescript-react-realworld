exports._atob = function (Left, Right, str) {
  var result;

  try {
    result = Right(atob(str));
  }
  catch (error) {
    result = Left(error);
  }

  return result;
};

exports._btoa = function (Left, Right, str) {
  var result;

  try {
    result = Right(btoa(str));
  }
  catch (error) {
    result = Left(error);
  }

  return result;
};
