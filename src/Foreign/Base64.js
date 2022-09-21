export const _atob = (Left, Right, str) => {
  var result;

  try {
    result = Right(atob(str));
  }
  catch (error) {
    result = Left(error);
  }

  return result;
};

export const _btoa =  (Left, Right, str) => {
  var result;

  try {
    result = Right(btoa(str));
  }
  catch (error) {
    result = Left(error);
  }

  return result;
};
