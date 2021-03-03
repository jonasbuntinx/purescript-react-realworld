if (process.env.NODE_ENV == "production") {
  window.renderWithState = require("/dce-output/Main").renderWithState;
} else {
  require("/output/Main").renderWithState();
}
