if (process.env.NODE_ENV == "production") {
  require("/output-es/Main").main();
} else {
  require("/output/Main").main();
}
