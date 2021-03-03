if (process.env.NODE_ENV == "production") {
  require("/dce-output/Main").main(window.dehydrated);
} else {
  require("/output/Main").main();
}
