const path = require('path');

module.exports = {
  entry: './index.js', // Entry point
  output: {
    filename: 'bundle.js', // Output file
    path: path.resolve(__dirname, 'dist'),
  },
  mode: 'development', // Or 'production'
};