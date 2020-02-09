const fs = require('fs-extra');
const path = require('path');

fs.copySync(
    path.join(__dirname, "../package.json"),
    path.join(__dirname, "../pkg/package.json")
);