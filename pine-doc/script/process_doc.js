const path = require("path");
const cheerio = require('cheerio')
const fs = require("fs");

console.log(process.argv);

process.argv.slice(2).forEach(function (processPath) {
    const htmlPath = path.join(process.cwd(), processPath);
    console.log("Will process html file ", htmlPath);

    const $ = cheerio.load(fs.readFileSync(htmlPath).toString());

    $("pre").each(function (i, element) {
        let lang = element.attribs["lang"];
        element.children.filter((child) => {
            return child.tagName.toUpperCase() === "CODE";
        }).forEach((child) => {
            child.attribs["class"] = `${lang}`;
        })
    });

    // console.log("Result:", $.html());

    fs.writeFileSync(htmlPath, $.html());
});
