OUTPUT_PATH=./pine-doc/static/output/
if [ ! -f ${OUTPUT_PATH}highlight.min.js ]; then
    curl -L -o ${OUTPUT_PATH}build.zip https://github.com/liuxiong332/highlight.js/releases/download/11.0/build.zip
    unzip ${OUTPUT_PATH}build.zip -d ${OUTPUT_PATH}
    cp ${OUTPUT_PATH}build/highlight.min.js ${OUTPUT_PATH}highlight.min.js
    cp ${OUTPUT_PATH}build/demo/jquery-2.1.1.min.js ${OUTPUT_PATH}jquery-2.1.1.min.js
    cp ${OUTPUT_PATH}build/demo/perfect-scrollbar.min.css ${OUTPUT_PATH}perfect-scrollbar.min.css
    cp ${OUTPUT_PATH}build/demo/perfect-scrollbar.min.js ${OUTPUT_PATH}perfect-scrollbar.min.js
    cp ${OUTPUT_PATH}build/demo/styles/default.css ${OUTPUT_PATH}default.css
    # cp ${OUTPUT_PATH}build/demo/styles/github-gist.css ${OUTPUT_PATH}github-gist.css
fi

cargo build
./target/debug/pine-doc

if [ ! -d ./pine-doc/node_modules ]; then
    cd pine-doc
    npm install
    cd ..
fi

node ./pine-doc/script/process_doc.js ${OUTPUT_PATH}all_doc.html ${OUTPUT_PATH}doc.html