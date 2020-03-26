const {
    init_panic_hook, new_runner, parse_src, gen_io_info,
    run_with_data, run_with_input, run, update, update_from,
    output_array, output_array_get, output_series
} = require("../pkg");
const { memory } = require("../pkg/node/pine_bg");

init_panic_hook();

let runner = new_runner();
console.log("runner:", runner);

// Parse the pine source code.
let result = parse_src(runner, `
study(title="DXC1", shorttitle="DXC2")
src = input(title='Source', type=input.source, defval=close)
fast_ma = sma(src, 10)
`);
console.log("src result", result);

// Get the input and output information
let ioInfo = gen_io_info(runner);
console.log("io info:", JSON.stringify(ioInfo));

function parseOutputSeries(outSeries) {
    let outputData = [];
    let byteOffset = 0;

    // The count of data list
    const countPtr = new Float64Array(memory.buffer, outSeries, 1);
    let listCount = Number(countPtr[0]);
    byteOffset += 8;

    for (let i = 0; i < listCount; i += 1) {
        const countPtr = new Float64Array(memory.buffer, outSeries + byteOffset, 1);
        let count = Number(countPtr[0]);
        console.log("count :", count);

        byteOffset += 8;
        if (count === 0) {
            outputData.push([]);
        } else {
            outputData.push(new Float64Array(memory.buffer, outSeries + byteOffset, count));
            byteOffset += 8 * count;
        }
    }
    return outputData;
}

// Extract data from the data pointer
function getOutputData(dataPtr) {
    let dataInfo = output_array(dataPtr);
    let allData = [];
    console.log("data info", dataInfo);

    for (let i = 0; i < dataInfo[2]; i += 1) {
        let oneData = output_array_get(dataPtr, i);
        let seriesData = output_series(oneData);
        allData.push(parseOutputSeries(seriesData));
    }
    return allData;
}

let dataPtr = run_with_data(
    runner, ["close", "open", "high", "low"], 1,
    new Float64Array([100.1, 90, 0, 0]),
);

console.log("Run with data result:", getOutputData(dataPtr));
