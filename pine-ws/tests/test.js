const { init_panic_hook, new_runner, parse_src, gen_io_info, run_with_data, run_with_input, run, update, update_from } = require("../pkg");
const { memory } = require("../pkg/node/pine_bg");

init_panic_hook();

let runner = new_runner();
console.log("runner:", runner);

let result = parse_src(runner, "m = input(1, 'hello', 'int')\nplot(close + m)");
console.log("src result", result);

let io_info = gen_io_info(runner);
console.log("io info:", JSON.stringify(io_info));

console.log("len", new Float64Array([0, 0, 0, 0]).length);

let dataPtr = run_with_data(
    runner, ["close", "open", "high", "low"], 1,
    new Float64Array([100.1, 90, 0, 0]),
);

function getOutputData(dataPtr) {
    let outputData = [];
    let byteOffset = 0;
    for (let i = 0; i < io_info.outputs.length; i += 1) {
        const cells = new Int32Array(memory.buffer, dataPtr + byteOffset, 2);
        byteOffset += 4 * 2;
        console.log("cells", cells);
        if (cells[0] === 0 && cells[1] === 0) {
            outputData.push([]);
        } else {
            let count = cells[1] - cells[0];
            outputData.push(new Float64Array(memory.buffer, dataPtr + byteOffset, count));
            byteOffset += 8 * count;
        }
    }
    return outputData;
}

console.log("Run with data result:", getOutputData(dataPtr));

dataPtr = run_with_input(runner, [{ type: "Int", content: 100 }]);
console.log("Run with input result:", getOutputData(dataPtr));

dataPtr = run(
    runner, [{ type: "Int", content: 200 }],
    ["close", "open", "high", "low"], 1,
    new Float64Array([10, 90, 0, 0]),
);
console.log("Run with data and input result:", getOutputData(dataPtr));

dataPtr = update(
    runner, ["close", "open", "high", "low"], 1,
    new Float64Array([1, 2, 3, 4]),
);
console.log("Update result:", getOutputData(dataPtr));

dataPtr = update_from(
    runner, ["close", "open", "high", "low"], 0, 1,
    new Float64Array([10, 2, 3, 4]),
);
console.log("Update result:", getOutputData(dataPtr));
