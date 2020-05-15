const {
    init_panic_hook,
    new_runner,
    parse_src,
    gen_io_info,
    run_with_data,
    run_with_input,
    run,
    update,
    update_from,
    output_array,
    output_array_get,
    output_series,
    output_options,
    output_colors,
    __wasm: { memory }
} = require("../pkg");

init_panic_hook();

const DATA_TYPE = {
    FLOAT64: 0,
    INT32: 1,
};

function parseOutputSeries(outSeries, dataType) {

    let ArrayType = dataType === DATA_TYPE.FLOAT64 ? Float64Array : Int32Array;
    let unitBytes = dataType === DATA_TYPE.FLOAT64 ? 8 : 4;

    let outputData = [];
    let byteOffset = 0;

    // The count of data list
    const countPtr = new ArrayType(memory.buffer, outSeries, 1);
    let listCount = Number(countPtr[0]);
    byteOffset += unitBytes;

    for (let i = 0; i < listCount; i += 1) {
        const countPtr = new ArrayType(memory.buffer, outSeries + byteOffset, 1);
        let count = Number(countPtr[0]);

        byteOffset += unitBytes;
        if (count === 0) {
            outputData.push(new ArrayType([]));
        } else {
            outputData.push(new ArrayType(memory.buffer, outSeries + byteOffset, count).slice());
            byteOffset += unitBytes * count;
        }
    }
    return outputData;
}

// Extract data from the data pointer
function getOutputData(dataPtr) {
    let dataInfo = output_array(dataPtr);
    let allData = [];

    for (let i = 0; i < dataInfo[2]; i += 1) {
        let outputData = {};
        let oneData = output_array_get(dataPtr, i);

        outputData.series = parseOutputSeries(output_series(oneData), DATA_TYPE.FLOAT64);
        outputData.colorOptions = output_options(oneData);
        outputData.colors = parseOutputSeries(output_colors(oneData), DATA_TYPE.INT32);
        allData.push(outputData);
    }
    return allData;
}

class Runner {
    constructor() {
        this.runner = new_runner();
    }

    parse(src) {
        parse_src(this.runner, src);
    }

    genIOInfo() {
        return gen_io_info(this.runner);
    }

    runWithData(srcs, len, data, syminfo) {
        let dataPtr = run_with_data(
            this.runner, srcs, len, data, syminfo,
        );
        return getOutputData(dataPtr);
    }

    runWithInput(input) {
        let dataPtr = run_with_input(this.runner, [{
            type: "Int",
            content: 100
        }]);
        return getOutputData(dataPtr);
    }

    run(input, srcs, len, data, syminfo) {
        let dataPtr = run(this.runner, input, srcs, len, data, syminfo);
        return getOutputData(dataPtr);
    }

    update(srcs, len, data) {
        let dataPtr = update(this.runner, srcs, len, data);
        return getOutputData(dataPtr);
    }

    updateFrom(srcs, from, len, data) {
        let dataPtr = update_from(this.runner, srcs, from, len, data);
        return getOutputData(dataPtr);
    }
}

module.exports = Runner;