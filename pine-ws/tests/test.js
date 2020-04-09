const Runner = require("./Runner");
const assert = require('assert');

it('simple test', function () {
    let runner = new Runner();
    runner.parse("m = input(1, 'hello', 'int')\nplot(close + m)");
    let ioInfo = runner.genIOInfo();
    // console.log("io info:", JSON.stringify(ioInfo));
    assert.equal(ioInfo.inputs.length, 1);
    assert.deepEqual(ioInfo.input_srcs, [{ "ticker": null, "srcs": ["close"] }]);

    let output = runner.runWithData(["close", "open", "high", "low"], 1, new Float64Array([10, 90, 0, 0]));
    assert.equal(output.length, 1);
    assert.deepEqual(output[0].series, [new Float64Array([11.0])]);
    // assert.deepEqual(Array.from(output[0].series), [11.0]);

    output = runner.runWithInput([{ type: "Int", content: 100 }]);
    assert.deepEqual(output[0].series, [new Float64Array([110.0])]);

    output = runner.run(
        [{
            type: "Int",
            content: 200
        }],
        ["close", "open", "high", "low"], 1, new Float64Array([10, 90, 0, 0]));
    assert.deepEqual(output[0].series, [new Float64Array([210.0])]);

    output = runner.update(["close", "open", "high", "low"], 1, new Float64Array([1, 2, 3, 4]));
    assert.deepEqual(output[0].series, [new Float64Array([201.0])]);

    output = runner.updateFrom(
        ["close", "open", "high", "low"], 0, 1, new Float64Array([10, 2, 3, 4]),
    );
    assert.deepEqual(output[0].series, [new Float64Array([210.0])]);
});

it("input with options test", function () {
    let runner = new Runner();
    runner.parse("m = input(1, 'hello', 'int', options=[1, 2, 3])\n");
    assert.deepEqual(runner.genIOInfo().inputs[0], {
        type: 'Int',
        defval: 1,
        title: 'hello',
        input_type: 'int',
        minval: null,
        maxval: null,
        confirm: null,
        step: null,
        options: [1, 2, 3]
    });
});

it("input with string options test", function () {
    let runner = new Runner();
    runner.parse('m = input(title="Smoothing", defval="RMA", options=["RMA", "SMA", "EMA", "WMA"])');
    // console.log(runner.genIOInfo().inputs[0]);
    assert.deepEqual(runner.genIOInfo().inputs[0], {
        type: 'String',
        defval: 'RMA',
        title: 'Smoothing',
        input_type: 'string',
        confirm: null,
        options: ["RMA", "SMA", "EMA", "WMA"]
    });
});

it("volume should plot", function () {
    let runner = new Runner();
    runner.parse('plot(volume)');
    // console.log(runner.genIOInfo().inputs[0]);
    console.log(runner.genIOInfo().input_srcs);
    assert.deepEqual(runner.genIOInfo().input_srcs, [{ ticker: null, srcs: ['volume'] }]);
    let result = runner.runWithData(["volume"], 1, new Float64Array([10]));
    console.log(result);
    assert.deepEqual(result[0].series, [new Float64Array([10.0])]);

});