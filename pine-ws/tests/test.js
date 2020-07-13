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

const Script = `
//@version=4
study(title="TRIX", shorttitle="TRIX", format=format.price, precision=2)
length = input(2, minval=1)
out = ema(log(close), 2) 
plot(out, color=color.maroon, title="TRIX")
`;

it("log test", function () {
    let runner = new Runner();
    runner.parse(Script);
    // console.log(runner.genIOInfo().inputs[0]);
    console.log(runner.genIOInfo().input_srcs);
    assert.deepEqual(runner.genIOInfo().input_srcs, [{ ticker: null, srcs: ['close'] }]);
    let farray = new Float64Array(100);
    for (let i = 0; i < 3; i += 1) {
        farray[i] = i + 100.0;
    }
    let result = runner.runWithData(["close"], 3, farray);
    // console.log(result[0].series[0]);
    assert(!isNaN(result[0].series[0][0]));
    // assert.deepEqual(result[0].series, [new Float64Array([10.0])]);
});

const FLScript = `
plot(open)
plot(close)
plot(close)
plot(close)
plot(close)
plot(close)
plot(close)
plot(close)
plot(close)
plot(close)
`;

it("fl script test", function () {
    let runner = new Runner();

    runner.parse(FLScript);
    // console.log(runner.genIOInfo().inputs[0]);
    assert.deepEqual(runner.genIOInfo().input_srcs, [{ ticker: null, srcs: ['open', 'close'] }]);

    for (let m = 0; m < 3; m += 1) {
        let farray = new Float64Array(800);
        for (let i = 0; i < 400 * 2; i += 1) {
            farray[i] = i + 100.0;
        }

        let result = runner.runWithData(["close", "open"], 400, farray);
        // console.log(result[0].series[0]);
        result.map(d => {
            assert.equal(d.series[0].length, 400);
        });
    }
});

it("abs test", function () {
    let runner = new Runner();

    runner.parse("plot(abs(-5))");
    runner.genIOInfo();

    // console.log(result[0].series[0]);
    let result = runner.runWithData([], 3, new Float64Array(0));
    // console.log(result[0].series[0]);
    assert.deepEqual(result[0].series, [new Float64Array([5.0, 5.0, 5.0])]);

    result = runner.run([], [], 2, new Float64Array(0));
    assert.deepEqual(result[0].series, [new Float64Array([5.0, 5.0])]);

});

it("dayofweek test", function () {
    let runner = new Runner();
    runner.parse("plot(dayofweek)");
    console.log(runner.genIOInfo().input_srcs);
    // assert.deepEqual(runner.genIOInfo().input_srcs, [{ ticker: null, srcs: ['time'] }]);

    runner.runWithData([], 3, new Float64Array(0), {
        symbol_type: "stock", timezone: "GTC+8", ticker: "", session: "", trade_start: "",
        trade_end: "", root: "", currency: "", description: "", mintick: 1,
    });
});

it("alma test", function () {
    let runner = new Runner();
    runner.parse("plot(alma(4, 4, 0.85, 2.0))");
    console.log(runner.genIOInfo().input_srcs);
    // assert.deepEqual(runner.genIOInfo().input_srcs, [{ ticker: null, srcs: ['time'] }]);

    let farray = new Float64Array(100);
    for (let i = 0; i < 8; i += 1) {
        farray[i] = i + 100.0;
    }
    let result = runner.runWithData(["time"], 8, farray, {
        symbol_type: "stock", timezone: "America/New_York", ticker: "", session: "", trade_start: "",
        trade_end: "", root: "", currency: "", description: "", mintick: 1,
    });
    // console.log(result[0].series);
    assert.deepEqual(result[0].series[0].slice(3, 8), new Float64Array([4, 4, 4, 4, 4]));
});

it("timenow test", function () {
    let runner = new Runner();
    runner.parse("int a = timenow\nplot(a)");
    runner.genIOInfo();
});

const RSIScript = `
src = close
len = 14
up = rma(max(change(src), 0), len)
down = rma(-min(change(src), 0), len)
rsia = down == 0 ? 100 : up == 0 ? 0 : 100 - (100 / (1 + up / down))
`;

it("rsi test", function () {
    let runner = new Runner();
    runner.parse(RSIScript);
    // runner.genIOInfo();

    let farray = new Float64Array(100);
    for (let i = 0; i < 2; i += 1) {
        farray[i] = i + 100.0;
    }
    for (let i = 0; i < 2; i += 1) {
        farray[i] = NaN;
    }
    let result = runner.runWithData(["close"], 4, farray);
    console.log("result", result);
});