(async function () {
    const readline = require('node:readline/promises').createInterface({
        input: process.stdin,
        output: process.stdout
    });
    function cleanup() {
        readline.close();
    }
    const output = {
        write: async function (ctx, ref_s) {
            process.stdout.write(ref_s.get());
        },
    };
    const input = {
        read_line: async function (ctx) {
            return await readline.question('')
        },
    }
    const contexts = {
        input,
        output,
    };
    return { contexts, cleanup };
})()