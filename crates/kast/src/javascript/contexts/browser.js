(async function () {
    function cleanup() {
    }
    let output_buffer = "";
    const output = {
        write: async function (ctx, ref_s) {
            let s = ref_s.get();
            let newline_index;
            while ((newline_index = s.indexOf('\n')) != -1) {
                output_buffer += s.substring(0, newline_index);
                console.log(output_buffer);
                output_buffer = "";
                s = s.substring(newline_index + 1);
            }
            output_buffer += s;
        },
    };
    const input = {
        read_line: async function (ctx) {
            throw Error("no input");
        },
    }
    const contexts = {
        input,
        output,
    };
    return { contexts, cleanup };
})()