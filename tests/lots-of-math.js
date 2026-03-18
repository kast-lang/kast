const range = (start, end) => {
    return {
        iter: (consumer) => {
            let i = start;
            for (; ;) {
                if (i < end) {
                    let res = consumer(i);
                    if (res && res.err) {
                        return res.err;
                    }
                    i += 1;
                } else {
                    break;
                }
            }
        },
    };
};

let is_prime = x => {
    const block_token = 3124235;
    let res = range(2, x).iter(i => {
        if (x % i === 0) {
            result = false;
            return { err: true, token: block_token, value: false };
        }
        return true;
    });
    if (res && res.err) {
        let e = res;
        if (e.token === block_token) return e.value;
    }
    return true;
};

let sum = 0;
range(2, 30000).iter((x) => {
    if (is_prime(x)) {
        sum += x;
    }
    return true;
});
console.log(sum);

// let sum = 0;
// for (let x = 2; x < 30000; x++) {
//   let is_prime = true;
//   for (let i = 2; i < x; i++) {
//     if (x % i === 0) {
//       is_prime = false;
//       break;
//     }
//   }
//   if (is_prime) {
//     sum = sum + x;
//   }
// }
// console.log(sum);
