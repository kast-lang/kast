# Run on https://play.kast-lang.org/
 
use std.*;
 
# ---------- Params ----------
 
let resolutionXBuffer :: &string = "FIX FRICKING LOOPS SMH !!!!!!!!!!!!!!!!!";
let resolutionYBuffer :: &string = "KUVIII !!!!!!!!!!!!!";
let iterationsBuffer :: &string =  "ERMMMMMM";
 
let offsetX :: float64 = - 0.75;
let offsetY :: float64 = 0;
let bounds :: float64 = 1.5;
let threshold :: float64 = 10;
 
# ---------- Setup ----------
 
let mut resolutionX :: float64 = 0;
let mut resolutionY :: float64 = 0;
 
for _ :: char in chars resolutionXBuffer {
    resolutionX += 1;
};
 
for _ :: char in chars resolutionYBuffer {
    resolutionY += 1;
};
 
# ---------- Helpers ----------
 
const write :: &string -> () with output = line => (
    let output = current output;
    output.write line;
);
 
let lerp = fn(a :: float64, b :: float64, t :: float64) -> float64 {
    (b - a) * t + a
};
 
let inverseLerp = fn(a :: float64, b :: float64, v :: float64) -> float64 {
    (v - a) / (b - a)
};
 
let remap = fn(fromA :: float64, fromB :: float64, toA :: float64, toB :: float64, v :: float64) -> float64 {
    let t :: float64 = inverseLerp(fromA, fromB, v);
    lerp(toA, toB, t)
};
 
const Complex :: type = (float64, float64);
 
let add = fn(a :: Complex, b :: Complex) -> Complex {
    (a.0 + b.0, a.1 + b.1)
};
 
let multiply = fn(a :: Complex, b :: Complex) -> Complex {
    (
        a.0 * b.0 - a.1 * b.1,
        a.0 * b.1 + a.1 * b.0
    )
};
 
let sqMagnitude = fn(a :: Complex) -> float64 {
    a.0*a.0 + a.1*a.1
};
 
let f = fn(z :: Complex, c :: Complex) -> Complex {
    add(multiply(z, z), c)
};
 
# ---------- Rendering ----------
 
let render = fn(t :: int32) {
    let result :: &string = if t < 2 then " "
    else (
        if t < 4 then "░"
        else (
            if t < 6 then "▒"
            else "█"
        )
    );
    write result;
};
 
let mut vy :: float64 = 0;
for _ :: char in chars resolutionYBuffer {
 
    let mut vx :: float64 = 0;
    for _ :: char in chars resolutionXBuffer {
 
        let wx :: float64 = remap(0, resolutionX, -bounds, bounds, vx) + offsetX;
        let wy :: float64 = remap(0, resolutionY, -bounds, bounds, vy) + offsetY;
 
        let mut z :: Complex = (wx, wy);
        let mut c :: Complex = (wx, wy);
 
        let mut stable :: int32 = 0;
 
        for _ :: char in chars iterationsBuffer {
            z = f(z, c);
 
            let t = sqMagnitude(z);
            if t < threshold then (
                stable += 1;
            );
        };
 
        render(stable);
 
        vx += 1;
    };
 
    write "\n";
    vy += 1;
};
