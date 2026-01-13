use std.prelude.*;

const CanvasRenderingContext2DRaw = @opaque_type;
const CanvasRenderingContext = newtype (
    .raw :: CanvasRenderingContext2DRaw,
);

impl CanvasRenderingContext as module = (
    module:
    
    const strokeRect = (x, y, width, height) => (
        ctx => (
            (@native "async ({ctx,x,y,width,height}) => {ctx.strokeRect(x,y,width,height)}")(.ctx = ctx.raw, .x, .y, .width, .height)
        
        )
    );
    const fillRect = (x, y, width, height) => (
        ctx => (
            (@native "async ({ctx,x,y,width,height}) => {ctx.fillRect(x,y,width,height)}")(.ctx = ctx.raw, .x, .y, .width, .height)
        
        )
    );
    const beginPath = (ctx) => (
        (@native "async ({ctx}) => {ctx.beginPath()}")(.ctx = ctx.raw)
    
    );
    const closePath = (ctx) => (
        (@native "async ({ctx}) => {ctx.closePath()}")(.ctx = ctx.raw)
    
    );
    const moveTo = (x, y) => (
        (ctx) => (
            (@native "async ({ctx,x,y}) => {ctx.moveTo(x,y)}")(.ctx = ctx.raw, .x, .y)
        
        )
    );
    const lineTo = (x, y) => (
        (ctx) => (
            (@native "async ({ctx,x,y}) => {ctx.lineTo(x,y)}")(.ctx = ctx.raw, .x, .y)
        
        )
    );
    const stroke = (ctx) => (
        (@native "async ({ctx}) => {ctx.stroke()}")(.ctx = ctx.raw)
    
    );
);

const HtmlCanvasElement = newtype (
    .width :: Int32,
    .height :: Int32,
    .getContext :: String -> CanvasRenderingContext2DRaw,
);

const HtmlBodyElement = newtype (
    .innerHTML :: String,
    .appendChild :: HtmlCanvasElement -> (),
);

const HtmlDocumentElement = newtype (
    .body :: HtmlBodyElement,
    .createElement :: String -> HtmlCanvasElement,
);

const document = () -> HtmlDocumentElement => (
    @native "document"
);

let mut document = document();

let print_html = (html :: String) => (
    document.body.innerHTML += html;
);

print("Hello, console");
print_html("<p>Hello, <i>World</i></p>");

dbg.print(@native "document.createElement('canvas')");

const createElement = (document, t) => (
    (@native "async ({document, t}) => document.createElement(t)")(.document, .t)

);
const appendChild = (element, child) => (
    (@native "async ({element, child}) => element.appendChild(child)")(.element, .child)

);

let mut canvas = createElement(document, "canvas");
canvas.width = 400;
canvas.height = 300;
appendChild(document.body, canvas);

const Ctx = CanvasRenderingContext;
const getContext = (canvas, t) -> CanvasRenderingContext => (
    .raw = (@native "async ({canvas, t}) => canvas.getContext(t)")(.canvas, .t),
);

let mut ctx = getContext(canvas, "2d");

ctx |> Ctx.strokeRect(75, 140, 150, 110);
ctx |> Ctx.fillRect(130, 190, 40, 60);
ctx |> Ctx.beginPath;
ctx |> Ctx.moveTo(50, 140);
ctx |> Ctx.lineTo(150, 60);
ctx |> Ctx.lineTo(250, 140);
ctx |> Ctx.closePath;
ctx |> Ctx.stroke;
