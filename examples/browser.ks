use std.prelude.*;

const CanvasRenderingContext2DRaw = @opaque_type;
const CanvasRenderingContext = newtype {
    .raw :: CanvasRenderingContext2DRaw,
};

impl CanvasRenderingContext as module = (
    module:
    
    const strokeRect = (context, x, y, width, height) => (
        (@native "async ({context,x,y,width,height},ctx) => {context.strokeRect(x,y,width,height)}")(.context = context.raw, .x, .y, .width, .height)
    );
    const fillRect = (context, x, y, width, height) => (
        (@native "async ({context,x,y,width,height},ctx) => {context.fillRect(x,y,width,height)}")(.context = context.raw, .x, .y, .width, .height)
    );
    const beginPath = (context) => (
        (@native "async ({context},ctx) => {context.beginPath()}")(.context = context.raw)
    
    );
    const closePath = (context) => (
        (@native "async ({context},ctx) => {context.closePath()}")(.context = context.raw)
    
    );
    const moveTo = (context, x, y) => (
        (@native "async ({context,x,y},ctx) => {context.moveTo(x,y)}")(.context = context.raw, .x, .y)
    );
    const lineTo = (context, x, y) => (
        (@native "async ({context,x,y},ctx) => {context.lineTo(x,y)}")(.context = context.raw, .x, .y)
    );
    const stroke = (context) => (
        (@native "async ({context},ctx) => {context.stroke()}")(.context = context.raw)
    
    );
);

const HtmlCanvasElement = newtype {
    .width :: Int32,
    .height :: Int32,
    .getContext :: String -> CanvasRenderingContext2DRaw,
};

const HtmlBodyElement = newtype {
    .innerHTML :: String,
    .appendChild :: HtmlCanvasElement -> (),
};

const HtmlDocumentElement = newtype {
    .body :: HtmlBodyElement,
    .createElement :: String -> HtmlCanvasElement,
};

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
    (@native "async ({document, t},ctx) => document.createElement(t)")(.document, .t)

);
const appendChild = (element, child) => (
    (@native "async ({element, child},ctx) => element.appendChild(child)")(.element, .child)

);

let mut canvas = createElement(document, "canvas");
canvas.width = 400;
canvas.height = 300;
appendChild(document.body, canvas);

const Context = CanvasRenderingContext;
const getContext = (canvas, t) -> CanvasRenderingContext => {
    .raw = (@native "async ({canvas, t},ctx) => canvas.getContext(t)")(.canvas, .t),
};

let mut context = getContext(canvas, "2d");

context |> Context.strokeRect(75, 140, 150, 110);
context |> Context.fillRect(130, 190, 40, 60);
context |> Context.beginPath;
context |> Context.moveTo(50, 140);
context |> Context.lineTo(150, 60);
context |> Context.lineTo(250, 140);
context |> Context.closePath;
context |> Context.stroke;
