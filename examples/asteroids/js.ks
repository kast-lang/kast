module:

use std;

let HtmlElement :: type = builtin "HtmlElement";

let document = (
    getElementById: builtin "function(id) { return document.getElementById(id); }" :: string -> HtmlElement,
);
