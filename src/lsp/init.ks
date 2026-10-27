use (import "./common.ks").*;
use (import "./state.ks").*;
use (import "./semantic_tokens.ks").*;

module:

const initialize = (state :: &mut State, request :: Json.t) -> Json.t => (
    let init_json = @eval (
        std.fs.read_file(&format!("\(std.path.dirname(__FILE__))/init.json") |> as_str)
    );
    let mut response = Json.parse(&init_json |> as_str) |> Result.unwrap;
    let :Object ref mut fields = response;
    let (&mut (:Object ref mut capabilities)) = fields
        |> OrdMap.get_mut(&RcStr.allocate("capabilities"))
        |> Option.unwrap;
    let (&mut (:Object ref mut semantic_tokens_provider)) = capabilities
        |> OrdMap.get_mut(&RcStr.allocate("semanticTokensProvider"))
        |> Option.unwrap;
    let legend = semantic_tokens_provider
        |> OrdMap.get_mut(&RcStr.allocate("legend"))
        |> Option.unwrap;
    legend^ = semantic_tokens.legend();
    response
);
