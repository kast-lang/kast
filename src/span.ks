use (import "./uri.ks").*;
use (import "./position.ks").*;

module:

const Span = newtype {
    .start :: Position,
    .end :: Position,
    .uri :: Uri,
};