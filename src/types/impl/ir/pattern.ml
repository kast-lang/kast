open Kast_common

module Shape = struct
  module type Deps = sig
    module Pattern : sig
      type t [@@deriving eq, ord]
    end

    module Binding : sig
      type t [@@deriving eq, ord]
    end
  end

  module type S = sig
    module Deps : Deps

    type tuple = Deps.Pattern.t tuple_of [@@deriving eq, ord]

    and variant = {
      label : Label.t;
      label_span : Span.t;
      value : Deps.Pattern.t option;
    }
    [@@deriving eq, ord]

    and binding = {
      by_ref : bool;
      binding : Deps.Binding.t;
    }
    [@@deriving eq, ord]

    and t =
      | Placeholder
      | Ref of Deps.Pattern.t
      | Unit
      | Binding of binding
      | Tuple of tuple
      | Variant of variant
      | Error
    [@@deriving eq, ord]
  end

  module Make (Deps : Deps) : S with module Deps = Deps = struct
    module Deps = Deps

    type tuple = Deps.Pattern.t tuple_of [@@deriving eq, ord]

    and variant = {
      label : Label.t;
      label_span : Span.t;
      value : Deps.Pattern.t option;
    }
    [@@deriving eq, ord]

    and binding = {
      by_ref : bool;
      binding : Deps.Binding.t;
    }
    [@@deriving eq, ord]

    and t =
      | Placeholder
      | Ref of Deps.Pattern.t
      | Unit
      | Binding of binding
      | Tuple of tuple
      | Variant of variant
      | Error
    [@@deriving eq, ord]
  end
end

module T = struct
  module type Deps = sig
    module PatternShape : sig
      type t [@@deriving eq, ord]
    end

    module IrData : sig
      type t [@@deriving eq, ord]
    end
  end

  module type S = sig
    module Deps : Deps

    type t = {
      shape : Deps.PatternShape.t;
      data : Deps.IrData.t;
    }
    [@@deriving eq, ord]
  end

  module Make (Deps : Deps) : S with module Deps = Deps = struct
    module Deps = Deps

    type t = {
      shape : Deps.PatternShape.t;
      data : Deps.IrData.t;
    }
    [@@deriving eq, ord]
  end
end

include T
