open Std
include Kast_core
module Parser = Kast_parser

module Plugins = struct
  module Basic = Kast_basic
  module Macros = Kast_macros
end

module Syntax = struct
  include Kast_syntax
  module Default = Kast_default_syntax
end

let init () =
  Kast_core.init ();
  Plugins.Basic.init ();
  Plugins.Macros.init ()
