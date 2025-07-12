open Std
include Kast_core
module Parser = Kast_parser

module Plugins = struct
  module BasicTypes = Kast_basic_types
  module Macros = Kast_macros
end

module Syntax = struct
  include Kast_syntax
  module Default = Kast_default_syntax
end

let init () =
  Kast_core.init ();
  Plugins.BasicTypes.init ();
  Plugins.Macros.init ()
