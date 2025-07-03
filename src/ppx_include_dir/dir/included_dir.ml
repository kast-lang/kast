open Std

type file = { contents : string }

type entry =
  | File of file
  | Dir of included_dir

and included_dir = { entries : entry StringMap.t }
