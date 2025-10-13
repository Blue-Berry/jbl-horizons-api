open Core

type body =
  { id : int
  ; name : string
  ; designation : string
  ; aliases : string
  }
[@@deriving sexp]

let parse_line (line : string) : body option =
  (* Match lines that start with whitespace followed by digits or negative sign *)
  let stripped = String.strip line in
  if (not (String.is_empty stripped))
     && (Char.is_digit (String.get stripped 0) || Char.equal (String.get stripped 0) '-')
  then (
    (* The format is fixed-width columns separated by multiple spaces:
       ID#      Name                               Designation  IAU/aliases/other
       Columns are roughly at positions: 0-8 (ID), 9-44 (Name), 45-57 (Designation), 58+ (Aliases) *)
    let id_str = String.sub line ~pos:0 ~len:(min 9 (String.length line)) |> String.strip in
    (match Int.of_string_opt id_str with
     | None -> None
     | Some id ->
       let name =
         if String.length line > 9
         then String.sub line ~pos:9 ~len:(min 36 (String.length line - 9)) |> String.strip
         else ""
       in
       let designation =
         if String.length line > 45
         then String.sub line ~pos:45 ~len:(min 13 (String.length line - 45)) |> String.strip
         else ""
       in
       let aliases =
         if String.length line > 58
         then String.sub line ~pos:58 ~len:(String.length line - 58) |> String.strip
         else ""
       in
       Some { id; name; designation; aliases }))
  else None
;;

let parse (input : string) : body list =
  String.split_lines input |> List.filter_map ~f:parse_line
;;