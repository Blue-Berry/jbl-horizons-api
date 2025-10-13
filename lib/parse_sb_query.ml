open Core

type body =
  { designation : string
  ; name : string
  }
[@@deriving sexp]

let parse_line (line : string) : body option =
  (* Match lines that start with whitespace followed by digits (record number) *)
  let stripped = String.strip line in
  if (not (String.is_empty stripped)) && Char.is_digit (String.get stripped 0)
  then (
    (* The format is: Record# Epoch-yr Primary_Desig Name ... *)
    let parts =
      String.split_on_chars line ~on:[ ' '; '\t' ]
      |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    match parts with
    | _ :: _ :: desig1 :: desig2 :: name :: _ ->
      (* Primary designation is two parts (e.g., "A847 NA" or "2025 TT") *)
      (* Name follows the designation *)
      Some { designation = desig1 ^ " " ^ desig2; name }
    | _ -> None)
  else None
;;

let parse (input : string) : body list =
  String.split_lines input |> List.filter_map ~f:parse_line
;;
