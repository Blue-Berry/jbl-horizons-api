open Core
module Command = Api.Command
module SBQuery = Api.SBQuery
module Store = Body_store

let get_mb_list () = Lwt_main.run @@ Api.MBQuery.fetch () |> Parse_mb_query.parse

let get_body (c : Api.Command.t) ~start ~stop step =
  let rec loop i =
    if i > 10 then failwith "Failed to fetch body";
    match Lwt_main.run @@ Api.Body.fetch c ~start ~stop step with
    | Some body -> Parse_body.parse_horizons_data body
    | (exception _) | None ->
      Core_unix.nanosleep 0.2 |> ignore;
      loop (i + 1)
  in
  loop 0
;;

let get_sb_list (q : SBQuery.t) =
  Lwt_main.run @@ Api.SBQuery.fetch q |> Parse_sb_query.parse
;;

let sexp_of_ephemeris_data = Parse_body.sexp_of_ephemeris_data
