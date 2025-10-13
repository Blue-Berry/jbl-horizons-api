open Core
module Command = Api.Command
module SBQuery = Api.SBQuery

let get_mb_list () = Lwt_main.run @@ Api.MBQuery.fetch () |> Parse_mb_query.parse

let get_body (c : Api.Command.t) ~start ~stop step =
  Lwt_main.run @@ Api.Body.fetch c ~start ~stop step |> Parse_body.parse_horizons_data
;;

let get_sb_list (q : SBQuery.t) =
  Lwt_main.run @@ Api.SBQuery.fetch q |> Parse_sb_query.parse
;;

let sexp_of_ephemeris_data = Parse_body.sexp_of_ephemeris_data
