open Lwt
open Cohttp
open Cohttp_lwt_unix
open Core

(* let url = *)
(*   "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND='1%3B'&EPHEM_TYPE=vectors&START_TIME='2025-10-10'&STOP_TIME='2025-10-11'&OBJ_DATA=NO&ref_system='icrf'&csv_format=yes" *)
(* ;; *)

let barycenter = "500@0"

let create_url
      (command : string)
      (start : Time_float.t)
      (stop : Time_float.t)
      (step : Time_float.Span.t)
  =
  let params =
    [ "format", "text"
    ; "COMMAND", command
    ; "CENTER", barycenter
    ; "EPHEM_TYPE", "vectors"
    ; "START_TIME", Time_float.to_string_iso8601_basic ~zone:Time_float.Zone.utc start
    ; "STOP_TIME", Time_float.to_string_iso8601_basic ~zone:Time_float.Zone.utc stop
    ; "STEP_SIZE", Time_float.Span.to_string step
    ; "OBJ_DATA", "NO"
    ; "ref_system", "icrf"
    ; "csv_format", "yes"
    ]
    |> List.map ~f:(fun (k, v) -> k ^ "=" ^ v)
    |> String.concat ~sep:"&"
  in
  "https://ssd.jpl.nasa.gov/api/horizons.api?" ^ params
;;

let main =
  Client.get
    (Uri.of_string
       (create_url
          "1%3B"
          (Time_float.now ())
          Time_float.(add (Time_float.now ()) Span.day)
          Time_float.Span.hour))
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body
  |> Cohttp_lwt.Body.to_string
  >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body
;;
