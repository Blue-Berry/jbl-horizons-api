open! Core

(* let mb = Jbl_horizons_api.get_mb_list () |> List.hd_exn *)
(* let () = print_endline mb.name *)

(* let mb = *)
(*   Jbl_horizons_api.( *)
(*     get_body *)
(*       (Command.ID mb.id) *)
(*       ~start:(Time_float.now ()) *)
(*       ~stop:(Time_float.add (Time_float.now ()) Time_float.Span.day) *)
(*       Time_float.Span.hour) *)
(* ;; *)

(* let () = print_s (Jbl_horizons_api.sexp_of_ephemeris_data mb) *)
let sbs = Jbl_horizons_api.(get_sb_list SBQuery.[ LT (IN, 1.) ])

let get_ep sb =
  Jbl_horizons_api.(
    get_body
      (Command.DES sb)
      ~start:(Time_float.now ())
      ~stop:(Time_float.add (Time_float.now ()) Time_float.Span.day)
      Time_float.Span.hour)
;;

let () =
  List.iter sbs ~f:(fun sb ->
    print_s (Jbl_horizons_api.sexp_of_ephemeris_data (get_ep sb.designation)))
;;
