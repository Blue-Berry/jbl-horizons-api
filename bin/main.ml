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

let get_ep sb =
  Jbl_horizons_api.(
    get_body
      (Command.DES sb)
      ~start:(Time_float.now ())
      ~stop:(Time_float.add (Time_float.now ()) Time_float.Span.day)
      Time_float.Span.hour)
;;

(* let mbs = Jbl_horizons_api.(get_mb_list ()) *)
let sbs = Jbl_horizons_api.(get_sb_list SBQuery.[ LT (A, 1.) ])

let () =
  let open Jbl_horizons_api in
  match Store.open_db () with
  | Error e -> print_endline e
  | Ok db ->
    let i = ref 0 in
    List.iter sbs ~f:(fun b ->
      Store.insert_ephemeris db (get_ep b.designation) |> ignore;
      incr i;
      print_endline @@ Int.to_string !i);
    Store.close_db db |> ignore
;;
