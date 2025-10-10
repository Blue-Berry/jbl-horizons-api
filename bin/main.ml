let () =
  let body = Lwt_main.run Jbl_horizons_api.Api.main in
  print_endline ("Received body\n" ^ body)
;;
