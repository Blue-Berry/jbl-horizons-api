open Lwt
open Cohttp
open Cohttp_lwt_unix
open Core

module Body = struct
  (* let url = *)
  (*   "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND='1%3B'&EPHEM_TYPE=vectors&START_TIME='2025-10-10'&STOP_TIME='2025-10-11'&OBJ_DATA=NO&ref_system='icrf'&csv_format=yes" *)
  (* ;; *)

  let barycenter = "500@0"

  let furnish s : string =
    String.substr_replace_all ~pattern:" " ~with_:"%20" s
    |> String.substr_replace_all ~pattern:";" ~with_:"%3B"
  ;;

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
      |> furnish
    in
    "https://ssd.jpl.nasa.gov/api/horizons.api?" ^ params
  ;;

  let fetch
        (command : string)
        (start : Time_float.t)
        (stop : Time_float.t)
        (step : Time_float.Span.t)
    : string Lwt.t
    =
    Client.get (Uri.of_string (create_url command start stop step))
    >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    if code <> 200
    then Lwt.fail_with (Printf.sprintf "HTTP request failed with code %d" code)
    else body |> Cohttp_lwt.Body.to_string
  ;;
end

module SBQuery = struct
  (*    Search for small-bodies with following keywords (R=real, I=integer, C=char). *)
  (* Use comparisons from the set { <, >, <>, = }. Separate each field with a semi- *)
  (* colon. Example search formulation: *)

  (*                    A < 2.5; IN > 7.8; STYP = S; GM <> 0; *)

  (* The first group of keywords are common to asteroids AND comets: *)

  (*  Type     Keyword     Description *)
  (*  ----     -------     ----------- *)
  (*   C       NAME ...... Asteroid OR comet name fragment *)
  (*   C       DES ....... Object designation *)
  (*   R       EPOCH ..... Julian Date of osculating elements *)
  (*   R       CALEPO .... Calendar date of osc. elements; YYYYMMDD.ffff *)
  (*   R       A ......... Semi-major axis (AU) *)
  (*   R       EC ........ Eccentricity *)
  (*   R       IN ........ Inclination of orbit plane (DEG) wrt ecliptic *)
  (*   R       OM ........ Longitude of Ascending Node (DEG) wrt ecliptic/equinox *)
  (*   R       W ......... Argument of Perihelion (DEG) wrt ecliptic/equinox *)
  (*   R       TP ........ Perihelion Julian Date *)
  (*   R       CALTP ..... Perihelion calendar date; YYYYMMDD.ffff *)
  (*   R       MA ........ Mean anomaly (DEG) *)
  (*   R       PER ....... Orbital period (YRS) *)
  (*   R       RAD ....... Object radius (KM) *)
  (*   R       GM ........ Object GM (KM^3/S^2), only a few are known *)
  (*   R       QR ........ Perihelion distance (AU) *)
  (*   R       ADIST ..... Aphelion distance (AU) *)
  (*   R       ANGMOM .... Specific angular momentum (AU^2/DAY) *)
  (*   R       N ......... Mean motion (DEG/DAY) *)
  (*   R       DAN ....... Heliocentric dist. (AU) of ascending node *)
  (*   R       DDN ....... Heliocentric dist. (AU) of descending node *)
  (*   R       L ......... Ecliptic longitude of perihelion (DEG) *)
  (*   R       B ......... Ecliptic latitude of perihelion (DEG) *)
  (*   I       NOBS ...... Number of astrometric determinations in solution *)

  (* For example, *)

  (*     "A < 2.5; IN > 10; AST;"        match parameters against asteroids ONLY. *)
  (*     "A < 2.5; IN > 10; AST; LIST;"  match AND display values of the parameters. *)

  (* curl -s "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND='A%20<%202.5%3B%20IN%20>%2010%3B%20AST%3B'" *)

  type 'a param =
    | NAME : string param
    | DES : string param
    | EPOCH : float param
    | CALEPO : float param
    | A : float param
    | EC : float param
    | IN : float param
    | OM : float param
    | W : float param
    | TP : float param
    | CALTP : float param
    | MA : float param
    | PER : float param
    | RAD : float param
    | GM : float param
    | QR : float param
    | ADIST : float param
    | ANGMOM : float param
    | N : float param
    | DAN : float param
    | DDN : float param
    | L : float param
    | B : float param
    | NOBS : int param [@deriving sexp]

  type comp =
    | GT : 'a * 'a param -> comp
    | LT : 'a * 'a param -> comp
    | NOT : 'a * 'a param -> comp
    | EQ : 'a * 'a param -> comp [@deriving sexp]

  type t = (comp list[@deriving sexp])

  let param_to_string : type a. a param -> string = function
    | NAME -> "NAME"
    | DES -> "DES"
    | EPOCH -> "EPOCH"
    | CALEPO -> "CALEPO"
    | A -> "A"
    | EC -> "EC"
    | IN -> "IN"
    | OM -> "OM"
    | W -> "W"
    | TP -> "TP"
    | CALTP -> "CALTP"
    | MA -> "MA"
    | PER -> "PER"
    | RAD -> "RAD"
    | GM -> "GM"
    | QR -> "QR"
    | ADIST -> "ADIST"
    | ANGMOM -> "ANGMOM"
    | N -> "N"
    | DAN -> "DAN"
    | DDN -> "DDN"
    | L -> "L"
    | B -> "B"
    | NOBS -> "NOBS"
  ;;

  let value_to_string : type a. a -> a param -> string =
    fun v p ->
    match p with
    | NAME -> v
    | DES -> v
    | EPOCH -> Float.to_string v
    | CALEPO -> Float.to_string v
    | A -> Float.to_string v
    | EC -> Float.to_string v
    | IN -> Float.to_string v
    | OM -> Float.to_string v
    | W -> Float.to_string v
    | TP -> Float.to_string v
    | CALTP -> Float.to_string v
    | MA -> Float.to_string v
    | PER -> Float.to_string v
    | RAD -> Float.to_string v
    | GM -> Float.to_string v
    | QR -> Float.to_string v
    | ADIST -> Float.to_string v
    | ANGMOM -> Float.to_string v
    | N -> Float.to_string v
    | DAN -> Float.to_string v
    | DDN -> Float.to_string v
    | L -> Float.to_string v
    | B -> Float.to_string v
    | NOBS -> Int.to_string v
  ;;

  let comp_to_string : comp -> string = function
    | GT (v, p) -> param_to_string p ^ " > " ^ value_to_string v p
    | LT (v, p) -> param_to_string p ^ " < " ^ value_to_string v p
    | NOT (v, p) -> param_to_string p ^ " <> " ^ value_to_string v p
    | EQ (v, p) -> param_to_string p ^ " = " ^ value_to_string v p
  ;;

  let furnish s : string =
    String.substr_replace_all ~pattern:" " ~with_:"%20" s
    |> String.substr_replace_all ~pattern:";" ~with_:"%3B"
  ;;

  let to_query_string (t : t) : string =
    List.map ~f:comp_to_string t |> String.concat ~sep:"; " |> fun s -> s ^ ";" |> furnish
  ;;

  let create_url (t : t) : string =
    let command = to_query_string t in
    "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND='" ^ command ^ "'"
  ;;

  let fetch (t : t) : string Lwt.t =
    Client.get (Uri.of_string (create_url t))
    >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    if code <> 200
    then Lwt.fail_with (Printf.sprintf "HTTP request failed with code %d" code)
    else body |> Cohttp_lwt.Body.to_string
  ;;
end

module MBQuery = struct
  let url = "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND='MB'"

  let fetch () : string Lwt.t =
    Client.get (Uri.of_string url)
    >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    if code <> 200
    then Lwt.fail_with (Printf.sprintf "HTTP request failed with code %d" code)
    else body |> Cohttp_lwt.Body.to_string
  ;;
end

module Command = struct
  type t =
    | ID of int
    | DES of string
    | IAU of int
    | Name of string

  let to_string t =
    let furnish s : string =
      String.substr_replace_all ~pattern:" " ~with_:"%20" s
      |> String.substr_replace_all ~pattern:";" ~with_:"%3B"
    in
    let to_string = function
      | ID id -> Int.to_string id
      | DES s -> sprintf "DES=%s;" s
      | IAU id -> sprintf "%d;" id
      | Name s -> sprintf "%s;" s
    in
    to_string t |> furnish
  ;;
end
