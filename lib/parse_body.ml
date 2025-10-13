open Core

(* JPL Horizons ephemeris data types and parser *)

type km [@@deriving sexp, equal]
type km_per_sec [@@deriving sexp, equal]
type sec [@@deriving sexp, equal]

type 'a vector3 =
  { x : float
  ; y : float
  ; z : float
  }
[@@deriving sexp, equal]

type state_vector =
  { jdtdb : float
  ; calendar_date : string
  ; position : km vector3 (* km *)
  ; velocity : km_per_sec vector3 (* km/sec *)
  ; light_time : float (* sec *)
  ; range : float (* km *)
  ; range_rate : float (* km/sec *)
  }
[@@deriving sexp, equal]

type osculating_elements =
  { epoch : float
  ; eccentricity : float
  ; perihelion_distance : float
  ; time_of_perihelion : float
  ; longitude_of_ascending_node : float
  ; argument_of_perihelion : float
  ; inclination : float
  }
[@@deriving sexp, equal]

type physical_parameters =
  { gm : float option (* km^3/s^2 *)
  ; radius : float option (* km *)
  ; rotation_period : float option (* hours *)
  ; absolute_magnitude : float option
  ; slope_parameter : float option
  ; color_index : float option
  ; albedo : float option
  ; spectral_type : string option
  }
[@@deriving sexp, equal]

type metadata =
  { target_body : string
  ; center_body : string
  ; start_time : string
  ; stop_time : string
  ; step_size : string
  ; output_units : string
  ; reference_frame : string
  }
[@@deriving sexp, equal]

type ephemeris_data =
  { metadata : metadata
  ; osculating_elements : osculating_elements option
  ; physical_params : physical_parameters option
  ; state_vectors : state_vector list
  }
[@@deriving sexp, equal]

let extract_value line =
  match String.lsplit2 line ~on:':' with
  | Some (_, rest) -> String.strip rest
  | None -> ""
;;

let extract_field_value field_prefix str =
  (* Look for " FIELD=" or "^FIELD=" pattern to avoid matching inside words *)
  let pattern = field_prefix ^ "=" in
  let rec find_pattern str start_pos =
    match String.substr_index ~pos:start_pos str ~pattern with
    | None -> None
    | Some idx ->
      (* Check if preceded by space or at start of line *)
      let valid_prefix =
        idx = 0 || (idx > 0 && Char.equal (String.get str (idx - 1)) ' ')
      in
      if valid_prefix then Some idx else find_pattern str (idx + 1)
  in
  match find_pattern str 0 with
  | None -> None
  | Some idx ->
    let after_eq = idx + String.length pattern in
    let rest = String.drop_prefix str after_eq |> String.strip in
    (* Split on whitespace and take first token, handling scientific notation *)
    let tokens =
      String.split rest ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    (match tokens with
     | [] -> None
     | first :: _ ->
       (* Stop at '!' which indicates comments *)
       let value =
         match String.index first '!' with
         | Some pos -> String.prefix first pos
         | None -> first
       in
       Float.of_string_opt (String.strip value))
;;

let extract_string_field field_prefix str =
  (* Look for " FIELD=" or "^FIELD=" pattern to avoid matching inside words *)
  let pattern = field_prefix ^ "=" in
  let rec find_pattern str start_pos =
    match String.substr_index ~pos:start_pos str ~pattern with
    | None -> None
    | Some idx ->
      (* Check if preceded by space or at start of line *)
      let valid_prefix =
        idx = 0 || (idx > 0 && Char.equal (String.get str (idx - 1)) ' ')
      in
      if valid_prefix then Some idx else find_pattern str (idx + 1)
  in
  match find_pattern str 0 with
  | None -> None
  | Some idx ->
    let after_eq = idx + String.length pattern in
    let rest = String.drop_prefix str after_eq |> String.strip in
    (* Take first token *)
    let tokens =
      String.split rest ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    (match tokens with
     | [] -> None
     | first :: _ ->
       let v = String.strip first in
       if String.is_empty v then None else Some v)
;;

(* Parse metadata from header *)
let parse_metadata lines =
  let rec find_field lines field =
    match lines with
    | [] -> None
    | line :: rest ->
      if String.is_prefix line ~prefix:field
      then Some (extract_value line)
      else find_field rest field
  in
  let target = Option.value (find_field lines "Target body name") ~default:"" in
  let center = Option.value (find_field lines "Center body name") ~default:"" in
  let start_time = Option.value (find_field lines "Start time") ~default:"" in
  let stop_time = Option.value (find_field lines "Stop  time") ~default:"" in
  let step_size = Option.value (find_field lines "Step-size") ~default:"" in
  let units = Option.value (find_field lines "Output units") ~default:"" in
  let frame = Option.value (find_field lines "Reference frame") ~default:"" in
  { target_body = target
  ; center_body = center
  ; start_time
  ; stop_time
  ; step_size
  ; output_units = units
  ; reference_frame = frame
  }
;;

(* Parse osculating elements *)
let parse_osculating_elements lines =
  (* Find all the fields across all lines *)
  let epoch = List.find_map lines ~f:(extract_field_value "EPOCH") in
  let ec = List.find_map lines ~f:(extract_field_value "EC") in
  let qr = List.find_map lines ~f:(extract_field_value "QR") in
  let tp = List.find_map lines ~f:(extract_field_value "TP") in
  let om = List.find_map lines ~f:(extract_field_value "OM") in
  let w = List.find_map lines ~f:(extract_field_value "W") in
  let inc = List.find_map lines ~f:(extract_field_value "IN") in
  match epoch, ec, qr, tp, om, w, inc with
  | Some ep, Some e, Some q, Some t, Some o, Some ww, Some i ->
    Some
      { epoch = ep
      ; eccentricity = e
      ; perihelion_distance = q
      ; time_of_perihelion = t
      ; longitude_of_ascending_node = o
      ; argument_of_perihelion = ww
      ; inclination = i
      }
  | _ -> None
;;

(* Parse physical parameters *)
let parse_physical_parameters lines =
  (* Only look in lines that mention "physical parameters" or contain GM/RAD *)
  let physical_lines =
    List.filter lines ~f:(fun line ->
      String.is_substring line ~substring:"physical parameters"
      || String.is_substring line ~substring:"GM="
      || String.is_substring line ~substring:"H="
      || String.is_substring line ~substring:"ALBEDO=")
  in
  let gm = List.find_map physical_lines ~f:(extract_field_value "GM") in
  let radius = List.find_map physical_lines ~f:(extract_field_value "RAD") in
  let rotation_period = List.find_map physical_lines ~f:(extract_field_value "ROTPER") in
  let absolute_magnitude = List.find_map physical_lines ~f:(extract_field_value "H") in
  let slope_parameter = List.find_map physical_lines ~f:(extract_field_value "G") in
  let color_index = List.find_map physical_lines ~f:(extract_field_value "B-V") in
  let albedo = List.find_map physical_lines ~f:(extract_field_value "ALBEDO") in
  let spectral_type = List.find_map physical_lines ~f:(extract_string_field "STYP") in
  match gm with
  | None -> None
  | Some _ ->
    Some
      { gm
      ; radius
      ; rotation_period
      ; absolute_magnitude
      ; slope_parameter
      ; color_index
      ; albedo
      ; spectral_type
      }
;;

(* Parse state vector line *)
let parse_state_vector line =
  let parts = String.split line ~on:',' in
  match parts with
  | jd :: date :: x :: y :: z :: vx :: vy :: vz :: lt :: rg :: rr :: _ ->
    (try
       let jd_val = Float.of_string (String.strip jd) in
       let date_str = String.strip date in
       let x_val = Float.of_string (String.strip x) in
       let y_val = Float.of_string (String.strip y) in
       let z_val = Float.of_string (String.strip z) in
       let vx_val = Float.of_string (String.strip vx) in
       let vy_val = Float.of_string (String.strip vy) in
       let vz_val = Float.of_string (String.strip vz) in
       let lt_val = Float.of_string (String.strip lt) in
       let rg_val = Float.of_string (String.strip rg) in
       let rr_val = Float.of_string (String.strip rr) in
       Some
         { jdtdb = jd_val
         ; calendar_date = date_str
         ; position = { x = x_val; y = y_val; z = z_val }
         ; velocity = { x = vx_val; y = vy_val; z = vz_val }
         ; light_time = lt_val
         ; range = rg_val
         ; range_rate = rr_val
         }
     with
     | _ -> None)
  | _ -> None
;;

(* Main parser *)
let parse_horizons_data input =
  let lines = String.split_lines input in
  (* Extract metadata *)
  let metadata = parse_metadata lines in
  (* Find osculating elements *)
  let elements = parse_osculating_elements lines in
  (* Find physical parameters *)
  let physical = parse_physical_parameters lines in
  (* Extract state vectors between $$SOE and $$EOE *)
  let rec extract_vectors in_data lines acc =
    match lines with
    | [] -> List.rev acc
    | line :: rest ->
      let trimmed = String.strip line in
      if String.equal trimmed "$$SOE"
      then extract_vectors true rest acc
      else if String.equal trimmed "$$EOE"
      then List.rev acc
      else if in_data && not (String.is_empty trimmed)
      then (
        let first_char = String.get trimmed 0 in
        if Char.is_digit first_char
        then (
          match parse_state_vector line with
          | Some sv -> extract_vectors in_data rest (sv :: acc)
          | None -> extract_vectors in_data rest acc)
        else extract_vectors in_data rest acc)
      else extract_vectors in_data rest acc
  in
  let vectors = extract_vectors false lines [] in
  { metadata
  ; osculating_elements = elements
  ; physical_params = physical
  ; state_vectors = vectors
  }
;;
