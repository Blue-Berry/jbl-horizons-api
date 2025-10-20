open Core

type bodies = Parse_body.ephemeris_data list

(* Database handle *)
type t = Sqlite3.db

(* Initialize database with schema *)
let create_schema db =
  let sql_metadata =
    {|
    CREATE TABLE IF NOT EXISTS metadata (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      target_body TEXT NOT NULL,
      center_body TEXT NOT NULL,
      start_time TEXT NOT NULL,
      stop_time TEXT NOT NULL,
      step_size TEXT NOT NULL,
      output_units TEXT NOT NULL,
      reference_frame TEXT NOT NULL,
      created_at TEXT DEFAULT (datetime('now')),
      updated_at TEXT DEFAULT (datetime('now'))
    )
  |}
  in
  let sql_osculating_elements =
    {|
    CREATE TABLE IF NOT EXISTS osculating_elements (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      metadata_id INTEGER NOT NULL,
      epoch REAL NOT NULL,
      eccentricity REAL NOT NULL,
      perihelion_distance REAL NOT NULL,
      time_of_perihelion REAL NOT NULL,
      longitude_of_ascending_node REAL NOT NULL,
      argument_of_perihelion REAL NOT NULL,
      inclination REAL NOT NULL,
      FOREIGN KEY (metadata_id) REFERENCES metadata(id) ON DELETE CASCADE
    )
  |}
  in
  let sql_physical_parameters =
    {|
    CREATE TABLE IF NOT EXISTS physical_parameters (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      metadata_id INTEGER NOT NULL,
      gm REAL,
      radius REAL,
      rotation_period REAL,
      absolute_magnitude REAL,
      slope_parameter REAL,
      color_index REAL,
      albedo REAL,
      spectral_type TEXT,
      FOREIGN KEY (metadata_id) REFERENCES metadata(id) ON DELETE CASCADE
    )
  |}
  in
  let sql_state_vectors =
    {|
    CREATE TABLE IF NOT EXISTS state_vectors (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      metadata_id INTEGER NOT NULL,
      jdtdb REAL NOT NULL,
      calendar_date TEXT NOT NULL,
      pos_x REAL NOT NULL,
      pos_y REAL NOT NULL,
      pos_z REAL NOT NULL,
      vel_x REAL NOT NULL,
      vel_y REAL NOT NULL,
      vel_z REAL NOT NULL,
      light_time REAL NOT NULL,
      range REAL NOT NULL,
      range_rate REAL NOT NULL,
      FOREIGN KEY (metadata_id) REFERENCES metadata(id) ON DELETE CASCADE
    )
  |}
  in
  let sql_indices =
    [ {|CREATE INDEX IF NOT EXISTS idx_metadata_target ON metadata(target_body)|}
    ; {|CREATE INDEX IF NOT EXISTS idx_metadata_created ON metadata(created_at)|}
    ; {|CREATE INDEX IF NOT EXISTS idx_state_vectors_metadata ON state_vectors(metadata_id)|}
    ; {|CREATE INDEX IF NOT EXISTS idx_state_vectors_jdtdb ON state_vectors(jdtdb)|}
    ; {|CREATE INDEX IF NOT EXISTS idx_state_vectors_date ON state_vectors(calendar_date)|}
    ]
  in
  let exec_sql sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> Ok ()
    | rc -> Error (Printf.sprintf "SQL error: %s" (Sqlite3.Rc.to_string rc))
  in
  let open Result.Let_syntax in
  let%bind () = exec_sql sql_metadata in
  let%bind () = exec_sql sql_osculating_elements in
  let%bind () = exec_sql sql_physical_parameters in
  let%bind () = exec_sql sql_state_vectors in
  List.fold sql_indices ~init:(Ok ()) ~f:(fun acc sql ->
    let%bind () = acc in
    exec_sql sql)
;;

(* Open or create database *)
let open_db ?(path = "horizons.db") () =
  try
    let db = Sqlite3.db_open path in
    match create_schema db with
    | Ok () -> Ok db
    | Error msg -> Error msg
  with
  | Sqlite3.Error msg -> Error (Printf.sprintf "Failed to open database: %s" msg)
;;

(* Close database *)
let close_db db =
  match Sqlite3.db_close db with
  | true -> Ok ()
  | false -> Error "Failed to close database"
;;

(* Insert ephemeris data *)
let insert_ephemeris db (ephemeris : Parse_body.ephemeris_data) =
  let open Parse_body in
  (* Insert metadata *)
  let metadata_sql =
    {|
    INSERT INTO metadata (target_body, center_body, start_time, stop_time, step_size, output_units, reference_frame)
    VALUES (?, ?, ?, ?, ?, ?, ?)
  |}
  in
  let stmt = Sqlite3.prepare db metadata_sql in
  let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT ephemeris.metadata.target_body) in
  let _ = Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT ephemeris.metadata.center_body) in
  let _ = Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT ephemeris.metadata.start_time) in
  let _ = Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT ephemeris.metadata.stop_time) in
  let _ = Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT ephemeris.metadata.step_size) in
  let _ = Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT ephemeris.metadata.output_units) in
  let _ =
    Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT ephemeris.metadata.reference_frame)
  in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE ->
    let () = Sqlite3.finalize stmt |> ignore in
    let metadata_id = Sqlite3.last_insert_rowid db in
    (* Insert osculating elements if present *)
    let insert_osculating_result =
      match ephemeris.osculating_elements with
      | None -> Ok ()
      | Some oe ->
        let sql =
          {|
          INSERT INTO osculating_elements
          (metadata_id, epoch, eccentricity, perihelion_distance, time_of_perihelion,
           longitude_of_ascending_node, argument_of_perihelion, inclination)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        |}
        in
        let stmt = Sqlite3.prepare db sql in
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT metadata_id) in
        let _ = Sqlite3.bind stmt 2 (Sqlite3.Data.FLOAT oe.epoch) in
        let _ = Sqlite3.bind stmt 3 (Sqlite3.Data.FLOAT oe.eccentricity) in
        let _ = Sqlite3.bind stmt 4 (Sqlite3.Data.FLOAT oe.perihelion_distance) in
        let _ = Sqlite3.bind stmt 5 (Sqlite3.Data.FLOAT oe.time_of_perihelion) in
        let _ =
          Sqlite3.bind stmt 6 (Sqlite3.Data.FLOAT oe.longitude_of_ascending_node)
        in
        let _ = Sqlite3.bind stmt 7 (Sqlite3.Data.FLOAT oe.argument_of_perihelion) in
        let _ = Sqlite3.bind stmt 8 (Sqlite3.Data.FLOAT oe.inclination) in
        (match Sqlite3.step stmt with
         | Sqlite3.Rc.DONE ->
           let () = Sqlite3.finalize stmt |> ignore in
           Ok ()
         | rc ->
           let () = Sqlite3.finalize stmt |> ignore in
           Error (Printf.sprintf "Failed to insert osculating elements: %s" (Sqlite3.Rc.to_string rc)))
    in
    (* Insert physical parameters if present *)
    let insert_physical_result =
      match ephemeris.physical_params with
      | None -> Ok ()
      | Some pp ->
        let sql =
          {|
          INSERT INTO physical_parameters
          (metadata_id, gm, radius, rotation_period, absolute_magnitude,
           slope_parameter, color_index, albedo, spectral_type)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        |}
        in
        let stmt = Sqlite3.prepare db sql in
        let bind_opt idx opt_val =
          match opt_val with
          | None -> Sqlite3.bind stmt idx Sqlite3.Data.NULL
          | Some v -> Sqlite3.bind stmt idx (Sqlite3.Data.FLOAT v)
        in
        let bind_opt_text idx opt_val =
          match opt_val with
          | None -> Sqlite3.bind stmt idx Sqlite3.Data.NULL
          | Some v -> Sqlite3.bind stmt idx (Sqlite3.Data.TEXT v)
        in
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT metadata_id) in
        let _ = bind_opt 2 pp.gm in
        let _ = bind_opt 3 pp.radius in
        let _ = bind_opt 4 pp.rotation_period in
        let _ = bind_opt 5 pp.absolute_magnitude in
        let _ = bind_opt 6 pp.slope_parameter in
        let _ = bind_opt 7 pp.color_index in
        let _ = bind_opt 8 pp.albedo in
        let _ = bind_opt_text 9 pp.spectral_type in
        (match Sqlite3.step stmt with
         | Sqlite3.Rc.DONE ->
           let () = Sqlite3.finalize stmt |> ignore in
           Ok ()
         | rc ->
           let () = Sqlite3.finalize stmt |> ignore in
           Error (Printf.sprintf "Failed to insert physical parameters: %s" (Sqlite3.Rc.to_string rc)))
    in
    (* Insert state vectors *)
    let insert_vectors_result =
      let sql =
        {|
        INSERT INTO state_vectors
        (metadata_id, jdtdb, calendar_date, pos_x, pos_y, pos_z, vel_x, vel_y, vel_z,
         light_time, range, range_rate)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      |}
      in
      List.fold ephemeris.state_vectors ~init:(Ok ()) ~f:(fun acc sv ->
        match acc with
        | Error _ as e -> e
        | Ok () ->
          let stmt = Sqlite3.prepare db sql in
          let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT metadata_id) in
          let _ = Sqlite3.bind stmt 2 (Sqlite3.Data.FLOAT sv.jdtdb) in
          let _ = Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT sv.calendar_date) in
          let _ = Sqlite3.bind stmt 4 (Sqlite3.Data.FLOAT sv.position.x) in
          let _ = Sqlite3.bind stmt 5 (Sqlite3.Data.FLOAT sv.position.y) in
          let _ = Sqlite3.bind stmt 6 (Sqlite3.Data.FLOAT sv.position.z) in
          let _ = Sqlite3.bind stmt 7 (Sqlite3.Data.FLOAT sv.velocity.x) in
          let _ = Sqlite3.bind stmt 8 (Sqlite3.Data.FLOAT sv.velocity.y) in
          let _ = Sqlite3.bind stmt 9 (Sqlite3.Data.FLOAT sv.velocity.z) in
          let _ = Sqlite3.bind stmt 10 (Sqlite3.Data.FLOAT sv.light_time) in
          let _ = Sqlite3.bind stmt 11 (Sqlite3.Data.FLOAT sv.range) in
          let _ = Sqlite3.bind stmt 12 (Sqlite3.Data.FLOAT sv.range_rate) in
          (match Sqlite3.step stmt with
           | Sqlite3.Rc.DONE ->
             let () = Sqlite3.finalize stmt |> ignore in
             Ok ()
           | rc ->
             let () = Sqlite3.finalize stmt |> ignore in
             Error
               (Printf.sprintf
                  "Failed to insert state vector: %s"
                  (Sqlite3.Rc.to_string rc))))
    in
    let open Result.Let_syntax in
    let%bind () = insert_osculating_result in
    let%bind () = insert_physical_result in
    let%bind () = insert_vectors_result in
    Ok metadata_id
  | rc ->
    let () = Sqlite3.finalize stmt |> ignore in
    Error (Printf.sprintf "Failed to insert metadata: %s" (Sqlite3.Rc.to_string rc))
;;

(* Query all bodies *)
let query_all_bodies db =
  let sql = {|SELECT id, target_body, center_body FROM metadata ORDER BY created_at DESC|} in
  let stmt = Sqlite3.prepare db sql in
  let rec collect_rows acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
      let id = Sqlite3.column stmt 0 in
      let target = Sqlite3.column stmt 1 in
      let center = Sqlite3.column stmt 2 in
      (match id, target, center with
       | Sqlite3.Data.INT id, Sqlite3.Data.TEXT target, Sqlite3.Data.TEXT center ->
         collect_rows ((id, target, center) :: acc)
       | _ -> collect_rows acc)
    | Sqlite3.Rc.DONE ->
      let () = Sqlite3.finalize stmt |> ignore in
      Ok (List.rev acc)
    | rc ->
      let () = Sqlite3.finalize stmt |> ignore in
      Error (Printf.sprintf "Query failed: %s" (Sqlite3.Rc.to_string rc))
  in
  collect_rows []
;;

(* Query body by target name *)
let query_by_target db target_body =
  let sql =
    {|SELECT id, target_body, center_body, start_time, stop_time, step_size, output_units, reference_frame
      FROM metadata WHERE target_body = ? ORDER BY created_at DESC LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT target_body) in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
    let id = Sqlite3.column stmt 0 in
    (match id with
     | Sqlite3.Data.INT metadata_id ->
       let target = Sqlite3.column stmt 1 in
       let center = Sqlite3.column stmt 2 in
       let start_time = Sqlite3.column stmt 3 in
       let stop_time = Sqlite3.column stmt 4 in
       let step_size = Sqlite3.column stmt 5 in
       let output_units = Sqlite3.column stmt 6 in
       let reference_frame = Sqlite3.column stmt 7 in
       let () = Sqlite3.finalize stmt |> ignore in
       (match target, center, start_time, stop_time, step_size, output_units, reference_frame with
        | ( Sqlite3.Data.TEXT target
          , Sqlite3.Data.TEXT center
          , Sqlite3.Data.TEXT start_time
          , Sqlite3.Data.TEXT stop_time
          , Sqlite3.Data.TEXT step_size
          , Sqlite3.Data.TEXT output_units
          , Sqlite3.Data.TEXT reference_frame ) -> Ok (Some (metadata_id, target, center, start_time, stop_time, step_size, output_units, reference_frame))
        | _ -> Ok None)
     | _ ->
       let () = Sqlite3.finalize stmt |> ignore in
       Ok None)
  | Sqlite3.Rc.DONE ->
    let () = Sqlite3.finalize stmt |> ignore in
    Ok None
  | rc ->
    let () = Sqlite3.finalize stmt |> ignore in
    Error (Printf.sprintf "Query failed: %s" (Sqlite3.Rc.to_string rc))
;;

(* Query state vectors for a metadata_id with optional time range *)
let query_state_vectors db metadata_id ?start_jd ?end_jd () =
  let base_sql =
    {|SELECT jdtdb, calendar_date, pos_x, pos_y, pos_z, vel_x, vel_y, vel_z, light_time, range, range_rate
      FROM state_vectors WHERE metadata_id = ?|}
  in
  let sql =
    match start_jd, end_jd with
    | None, None -> base_sql ^ " ORDER BY jdtdb"
    | Some _, None -> base_sql ^ " AND jdtdb >= ? ORDER BY jdtdb"
    | None, Some _ -> base_sql ^ " AND jdtdb <= ? ORDER BY jdtdb"
    | Some _, Some _ -> base_sql ^ " AND jdtdb >= ? AND jdtdb <= ? ORDER BY jdtdb"
  in
  let stmt = Sqlite3.prepare db sql in
  let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT metadata_id) in
  let bind_idx =
    match start_jd, end_jd with
    | None, None -> 2
    | Some jd, None ->
      let _ = Sqlite3.bind stmt 2 (Sqlite3.Data.FLOAT jd) in
      3
    | None, Some jd ->
      let _ = Sqlite3.bind stmt 2 (Sqlite3.Data.FLOAT jd) in
      3
    | Some start, Some end_ ->
      let _ = Sqlite3.bind stmt 2 (Sqlite3.Data.FLOAT start) in
      let _ = Sqlite3.bind stmt 3 (Sqlite3.Data.FLOAT end_) in
      4
  in
  let _ = bind_idx in
  let rec collect_rows acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
      let jdtdb = Sqlite3.column stmt 0 in
      let calendar_date = Sqlite3.column stmt 1 in
      let pos_x = Sqlite3.column stmt 2 in
      let pos_y = Sqlite3.column stmt 3 in
      let pos_z = Sqlite3.column stmt 4 in
      let vel_x = Sqlite3.column stmt 5 in
      let vel_y = Sqlite3.column stmt 6 in
      let vel_z = Sqlite3.column stmt 7 in
      let light_time = Sqlite3.column stmt 8 in
      let range = Sqlite3.column stmt 9 in
      let range_rate = Sqlite3.column stmt 10 in
      (match
         ( jdtdb
         , calendar_date
         , pos_x
         , pos_y
         , pos_z
         , vel_x
         , vel_y
         , vel_z
         , light_time
         , range
         , range_rate )
       with
       | ( Sqlite3.Data.FLOAT jdtdb
         , Sqlite3.Data.TEXT calendar_date
         , Sqlite3.Data.FLOAT pos_x
         , Sqlite3.Data.FLOAT pos_y
         , Sqlite3.Data.FLOAT pos_z
         , Sqlite3.Data.FLOAT vel_x
         , Sqlite3.Data.FLOAT vel_y
         , Sqlite3.Data.FLOAT vel_z
         , Sqlite3.Data.FLOAT light_time
         , Sqlite3.Data.FLOAT range
         , Sqlite3.Data.FLOAT range_rate ) ->
         let sv : Parse_body.state_vector =
           { jdtdb
           ; calendar_date
           ; position = { x = pos_x; y = pos_y; z = pos_z }
           ; velocity = { x = vel_x; y = vel_y; z = vel_z }
           ; light_time
           ; range
           ; range_rate
           }
         in
         collect_rows (sv :: acc)
       | _ -> collect_rows acc)
    | Sqlite3.Rc.DONE ->
      let () = Sqlite3.finalize stmt |> ignore in
      Ok (List.rev acc)
    | rc ->
      let () = Sqlite3.finalize stmt |> ignore in
      Error (Printf.sprintf "Query failed: %s" (Sqlite3.Rc.to_string rc))
  in
  collect_rows []
;;

(* Delete body data by metadata_id *)
let delete_body db metadata_id =
  let sql = {|DELETE FROM metadata WHERE id = ?|} in
  let stmt = Sqlite3.prepare db sql in
  let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT metadata_id) in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE ->
    let () = Sqlite3.finalize stmt |> ignore in
    Ok ()
  | rc ->
    let () = Sqlite3.finalize stmt |> ignore in
    Error (Printf.sprintf "Delete failed: %s" (Sqlite3.Rc.to_string rc))
;;

(* Query bodies created within a time range using SQLite datetime functions *)
let query_by_time_range db ~days_ago =
  let sql =
    {|SELECT id, target_body, center_body, created_at
      FROM metadata
      WHERE datetime(created_at) >= datetime('now', '-' || ? || ' days')
      ORDER BY created_at DESC|}
  in
  let stmt = Sqlite3.prepare db sql in
  let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int days_ago)) in
  let rec collect_rows acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
      let id = Sqlite3.column stmt 0 in
      let target = Sqlite3.column stmt 1 in
      let center = Sqlite3.column stmt 2 in
      let created = Sqlite3.column stmt 3 in
      (match id, target, center, created with
       | ( Sqlite3.Data.INT id
         , Sqlite3.Data.TEXT target
         , Sqlite3.Data.TEXT center
         , Sqlite3.Data.TEXT created ) -> collect_rows ((id, target, center, created) :: acc)
       | _ -> collect_rows acc)
    | Sqlite3.Rc.DONE ->
      let () = Sqlite3.finalize stmt |> ignore in
      Ok (List.rev acc)
    | rc ->
      let () = Sqlite3.finalize stmt |> ignore in
      Error (Printf.sprintf "Query failed: %s" (Sqlite3.Rc.to_string rc))
  in
  collect_rows []
;;

(* Count state vectors for a body *)
let count_state_vectors db metadata_id =
  let sql = {|SELECT COUNT(*) FROM state_vectors WHERE metadata_id = ?|} in
  let stmt = Sqlite3.prepare db sql in
  let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT metadata_id) in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
    let count = Sqlite3.column stmt 0 in
    let () = Sqlite3.finalize stmt |> ignore in
    (match count with
     | Sqlite3.Data.INT count -> Ok count
     | _ -> Error "Unexpected result type")
  | rc ->
    let () = Sqlite3.finalize stmt |> ignore in
    Error (Printf.sprintf "Count failed: %s" (Sqlite3.Rc.to_string rc))
;;