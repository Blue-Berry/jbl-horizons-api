open! Core
module Parse = Parse_body

let input =
  {|
   *******************************************************************************
Ephemeris / API_USER Fri Oct 10 02:54:29 2025 Pasadena, USA      / Horizons
*******************************************************************************
Target body name: 1 Ceres (A801 AA)               {source: JPL#48}
Center body name: Solar System Barycenter (0)     {source: DE441}
Center-site name: BODY CENTER
*******************************************************************************
Start time      : A.D. 2025-Oct-10 09:54:27.2099 TDB
Stop  time      : A.D. 2025-Oct-11 09:54:27.2099 TDB
Step-size       : 60 minutes
*******************************************************************************
Center geodetic : 0.0, 0.0, 0.0                   {E-lon(deg),Lat(deg),Alt(km)}
Center cylindric: 0.0, 0.0, 0.0                   {E-lon(deg),Dxy(km),Dz(km)}
Center radii    : (undefined)
Small perturbers: Yes                             {source: SB441-N16}
Output units    : KM-S
Calendar mode   : Mixed Julian/Gregorian
Output type     : GEOMETRIC cartesian states
Output format   : 3 (position, velocity, LT, range, range-rate)
Reference frame : Ecliptic of J2000.0
*******************************************************************************
Initial IAU76/J2000 heliocentric ecliptic osculating elements (au, days, deg.):
  EPOCH=  2458849.5 ! 2020-Jan-01.00 (TDB)         Residual RMS= .24563
   EC= .07687465013145245  QR= 2.556401146697176   TP= 2458240.1791309435
   OM= 80.3011901917491    W=  73.80896808746482   IN= 10.59127767086216
  Equivalent ICRF heliocentric cartesian coordinates (au, au/d):
   X= 1.007608869613381E+00  Y=-2.390064275223502E+00  Z=-1.332124522752402E+00
  VX= 9.201724467227128E-03 VY= 3.370381135398406E-03 VZ=-2.850337057661093E-04
Asteroid physical parameters (km, seconds, rotational period in hours):
   GM= 62.6284             RAD= 469.7              ROTPER= 9.07417
   H= 3.34                 G= .120                 B-V= .713
                           ALBEDO= .090            STYP= C
*******************************************************************************
            JDTDB,            Calendar Date (TDB),                      X,                      Y,                      Z,                     VX,                     VY,                     VZ,                     LT,                     RG,                     RR,
**************************************************************************************************************************************************************************************************************************************************************************
$$SOE
2460958.912814930, A.D. 2025-Oct-10 09:54:27.2099,  4.243328170158925E+08,  7.987161493275928E+07, -7.569814997872370E+07, -3.766695133718917E+00,  1.637004743354000E+01,  1.214258886017703E+00,  1.462244039470832E+03,  4.383697347888096E+08, -8.731154383626661E-01,
2460958.954481596, A.D. 2025-Oct-10 10:54:27.2099,  4.243192525958322E+08,  7.993054628410977E+07, -7.569377787727197E+07, -3.769093784371077E+00,  1.636959214868754E+01,  1.214686362345918E+00,  1.462233553898434E+03,  4.383665912932869E+08, -8.732709607906843E-01,
2460958.996148263, A.D. 2025-Oct-10 11:54:27.2099,  4.243056795406764E+08,  7.998947599581927E+07, -7.568940423693359E+07, -3.771492408851478E+00,  1.636913652152356E+01,  1.215113823006861E+00,  1.462223066458559E+03,  4.383634472379088E+08, -8.734264690987970E-01,
$$EOE
**************************************************************************************************************************************************************************************************************************************************************************

TIME

  Barycentric Dynamical Time ("TDB" or T_eph) output was requested. This
continuous coordinate time is equivalent to the relativistic proper time
of a clock at rest in a reference frame co-moving with the solar system
barycenter but outside the system's gravity well. It is the independent
variable in the solar system relativistic equations of motion.

  TDB runs at a uniform rate of one SI second per second and is independent
of irregularities in Earth's rotation.

CALENDAR SYSTEM

  Mixed calendar mode was active such that calendar dates after AD 1582-Oct-15
(if any) are in the modern Gregorian system. Dates prior to 1582-Oct-5 (if any)
are in the Julian calendar system, which is automatically extended for dates
prior to its adoption on 45-Jan-1 BC.  The Julian calendar is useful for
matching historical dates. The Gregorian calendar more accurately corresponds
to the Earth's orbital motion and seasons. A "Gregorian-only" calendar mode is
available if such physical events are the primary interest.

REFERENCE FRAME AND COORDINATES

  Ecliptic at the standard reference epoch

    Reference epoch: J2000.0
    X-Y plane: adopted Earth orbital plane at the reference epoch
               Note: IAU76 obliquity of 84381.448 arcseconds wrt ICRF X-Y plane
    X-axis   : ICRF
    Z-axis   : perpendicular to the X-Y plane in the directional (+ or -) sense
               of Earth's north pole at the reference epoch.

  Symbol meaning:

    JDTDB    Julian Day Number, Barycentric Dynamical Time
      X      X-component of position vector (km)
      Y      Y-component of position vector (km)
      Z      Z-component of position vector (km)
      VX     X-component of velocity vector (km/sec)
      VY     Y-component of velocity vector (km/sec)
      VZ     Z-component of velocity vector (km/sec)
      LT     One-way down-leg Newtonian light-time (sec)
      RG     Range; distance from coordinate center (km)
      RR     Range-rate; radial velocity wrt coord. center (km/sec)

ABERRATIONS AND CORRECTIONS

 Geometric state vectors have NO corrections or aberrations applied.

Computations by ...

    Solar System Dynamics Group, Horizons On-Line Ephemeris System
    4800 Oak Grove Drive, Jet Propulsion Laboratory
    Pasadena, CA  91109   USA

    General site: https://ssd.jpl.nasa.gov/
    Mailing list: https://ssd.jpl.nasa.gov/email_list.html
    System news : https://ssd.jpl.nasa.gov/horizons/news.html
    User Guide  : https://ssd.jpl.nasa.gov/horizons/manual.html
    Connect     : browser        https://ssd.jpl.nasa.gov/horizons/app.html#/x
                  API            https://ssd-api.jpl.nasa.gov/doc/horizons.html
                  command-line   telnet ssd.jpl.nasa.gov 6775
                  e-mail/batch   https://ssd.jpl.nasa.gov/ftp/ssd/horizons_batch.txt
                  scripts        https://ssd.jpl.nasa.gov/ftp/ssd/SCRIPTS
    Author      : Jon.D.Giorgini@jpl.nasa.gov
*******************************************************************************
|}
;;

let%expect_test "parse metadata" =
  let result = Parse.parse_horizons_data input in
  print_s [%sexp (result.metadata : Parse.metadata)];
  [%expect
    {|
    ((target_body "1 Ceres (A801 AA)               {source: JPL#48}")
     (center_body "Solar System Barycenter (0)     {source: DE441}")
     (start_time "A.D. 2025-Oct-10 09:54:27.2099 TDB")
     (stop_time "A.D. 2025-Oct-11 09:54:27.2099 TDB") (step_size "60 minutes")
     (output_units KM-S) (reference_frame "Ecliptic of J2000.0")) |}]
;;

let%expect_test "parse osculating elements" =
  let result = Parse.parse_horizons_data input in
  print_s [%sexp (result.osculating_elements : Parse.osculating_elements option)];
  [%expect
    {|
    (((epoch 2458849.5) (eccentricity 0.076874650131452449)
      (perihelion_distance 2.5564011466971759)
      (time_of_perihelion 2458240.1791309435)
      (longitude_of_ascending_node 80.3011901917491)
      (argument_of_perihelion 73.80896808746482) (inclination 10.59127767086216)))
    |}]
;;

let%expect_test "parse physical parameters" =
  let result = Parse.parse_horizons_data input in
  print_s [%sexp (result.physical_params : Parse.physical_parameters option)];
  [%expect
    {|
    (((gm (62.6284)) (radius (469.7)) (rotation_period (9.07417))
      (absolute_magnitude (3.34)) (slope_parameter (0.12)) (color_index (0.713))
      (albedo (0.09)) (spectral_type (C))))
    |}]
;;

let%expect_test "parse state vectors count" =
  let result = Parse.parse_horizons_data input in
  print_s [%sexp (List.length result.state_vectors : int)];
  [%expect {| 3 |}]
;;

let%expect_test "parse first state vector" =
  let result = Parse.parse_horizons_data input in
  let first = List.hd_exn result.state_vectors in
  print_s [%sexp (first : Parse.state_vector)];
  [%expect
    {|
    ((jdtdb 2460958.91281493) (calendar_date "A.D. 2025-Oct-10 09:54:27.2099")
     (position
      ((x 424332817.01589251) (y 79871614.932759285) (z -75698149.9787237)))
     (velocity
      ((x -3.7666951337189172) (y 16.37004743354) (z 1.214258886017703)))
     (light_time 1462.2440394708319) (range 438369734.7888096)
     (range_rate -0.87311543836266614))
    |}]
;;

let%expect_test "parse all state vectors" =
  let result = Parse.parse_horizons_data input in
  List.iter result.state_vectors ~f:(fun sv ->
    print_s [%sexp ((sv.position, sv.calendar_date) : Parse.km Parse.vector3 * string)]);
  [%expect
    {|
    (((x 424332817.01589251) (y 79871614.932759285) (z -75698149.9787237))
     "A.D. 2025-Oct-10 09:54:27.2099")
    (((x 424319252.59583223) (y 79930546.284109771) (z -75693777.877271965))
     "A.D. 2025-Oct-10 10:54:27.2099")
    (((x 424305679.54067641) (y 79989475.995819271) (z -75689404.236933589))
     "A.D. 2025-Oct-10 11:54:27.2099")
    |}]
;;

let%expect_test "full parse result" =
  let result = Parse.parse_horizons_data input in
  (* Only print summary to avoid huge output *)
  printf "Metadata target: %s\n" result.metadata.target_body;
  printf "Osculating elements present: %b\n" (Option.is_some result.osculating_elements);
  printf "Physical params present: %b\n" (Option.is_some result.physical_params);
  printf "State vectors count: %d\n" (List.length result.state_vectors);
  [%expect
    {|
    Metadata target: 1 Ceres (A801 AA)               {source: JPL#48}
    Osculating elements present: true
    Physical params present: true
    State vectors count: 3 |}]
;;
