let test_input =
  {|
API VERSION: 1.2
API SOURCE: NASA/JPL Horizons API

*******************************************************************************
JPL/DASTCOM                Small-body Search Results       2025-Oct-13 02:31:33

 Asteroids only parameter search:
    A < 2.5; IN > 10.; LIST;

 Matching small-bodies:

    Record #  Epoch-yr  Primary Desig  Name                      A               IN
    --------  --------  -------------  ------------------------- --------------- -----------
           6    2017    A847 NA         Hebe                     2.4249360031239  14.7374212
          18    2017    A852 MA         Melpomene                2.2951087650735  10.1334893
          25    2017    A853 GB         Phocaea                  2.4001207550261  21.6061555
         105    2017    A868 SA         Artemis                  2.3740447649941  21.4443040
         115    2017    A871 PA         Thyra                    2.3805769438948  11.5904491
         172    2017    A877 CA         Baucis                   2.3806837835744  10.0204402
         186    2016    A878 GA         Celuta                   2.3626702715145  13.1838869
    50621812    2025    2025 TT        [...unnamed...]           1.1886823301298  25.8440958
    50621813    2025    2025 TU        [...unnamed...]           1.7507017179520  12.5740751
    50621815    2025    2025 TW        [...unnamed...]           1.2601546871639  23.3870259
    50621816    2025    2025 TX        [...unnamed...]           1.2505222543387  25.7112258
    50621819    2025    2025 TA1       [...unnamed...]           1.3015174040560  25.0196830
    50621822    2025    2025 TD1       [...unnamed...]           1.8625495000000  18.2617200
    50621824    2025    2025 TF1       [...unnamed...]           1.0209230977147  18.5015000
    50621825    2025    2025 TG1       [...unnamed...]           1.8760716000000  19.3550000
    50621827    2025    2025 TJ1       [...unnamed...]           1.4794949989152  12.0757490
    50621828    2025    2025 TK1       [...unnamed...]           1.3460576445713  14.3726520
    50621830    2025    2025 TM1       [...unnamed...]           1.3942472767200  17.5456117
    50621833    2025    2025 TP1       [...unnamed...]           1.2744325553332  16.5513520
    50621836    2025    2025 TS1       [...unnamed...]           1.4123572467113  15.0300474
    50621837    2025    2025 TT1       [...unnamed...]           1.1190274096760  24.9284638
    50621840    2025    2025 TW1       [...unnamed...]           1.2612413536246  15.4742938
    50621841    2025    2025 TX1       [...unnamed...]           1.4589550550092  26.6074152
    50621842    2025    2025 TY1       [...unnamed...]           .97813037001140  35.1356680
    50621851    2025    2025 TM2       [...unnamed...]           1.3596122218577  25.5090701
    50621852    2025    2025 TN2       [...unnamed...]           1.3588605383583  21.5705295
    50621856    2025    2025 TR2       [...unnamed...]           2.4940020380801  16.4118286
    50621863    2025    2025 TY2       [...unnamed...]           1.3466286194128  20.9506173
    50621867    2025    2025 TC3       [...unnamed...]           1.8699058000000  20.5525300
    50621892    2025    2025 TC4       [...unnamed...]           1.8577766000000  19.9697700
    50621913    2025    2025 TA5       [...unnamed...]           1.4094279380661  13.5978550
    50621917    2018    2015 BA620     [...unnamed...]           2.3588223445106  11.4510353

 (85933 matches. To SELECT, enter record # (integer), followed by semi-colon.)
*******************************************************************************
|}
;;

open Core

let%expect_test "parse small-body query results" =
  let bodies = Parse_sb_query.parse test_input in
  List.iter bodies ~f:(fun body ->
    Printf.printf "%s | %s\n" body.Parse_sb_query.designation body.Parse_sb_query.name);
  [%expect
    {|
    A847 NA | Hebe
    A852 MA | Melpomene
    A853 GB | Phocaea
    A868 SA | Artemis
    A871 PA | Thyra
    A877 CA | Baucis
    A878 GA | Celuta
    2025 TT | [...unnamed...]
    2025 TU | [...unnamed...]
    2025 TW | [...unnamed...]
    2025 TX | [...unnamed...]
    2025 TA1 | [...unnamed...]
    2025 TD1 | [...unnamed...]
    2025 TF1 | [...unnamed...]
    2025 TG1 | [...unnamed...]
    2025 TJ1 | [...unnamed...]
    2025 TK1 | [...unnamed...]
    2025 TM1 | [...unnamed...]
    2025 TP1 | [...unnamed...]
    2025 TS1 | [...unnamed...]
    2025 TT1 | [...unnamed...]
    2025 TW1 | [...unnamed...]
    2025 TX1 | [...unnamed...]
    2025 TY1 | [...unnamed...]
    2025 TM2 | [...unnamed...]
    2025 TN2 | [...unnamed...]
    2025 TR2 | [...unnamed...]
    2025 TY2 | [...unnamed...]
    2025 TC3 | [...unnamed...]
    2025 TC4 | [...unnamed...]
    2025 TA5 | [...unnamed...]
    2015 BA620 | [...unnamed...] |}]
;;
