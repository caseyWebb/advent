module Day5 exposing (..)

import Html exposing (b)
import List.Extra as List
import Parser as P exposing ((|.), (|=), Parser)


type alias Range =
    ( Int, Int )


inRange : Int -> Range -> Bool
inRange value ( start, end ) =
    value >= start && value <= end


type alias Mapping =
    ( Int, Int, Int )


solve : String -> Result (List P.DeadEnd) (Maybe Int)
solve =
    P.run inputParser
        >> Result.map
            (\( seeds, mappings ) ->
                List.foldl applyMappings seeds mappings
                    |> List.map Tuple.first
                    |> List.minimum
            )


inputParser : Parser ( List Range, List (List Mapping) )
inputParser =
    P.succeed Tuple.pair
        |= seedsParser
        |= mappingsParser


seedsParser : Parser (List Range)
seedsParser =
    P.succeed identity
        |. P.spaces
        |. P.symbol "seeds:"
        |= P.loop []
            (\seeds ->
                P.succeed
                    (Maybe.map (\seed -> P.Loop (seed :: seeds)) >> Maybe.withDefault (P.Done (List.reverse seeds)))
                    |. P.spaces
                    |= P.oneOf
                        [ P.succeed (\start width -> Just ( start, start + width - 1 ))
                            |= P.int
                            |. P.spaces
                            |= P.int
                        , P.succeed Nothing
                        ]
            )


mappingsParser : Parser (List (List Mapping))
mappingsParser =
    P.loop []
        (\mappings ->
            P.oneOf
                [ mappingParser |> P.map (\mapping -> P.Loop (mapping :: mappings))
                , P.end |> P.map (always (P.Done (List.reverse mappings)))
                ]
        )


mappingParser : Parser (List Mapping)
mappingParser =
    P.succeed identity
        |. P.chompUntil "map:"
        |. P.symbol "map:"
        |= P.loop []
            (\mappings ->
                P.oneOf
                    [ P.succeed (\a b c -> P.Loop (( a, b, c ) :: mappings))
                        |. P.spaces
                        |= P.int
                        |. P.spaces
                        |= P.int
                        |. P.spaces
                        |= P.int
                        |. P.spaces
                    , P.succeed (P.Done (List.reverse mappings))
                    ]
            )


type Mapped a
    = Mapped a
    | Unmapped a


partition : List (Mapped a) -> ( List a, List a )
partition =
    List.foldl
        (\mapped ( mappedAcc, unmappedAcc ) ->
            case mapped of
                Mapped a ->
                    ( a :: mappedAcc, unmappedAcc )

                Unmapped a ->
                    ( mappedAcc, a :: unmappedAcc )
        )
        ( [], [] )


unwrap : List (Mapped a) -> List a
unwrap =
    List.map
        (\mapped ->
            case mapped of
                Mapped a ->
                    a

                Unmapped a ->
                    a
        )


applyMappings : List Mapping -> List Range -> List Range
applyMappings mappings =
    List.concatMap
        (\range ->
            List.foldl
                (\mapping acc ->
                    let
                        ( mapped, unmapped ) =
                            partition acc
                    in
                    List.concatMap (mapRange mapping) unmapped ++ (mapped |> List.map Mapped)
                )
                [ Unmapped range ]
                mappings
                |> unwrap
        )
        >> collapseRanges


collapseRanges : List Range -> List Range
collapseRanges =
    List.sortBy Tuple.first
        >> List.foldl
            (\(( start, end ) as range) acc ->
                case acc of
                    [] ->
                        [ range ]

                    ( prevStart, prevEnd ) :: rest ->
                        if prevEnd + 1 >= start then
                            ( prevStart, end ) :: rest

                        else
                            range :: acc
            )
            []


mapRange : Mapping -> Range -> List (Mapped Range)
mapRange ( destStart, sourceStart, mappingWidth ) ( start, end ) =
    let
        sourceEnd =
            sourceStart + mappingWidth - 1

        destEnd =
            destStart + mappingWidth - 1

        sourceRange =
            ( sourceStart, sourceEnd )

        destRange =
            ( destStart, destEnd )

        leftOuterBound =
            sourceStart - 1

        rightOuterBound =
            sourceEnd + 1

        offset =
            start - sourceStart

        width =
            end - start
    in
    if inRange start sourceRange || inRange end sourceRange then
        case ( start < sourceStart, end > sourceEnd ) of
            ( True, True ) ->
                -- |-----------------|  <- value
                --     |---------|      <- mapping
                [ Unmapped ( start, sourceStart - 1 )
                , Mapped destRange
                , Unmapped ( rightOuterBound, end )
                ]

            ( True, False ) ->
                -- |-----------|   <- value
                --     |---------| <- mapping
                [ Unmapped ( start, leftOuterBound )
                , Mapped ( destStart, destStart + width - (leftOuterBound - start) )
                ]

            ( False, True ) ->
                --     |-----------| <- value
                -- |---------|       <- mapping
                [ Mapped ( destStart + offset, destEnd )
                , Unmapped ( rightOuterBound, end )
                ]

            ( False, False ) ->
                --     |---------|      <- value
                -- |-----------------|  <- mapping
                [ Mapped ( destStart + offset, destStart + offset + width )
                ]

    else
        [ Unmapped ( start, end ) ]


sampleInput : String
sampleInput =
    String.trimLeft """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""


input : String
input =
    String.trimLeft """
seeds: 763445965 78570222 1693788857 146680070 1157620425 535920936 3187993807 180072493 1047354752 20193861 2130924847 274042257 20816377 596708258 950268560 11451287 3503767450 182465951 3760349291 265669041

seed-to-soil map:
0 1894195346 315486903
1184603419 2977305241 40929361
1225532780 597717 4698739
1988113706 1603988885 78481073
679195301 529385087 505408118
1781158512 2285166785 39457705
352613463 2324624490 326581838
1820616217 1738931330 104130014
2066594779 2671974456 78036460
1288754536 1682469958 56461372
1371340411 3442489267 409818101
3341036988 1092718505 511270380
315486903 1857068786 37126560
1924746231 2209682249 49360033
1345215908 2259042282 26124503
2917167497 2651206328 20768128
1230231519 1034793205 57925300
2144631239 3421335965 21153302
2689873172 2750010916 227294325
1974106264 1843061344 14007442
2165784541 5296456 524088631
1288156819 0 597717
2937935625 3018234602 403101363

soil-to-fertilizer map:
3849355959 3101885274 170573538
3006688391 3025445580 66663107
1656285214 0 92188755
2361256790 4033128077 180329908
2073349245 3676064680 287907545
3458323699 4213457985 81509311
2607585697 2380345050 99923790
2541586698 3332501375 61727497
3789313396 3272458812 60042563
1858319884 917612384 162600389
404885474 510011966 407600418
4019929497 3603127472 72937208
812485892 1190058688 253881561
2603314195 2073349245 4271502
1469824760 92188755 186460454
0 278649209 231362757
1249323571 1443940249 220501189
4285190709 3092108687 9776587
1066367453 1817839802 182956118
3257369783 2724171240 184379320
2707509487 2480268840 111317034
2818826521 2192483180 187861870
1748473969 1080212773 109845915
3441749103 3394228872 16574596
3142507350 2077620747 114862433
3539833010 2908550560 116895020
231362757 1664441438 153398364
384761121 2000795920 20124353
3656728030 2591585874 132585366
3073351498 3963972225 69155852
4092866705 3410803468 192324004

fertilizer-to-water map:
2289944373 962149121 35670423
3187295333 3295892606 34862460
3798694276 3187295333 48036047
3633796172 3644673845 164898104
3222157793 3909959418 311250910
922447685 2248438960 142418952
648510907 2390857912 2187794
3533408703 3809571949 100387469
650698701 1840321280 271748984
464371310 553340958 30176545
3907291549 3330755066 313918779
2791296727 461310335 25110510
397451197 486420845 66920113
3846730323 3235331380 60561226
2575399801 2393045706 96151163
1064866637 583517503 106955439
1308190772 822897256 139251865
0 236670371 224639964
2671550964 0 119745763
1171822076 2112070264 136368696
224639964 690472942 132424314
494547855 2489196869 153963052
2498862112 160132682 76537689
1447442637 997819544 842501736
357064278 119745763 40386919
2325614796 2643159921 173247316

water-to-light map:
3181220930 3836164803 159006567
1405924027 2473049234 39930303
1275636734 1411161599 10830099
3831476263 1580605379 266491658
1570444205 1294782382 116379217
3133505036 2271845049 47715894
1305612481 2512979537 100311546
1286466833 1275636734 19145648
167024508 145118817 64336111
92584517 615923894 40913901
2227655152 2098041405 130620812
2181794752 2613291083 34326669
433451288 209454928 64553504
4223684323 2228662217 43182832
3527642272 3532330812 303833991
2855274869 2647617752 119788915
4266867155 2319560943 28100141
231360619 1167459710 18705935
2136463524 3995171370 45331228
2978600831 1421991698 154904205
4097967921 2825698355 78160170
0 489813287 92584517
1686823422 3023927616 328136942
2014960364 2903858525 116532044
1512152517 2767406667 58291688
395185371 1129193793 38265917
498004792 274008432 215804855
2975063784 3020390569 3537047
3340227497 3352064558 180266254
3524203227 4040502598 3439045
713809647 656837795 472355998
1445854330 2395217316 66298187
3520493751 1576895903 3709476
4176128091 2347661084 47556232
2216121421 2461515503 11533731
2604330501 1847097037 250944368
2358275964 4043941643 246054537
133498418 582397804 33526090
2131492408 4289996180 4971116
250066554 0 145118817

light-to-temperature map:
1212130960 1395832798 58636484
542019293 812004214 195851408
1706861165 2688952641 3198757
3097592274 3018951608 116723898
1148165381 3135675506 2936711
938881022 300892136 26815610
2884354982 3318488806 103553821
789335095 421331130 149545927
3270968199 1036156523 69989778
1142047574 1030038716 6117807
1710059922 1367448764 28384034
3961474833 1257923390 109525374
300892136 570877057 241127157
2564647329 3422042627 200129932
3534803741 2001828127 16273450
1738443956 1454469282 486329977
3717055162 2018101577 244419671
4071000207 2781125469 223967089
1030038716 3005092558 13859050
737870701 358179343 20992797
3388483069 4209077325 84901810
3551077191 3622172559 62988195
3614065386 2769635501 11489968
3215304333 3262824940 55663866
2278270544 2395185346 286376785
1151102092 1940799259 61028868
3625555354 4044566844 91499808
2987908803 1106146301 109683471
1134657064 2681562131 7390510
2236365744 2353280546 41904800
1270767444 3685160754 311880998
1043897766 2262521248 90759298
2806870879 2692151398 77484103
3340957977 3997041752 47525092
3214316172 4293979135 988161
2764777261 1215829772 42093618
965696632 379172140 42158990
3473384879 4147658463 61418862
2224773933 4136066652 11591811
1582648442 3201806221 61018719
758863498 327707746 30471597
1643667161 3138612217 63194004

temperature-to-humidity map:
3893671309 2897869925 193525827
1232495182 999263760 191357681
2440090529 1477966061 163000786
3554817869 2745268041 3258240
2845194314 3091395752 30017739
4087197136 1397536284 33207741
131252236 827398049 11627380
2183328852 2476037434 27127608
2603091315 2503165042 242102999
2875212053 1778930312 348370034
1427151141 891672290 77843721
1642958327 3928570372 200739395
3610489285 3219006125 283182024
3277129360 3580618829 128344865
2233175686 1190621441 206914843
891672290 4129309767 165657529
0 205218291 35419402
3584280491 3551111757 26208794
4247745260 1430744025 47222036
2210456460 3502188149 21171921
1843697722 2136406304 209010485
1423852863 3577320551 3298278
348097907 336470527 490927522
1057329819 3708963694 175165363
2231628381 3549564452 1547305
3268023402 2127300346 9105958
142879616 0 205218291
3558076109 3523360070 26204382
2052708207 2345416789 130620645
4120404877 969516011 29747749
35419402 240637693 95832834
4150152626 3121413491 97592634
3223582087 3884129057 44441315
1504994862 1640966847 137963465
3405474225 2748526281 149343644

humidity-to-location map:
3554305993 3441523152 94289982
4052786034 3015714705 242181262
491965155 895492593 72049890
2437667299 3726944956 38441480
0 387558480 141314689
3811971022 3765386436 101972362
3648595975 3535813134 51583790
1318420824 1500487572 135197760
230758172 30039204 163596268
3097413876 4221249785 73717511
3041068351 3257895967 56345525
1454254795 641271247 181430537
394354440 1139828795 97610715
3215016925 2618366793 339289068
827848339 193635472 113349295
150184459 306984767 80573713
1778801395 2099048921 519317872
1176891800 1379491771 111489820
1288381620 0 30039204
3171131387 3891749067 43885538
564015045 528873169 112398078
3913943384 3935634605 11560990
2767014161 3947195595 274054190
941197634 822701784 72790809
1453618584 1499851361 636211
2298119267 3587396924 139548032
3700179765 3867358798 24390269
3742730647 1778801395 11181531
1013988443 967542483 20851096
141314689 1490981591 8869770
3925504374 3314241492 127281660
2476108779 2088802749 10246172
676413123 988393579 151435216
3753912178 2957655861 58058844
2486354951 1789982926 280659210
3724570034 2070642136 18160613
1034839539 1237439510 142052261
"""
