# test bulk density functions
require(testthat)

# test for all functions of bulk density
# if not passing test it will indicate what collumn (function) name is not right

# define inputs
A_SOM_LOI = c(2, 5)
A_C_OF = c(20, 100)
A_CLAY_MI = c(10, 20)
A_SILT_MI = c(23, 10)
A_SAND_MI = c(15, 25)
A_DEPTH = c(0.3, 1)
A_H2O_T105 = c(20, 30)
B_ALTITUDE = c(10, 20)
B_SLOPE_DEGREE = c(3, 8)
B_SLOPE_ASPECT = c(3, 8)
A_PH_WA = c(6, 7)
A_CACO3_IF = c(0, 3)
A_N_RT = c(1200, 2000)
A_SAND_M50 = c(140, 250)



# run all bulk density functions

expect_equal(
  sptf_bd1(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
  expected = c(1343.750, 1141.657),
  tolerance = 0.01
)

expect_equal(
  sptf_bd2(A_SOM_LOI = A_SOM_LOI),
  expected = c(1200.1278, 977.7419),
  tolerance = 0.01
)

expect_equal(
  sptf_bd3(A_SOM_LOI = A_SOM_LOI),
  expected = c(931.4997, 844.7057),
  tolerance = 0.01
)

expect_equal(
  sptf_bd4(A_C_OF = A_C_OF),
  expected = c(1224.4222, 686.0185),
  tolerance = 0.01
)

expect_equal(
  sptf_bd5(A_C_OF = A_C_OF),
  expected = c(1304.2212, 790.2904),
  tolerance = 0.01
)

expect_equal(
  sptf_bd6(A_SOM_LOI = A_SOM_LOI),
  expected = c(1199.2896, 912.3666),
  tolerance = 0.01
)

expect_equal(
  sptf_bd7(A_C_OF = A_C_OF),
  expected = c(1118.0887, 537.0976),
  tolerance = 0.01
)

expect_equal(
  sptf_bd8(A_SOM_LOI = A_SOM_LOI),
  expected = c(1224.420, 1055.123),
  tolerance = 0.01
)

expect_equal(
  sptf_bd9(A_C_OF = A_C_OF),
  expected = c(1210.2801, 654.3957),
  tolerance = 0.01
)

expect_equal(sptf_bd10(A_C_OF = A_C_OF),
             expected = c(1284, 380),
             tolerance = 0.01)

expect_equal(
  sptf_bd11(A_SOM_LOI = A_SOM_LOI),
  expected = c(1168.1666, 904.4675),
  tolerance = 0.01
)

expect_equal(
  sptf_bd12(A_SOM_LOI = A_SOM_LOI),
  expected = c(1240.014, 1051.152),
  tolerance = 0.01
)

expect_equal(
  sptf_bd13(A_SOM_LOI = A_SOM_LOI),
  expected = c(1356.274, 1136.181),
  tolerance = 0.01
)

expect_equal(
  sptf_bd14(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1292, 898),
  tolerance = 0.01
)

expect_equal(
  sptf_bd15(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
  expected = c(1267, 884),
  tolerance = 0.01
)

expect_equal(
  sptf_bd16(A_SOM_LOI = A_SOM_LOI),
  expected = c(1471.609, 1275.207),
  tolerance = 0.01
)

expect_equal(
  sptf_bd17(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1433.19, 600.38),
  tolerance = 0.01
)

expect_equal(
  sptf_bd18(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1353.606, 1324.428),
  tolerance = 0.01
)

expect_equal(
  sptf_bd19(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1440.816, 1393.300),
  tolerance = 0.01
)

expect_equal(
  sptf_bd20(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1484.82, 1485.67),
  tolerance = 0.01
)

expect_equal(
  sptf_bd21(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(994.2566, 244.0945),
  tolerance = 0.01
)

expect_equal(
  sptf_bd22(A_SOM_LOI = A_SOM_LOI),
  expected = c(1153.8462, 913.0435),
  tolerance = 0.01
)

expect_equal(
  sptf_bd23(A_SOM_LOI = A_SOM_LOI),
  expected = c(5131.114, 1866.167),
  tolerance = 0.01
)

expect_equal(
  sptf_bd24(A_SOM_LOI = A_SOM_LOI),
  expected = c(1326.984, 1083.365),
  tolerance = 0.01
)

expect_equal(
  sptf_bd25(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SAND_MI,
    A_H2O_T105 = A_H2O_T105,
    A_DEPTH = A_DEPTH
  ),
  expected = c(1211.686, 814.369),
  tolerance = 0.01
)

expect_equal(
  sptf_bd26(A_SOM_LOI = A_SOM_LOI),
  expected = c(1336.6721, 985.6405),
  tolerance = 0.01
)

expect_equal(
  sptf_bd27(A_SOM_LOI = A_SOM_LOI),
  expected = c(1360.928, 1012.059),
  tolerance = 0.01
)

expect_equal(
  sptf_bd28(A_SAND_MI = A_SAND_MI, A_DEPTH = A_DEPTH),
  expected = c(1559.053, 1605.785),
  tolerance = 0.01
)

expect_equal(
  sptf_bd29(
    A_C_OF = A_C_OF,
    A_SAND_MI = A_SAND_MI,
    A_DEPTH = A_DEPTH
  ),
  expected = c(1230.266, 1183.802),
  tolerance = 0.01
)

expect_equal(
  sptf_bd30(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
  expected = c(1545.8, 1468.8), 
  tolerance = 0.01
)

expect_equal(
  sptf_bd31(A_CLAY_MI = A_CLAY_MI),
  expected = c(1517.4, 1512.4),
  tolerance = 0.01
)

expect_equal(
  sptf_bd32(A_C_OF = A_C_OF),
  expected = c(1816.414, 1731.285),
  tolerance = 0.01
)

expect_equal(
  sptf_bd33(
    A_SOM_LOI = A_SOM_LOI,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1502.9, 1580.5),
  tolerance = 0.01
)

expect_equal(
  sptf_bd34(A_SOM_LOI = A_SOM_LOI),
  expected = c(1561.352, 1479.767),
  tolerance = 0.01
)

expect_equal(
  sptf_bd35(A_SOM_LOI = A_SOM_LOI),
  expected = c(1312.401, 1084.665),
  tolerance = 0.01
)

expect_equal(
  sptf_bd36(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI
  ),
  expected = c(1179.3344, 784.3207),
  tolerance = 0.01
)

expect_equal(
  sptf_bd37(A_C_OF = A_C_OF),
  expected = c(1259.5305, 723.7003),
  tolerance = 0.01
)

expect_equal(
  sptf_bd38(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI
  ),
  expected = c(1289.5093, 872.7573),
  tolerance = 0.01
)

expect_equal(
  sptf_bd39(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_H2O_T105 = A_H2O_T105,
    A_DEPTH = A_DEPTH
  ),
  expected = c(1531.5, NA),
  tolerance = 0.01
)

expect_equal(
  sptf_bd40(
    A_SOM_LOI = A_SOM_LOI,
    A_SAND_MI = A_SAND_MI,
    A_DEPTH = A_DEPTH
  ),
  expected = c(1057.8015, 988.5369),
  tolerance = 0.01
)

expect_equal(
  sptf_bd41(A_SOM_LOI = A_SOM_LOI),
  expected = c(911.8690, 768.9277),
  tolerance = 0.01
)

expect_equal(
  sptf_bd42(A_C_OF = A_C_OF),
  expected = c(1604.932, 1491.000),
  tolerance = 0.01
)

expect_equal(
  sptf_bd43(A_C_OF = A_C_OF),
  expected = c(1522.884, 1397.338),
  tolerance = 0.01
)

expect_equal(
  sptf_bd44(A_C_OF = A_C_OF),
  expected = c(1131.9683, 410.9968),
  tolerance = 0.01
)

expect_equal(
  sptf_bd45(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI),
  expected = c(1242, 218),
  tolerance = 0.01
)

expect_equal(
  sptf_bd46(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI),
  expected = c(1301.34, 1273.00),
  tolerance = 0.01
)

expect_equal(
  sptf_bd47(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1410.925, 373.875),
  tolerance = 0.01
)

expect_equal(
  sptf_bd48(A_C_OF = A_C_OF),
  expected = c(871.0801, 452.0796),
  tolerance = 0.01
)

expect_equal(
  sptf_bd49(A_SOM_LOI = A_SOM_LOI),
  expected = c(1006.49, 846.17),
  tolerance = 0.01
)

expect_equal(
  sptf_bd50(A_SOM_LOI = A_SOM_LOI),
  expected = c(1429.9, 912.4),
  tolerance = 0.01
)

expect_equal(
  sptf_bd51(A_SOM_LOI = A_SOM_LOI),
  expected = c(1528.798, 1365.745),
  tolerance = 0.01
)

expect_equal(
  sptf_bd52(A_SOM_LOI = A_SOM_LOI),
  expected = c(1277.721, 1007.679),
  tolerance = 0.01
)

expect_equal(
  sptf_bd53(A_SOM_LOI = A_SOM_LOI),
  expected = c(1338.85, 1049.15),
  tolerance = 0.01
)

expect_equal(
  sptf_bd54(A_SOM_LOI = A_SOM_LOI),
  expected = c(1497.508, 1191.492),
  tolerance = 0.01
)

expect_equal(
  sptf_bd55(A_SOM_LOI = A_SOM_LOI),
  expected = c(1741.009, 1280.991),
  tolerance = 0.01
)

expect_equal(
  sptf_bd56(A_SOM_LOI = A_SOM_LOI),
  expected = c(1224.627, 1003.373),
  tolerance = 0.01
)

expect_equal(
  sptf_bd57(A_SOM_LOI = A_SOM_LOI),
  expected = c(1567.605, 1260.395),
  tolerance = 0.01
)

expect_equal(
  sptf_bd58(A_SOM_LOI = A_SOM_LOI),
  expected = c(2120.190, 1717.552),
  tolerance = 0.01
)

expect_equal(
  sptf_bd59(A_SOM_LOI = A_SOM_LOI),
  expected = c(1251.2752, 952.4238),
  tolerance = 0.01
)

expect_equal(
  sptf_bd60(A_SOM_LOI = A_SOM_LOI),
  expected = c(3875.984, 3366.251),
  tolerance = 0.01
)

expect_equal(
  sptf_bd61(A_SOM_LOI = A_SOM_LOI),
  expected = c(3918.984, 3409.251),
  tolerance = 0.01
)

expect_equal(
  sptf_bd62(A_SOM_LOI = A_SOM_LOI),
  expected = c(1481.043, 1187.648),
  tolerance = 0.01
)

expect_equal(
  sptf_bd63(A_SOM_LOI = A_SOM_LOI),
  expected = c(1525.821, 1162.053),
  tolerance = 0.01
)

expect_equal(
  sptf_bd64(A_SOM_LOI = A_SOM_LOI),
  expected = c(1286.997, 1138.075),
  tolerance = 0.01
)

expect_equal(
  sptf_bd65(A_C_OF = A_C_OF),
  expected = c(1263.944, 954.300),
  tolerance = 0.01
)

expect_equal(
  sptf_bd66(A_C_OF = A_C_OF),
  expected = c(1109.3845, 666.7891),
  tolerance = 0.01
)

expect_equal(
  sptf_bd67(A_C_OF = A_C_OF),
  expected = c(1250.9550, 852.0647),
  tolerance = 0.01
)

expect_equal(
  sptf_bd68(A_C_OF = A_C_OF),
  expected = c(1237.2706, 856.3363),
  tolerance = 0.01
)

expect_equal(
  sptf_bd69(A_CLAY_MI = A_CLAY_MI, A_DEPTH = A_DEPTH),
  expected = c(1482.233, 1666.690),
  tolerance = 0.01
)

expect_equal(
  sptf_bd70(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI,
    A_DEPTH = A_DEPTH,
    B_ALTITUDE = B_ALTITUDE,
    B_SLOPE_DEGREE = B_SLOPE_DEGREE,
    B_SLOPE_ASPECT = B_SLOPE_ASPECT
  ),
  expected = c(1500.783, 780.568),
  tolerance = 0.01
)

expect_equal(
  sptf_bd71(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI,
    B_SLOPE_DEGREE = B_SLOPE_DEGREE
  ),
  expected = c(1495.972, 2624.392),
  tolerance = 0.01
)

expect_equal(sptf_bd72(A_C_OF = A_C_OF),
             expected = c(1362, NA),
             tolerance = 0.01)

expect_equal(sptf_bd73(A_C_OF = A_C_OF),
             expected = c(1380 , NA),
             tolerance = 0.01)

expect_equal(sptf_bd74(A_C_OF = A_C_OF),
             expected = c(1470, NA),
             tolerance = 0.01)

expect_equal(sptf_bd75(A_C_OF = A_C_OF),
             expected = c(1408, NA),
             tolerance = 0.01)

expect_equal(sptf_bd76(A_C_OF = A_C_OF),
             expected = c(1023.584     , NA),
             tolerance = 0.01)

expect_equal(sptf_bd77(A_C_OF = A_C_OF),
             expected = c(1344 , NA),
             tolerance = 0.01)

expect_equal(
  sptf_bd78(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_PH_WA = A_PH_WA
  ),
  expected = c(1506, 523),
  tolerance = 0.01
)

expect_equal(
  sptf_bd79(
    A_C_OF = A_C_OF,
    A_SILT_MI = A_SILT_MI,
    A_SAND_MI = A_SAND_MI,
    A_PH_WA = A_PH_WA
  ),
  expected = c(1306, 695),
  tolerance = 0.01
)

expect_equal(
  sptf_bd80(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_CACO3_IF = A_CACO3_IF
  ),
  expected = c(1771.0, 1155.1),
  tolerance = 0.01
)

expect_equal(
  sptf_bd81(A_C_OF = A_C_OF),
  expected = c(1364.617, 1073.446),
  tolerance = 0.01
)

expect_equal(
  sptf_bd82(A_SAND_MI = A_SAND_MI, A_SILT_MI = A_SILT_MI),
  expected = c(1804.51, 1819.65),
  tolerance = 0.01
)

expect_equal(
  sptf_bd83(A_SAND_MI = A_SAND_MI, A_SILT_MI = A_SILT_MI),
  expected = c(1183.236, 1209.170),
  tolerance = 0.01
)

expect_equal(
  sptf_bd84(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
  expected = c(1360.9234, 882.6717),
  tolerance = 0.01
)

expect_equal(
  sptf_bd85(A_C_OF = A_C_OF),
  expected = c(2149.8738, 753.0637),
  tolerance = 0.01
)

expect_equal(
  sptf_bd86(
    A_C_OF = A_C_OF,
    A_SAND_MI = A_SAND_MI,
    A_CLAY_MI = A_CLAY_MI
  ),
  expected = c(1554.01, 1375.41),
  tolerance = 0.01
)

expect_equal(
  sptf_bd87(
    A_SOM_LOI = A_SOM_LOI,
    A_SAND_MI = A_SAND_MI,
    A_DEPTH = A_DEPTH
  ),
  expected = c(1043.0780, 974.5222),
  tolerance = 0.01
)

# check function later
expect_equal(
  sptf_bd88(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_CACO3_IF = A_CACO3_IF,
    A_PH_WA = A_PH_WA
  ),
  expected = c(-531.5518,-15229.5890),
  tolerance = 0.01
)

expect_equal(
  sptf_bd89(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1589.7, 1525.0),
  tolerance = 0.01
)

expect_equal(
  sptf_bd90(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
  expected = c(1522.302, 1334.935),
  tolerance = 0.01
)

expect_equal(
  sptf_bd91(A_SOM_LOI = A_SOM_LOI),
  expected = c(1430.615, 1238.697),
  tolerance = 0.01
)

expect_equal(
  sptf_bd92(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
  expected = c(1316.191, 1135.827),
  tolerance = 0.01
)

expect_equal(sptf_bd93(A_C_OF = A_C_OF),
             expected = c(1199.4   , NA),
             tolerance = 0.01)

expect_equal(
  sptf_bd94(
    A_SOM_LOI = A_SOM_LOI,
    A_SAND_MI = A_SAND_MI,
    A_CLAY_MI = A_CLAY_MI
  ),
  expected = c(1383.453, 1221.469),
  tolerance = 0.01
)

expect_equal(sptf_bd95(A_C_OF = A_C_OF),
             expected = c(1221.561     , NA),
             tolerance = 0.01)

expect_equal(
  sptf_bd96(A_SOM_LOI = A_SOM_LOI),
  expected = c(1430, 1280),
  tolerance = 0.01
)

expect_equal(
  sptf_bd97(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI),
  expected = c(1544.1232, 897.4294),
  tolerance = 0.01
)

expect_equal(sptf_bd98(A_C_OF = A_C_OF),
             expected = c(1325 , 25),
             tolerance = 0.01)

expect_equal(sptf_bd99(A_C_OF = A_C_OF),
             expected = c(1218 , NA),
             tolerance = 0.01)

expect_equal(
  sptf_bd100(A_N_RT = A_N_RT, A_C_OF = A_C_OF),
  expected = c(1326.4, 1264.0),
  tolerance = 0.01
)

expect_equal(
  sptf_bd101(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI),
  expected = c(1223, NA),
  tolerance = 0.01
)

expect_equal(
  sptf_bd102(A_C_OF = A_C_OF),
  expected = c(1204.912, 1007.112),
  tolerance = 0.01
)

expect_equal(
  sptf_bd103(A_C_OF = A_C_OF),
  expected = c(1268.1988, 911.3736),
  tolerance = 0.01
)

expect_equal(
  sptf_bd104(A_C_OF = A_C_OF),
  expected = c(1325.6899, 858.3987),
  tolerance = 0.01
)

expect_equal(
  sptf_bd105(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH),
  expected = c(1323.096, 1037.500),
  tolerance = 0.01
)

expect_equal(
  sptf_bd106(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH),
  expected = c(1128.803, 865.900),
  tolerance = 0.01
)

expect_equal(
  sptf_bd107(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH),
  expected = c(1342.069, 1177.100),
  tolerance = 0.01
)

expect_equal(
  sptf_bd108(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH),
  expected = c(1277.896, 1026.700),
  tolerance = 0.01
)

expect_equal(
  sptf_bd109(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH),
  expected = c(1269.297, 784.000),
  tolerance = 0.01
)

expect_equal(
  sptf_bd110(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH),
  expected = c(1125.791, 881.000),
  tolerance = 0.01
)

expect_equal(
  sptf_bd111(A_C_OF = A_C_OF),
  expected = c(1469.392, 1318.639),
  tolerance = 0.01
)

expect_equal(
  sptf_bd112(A_C_OF = A_C_OF),
  expected = c(1328.0212, 816.3265),
  tolerance = 0.01
)

expect_equal(
  sptf_bd113(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_PH_WA = A_PH_WA
  ),
  expected = c(1470.4120, 995.1578),
  tolerance = 0.01
)

expect_equal(sptf_bd114(A_C_OF = A_C_OF),
             expected = c(1199.4  , NA),
             tolerance = 0.01)

expect_equal(
  sptf_bd115(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1544.328, 1158.379),
  tolerance = 0.01
)

expect_equal(
  sptf_bd116(A_C_OF = A_C_OF),
  expected = c(1234.0232, 753.3821),
  tolerance = 0.01
)

expect_equal(
  sptf_bd117(A_C_OF = A_C_OF),
  expected = c(2334.857, 1304.898),
  tolerance = 0.01
)

expect_equal(
  sptf_bd118(A_C_OF = A_C_OF),
  expected = c(1217.9953, 720.9536),
  tolerance = 0.01
)

expect_equal(
  sptf_bd119(A_C_OF = A_C_OF),
  expected = c(1433.6, 736.0),
  tolerance = 0.01
)

expect_equal(
  sptf_bd120(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1263, 636),
  tolerance = 0.01
)

expect_equal(
  sptf_bd121(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_CACO3_IF = A_CACO3_IF,
    A_PH_WA = A_PH_WA,
    B_ALTITUDE = B_ALTITUDE,
    B_SLOPE_DEGREE = B_SLOPE_DEGREE
  ),
  expected = c(1117.341, 806.232),
  tolerance = 0.01
)

expect_equal(
  sptf_bd122(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI
  ),
  expected = c(1149.387, 919.961),
  tolerance = 0.01
)

expect_equal(
  sptf_bd123(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI
  ),
  expected = c(1181.6667, 970.1111),
  tolerance = 0.01
)

expect_equal(sptf_bd124(A_C_OF = A_C_OF),
             expected = c(1213, 253),
             tolerance = 0.01)

expect_equal(sptf_bd125(A_C_OF = A_C_OF),
             expected = c(1004, 232),
             tolerance = 0.01)

expect_equal(
  sptf_bd126(
    A_C_OF = A_C_OF,
    A_SILT_MI = A_SILT_MI,
    A_DEPTH = A_DEPTH
  ),
  expected = c(1201.9328, 459.6689),
  tolerance = 0.01
)

expect_equal(
  sptf_bd127(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH),
  expected = c(1379.7962, 701.0351),
  tolerance = 0.01
)

expect_equal(
  sptf_bd128(A_C_OF = A_C_OF),
  expected = c(1070.5931, 511.3874),
  tolerance = 0.01
)

expect_equal(
  sptf_bd129(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI,
    A_DEPTH = A_DEPTH
  ),
  expected = c(1311.6297, 801.4846),
  tolerance = 0.01
)

expect_equal(
  sptf_bd130(A_C_OF = A_C_OF),
  expected = c(1294.7883, 816.1684),
  tolerance = 0.01
)

expect_equal(
  sptf_bd131(A_C_OF = A_C_OF),
  expected = c(1393.8824, 990.7788),
  tolerance = 0.01
)

expect_equal(
  sptf_bd132(A_C_OF = A_C_OF),
  expected = c(1320.3547, 859.7398),
  tolerance = 0.01
)

expect_equal(
  sptf_bd133(
    A_C_OF = A_C_OF,
    A_DEPTH = A_DEPTH,
    B_SLOPE_DEGREE = B_SLOPE_DEGREE
  ),
  expected = c(995.4634, 924.2893),
  tolerance = 0.01
)

expect_equal(
  sptf_bd134(A_SOM_LOI = A_SOM_LOI),
  expected = c(1440.062, 1277.220),
  tolerance = 0.01
)

expect_equal(
  sptf_bd135(A_SOM_LOI = A_SOM_LOI),
  expected = c(1463.419, 1222.350),
  tolerance = 0.01
)

expect_equal(
  sptf_bd136(A_C_OF = A_C_OF),
  expected = c(1149.2427, 348.2262),
  tolerance = 0.01
)

expect_equal(
  sptf_bd137(
    A_SOM_LOI = A_SOM_LOI,
    A_SILT_MI = A_SILT_MI,
    A_PH_WA = A_PH_WA,
    A_DEPTH = A_DEPTH
  ),
  expected = c(1459.733, 1616.333),
  tolerance = 0.01
)

expect_equal(
  sptf_bd138(A_SOM_LOI = A_SOM_LOI, A_SILT_MI = A_SILT_MI),
  expected = c(1441.1, 1167.0),
  tolerance = 0.01
)

expect_equal(
  sptf_bd139(
    A_SOM_LOI = A_SOM_LOI,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SILT_MI
  ),
  expected = c(1333.568, 1142.600),
  tolerance = 0.01
)

expect_equal(
  sptf_bd140(
    A_SOM_LOI = A_SOM_LOI,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(149.83387, 88.06432),
  tolerance = 0.01
)

expect_equal(
  sptf_bd141(
    A_C_OF = A_C_OF,
    A_N_RT = A_N_RT,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI
  ),
  expected = c(1156.1, 412.5),
  tolerance = 0.01
)

expect_equal(
  sptf_bd142(
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(2425.6, 2335.2),
  tolerance = 0.01
)

expect_equal(
  sptf_bd143(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(2018.4, 1283.0),
  tolerance = 0.01
)

expect_equal(
  sptf_bd144(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI
  ),
  expected = c(1630.6, 1023.0),
  tolerance = 0.01
)

expect_equal(
  sptf_bd145(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1482.90, 1295.75),
  tolerance = 0.01
)

expect_equal(
  sptf_bd146(A_C_OF = A_C_OF),
  expected = c(1405.445, 1070.023),
  tolerance = 0.01
)

expect_equal(
  sptf_bd147(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI,
    A_DEPTH = A_DEPTH
  ),
  expected = c(1571, NA),
  tolerance = 0.01
)

expect_equal(
  sptf_bd148(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
  expected = c(1890.655, 1717.884),
  tolerance = 0.01
)

expect_equal(
  sptf_bd149(A_C_OF = A_C_OF),
  expected = c(1325.454, 1200.009),
  tolerance = 0.01
)

expect_equal(
  sptf_bd150(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH),
  expected = c(880.6 ,-615.6),
  tolerance = 0.01
)

expect_equal(
  sptf_bd151(
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1088.52, 1089.60),
  tolerance = 0.01
)

expect_equal(
  sptf_bd152(A_SOM_LOI = A_SOM_LOI),
  expected = c(1464.8, 1202.0),
  tolerance = 0.01
)

expect_equal(
  sptf_bd153(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
  expected = c(1219.121, 1232.327),
  tolerance = 0.01
)

expect_equal(
  sptf_bd154(A_C_OF = A_C_OF, A_PH_WA = A_PH_WA),
  expected = c(1179, 124),
  tolerance = 0.01
)

expect_equal(
  sptf_bd155(A_C_OF = A_C_OF, A_SILT_MI = A_SILT_MI),
  expected = c(885 ,-1461),
  tolerance = 0.01
)

expect_equal(
  sptf_bd156(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
  expected = c(1032.9,-460.5),
  tolerance = 0.01
)

expect_equal(
  sptf_bd157(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(800, 800),
  tolerance = 0.01
)

expect_equal(
  sptf_bd158(
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(800, 800),
  tolerance = 0.01
)

expect_equal(
  sptf_bd159(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
  expected = c(1351.3514, 865.8009),
  tolerance = 0.01
)

expect_equal(
  sptf_bd160(A_SOM_LOI = A_SOM_LOI),
  expected = c(683.49, 630.00),
  tolerance = 0.01
)

expect_equal(
  sptf_bd161(A_SOM_LOI = A_SOM_LOI),
  expected = c(721.38, 630.00),
  tolerance = 0.01
)

expect_equal(
  sptf_bd162(A_C_OF = A_C_OF),
  expected = c(408.8809,-1024.5955),
  tolerance = 0.01
)

expect_equal(
  sptf_bd163(A_C_OF = A_C_OF),
  expected = c(1113.116, 892.856),
  tolerance = 0.01
)

expect_equal(
  sptf_bd164(
    A_SOM_LOI = A_SOM_LOI,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1535.9, 1349.0),
  tolerance = 0.01
)

expect_equal(
  sptf_bd165(A_SOM_LOI = A_SOM_LOI),
  expected = c(1750, 1585),
  tolerance = 0.01
)

expect_equal(
  sptf_bd166(A_C_OF = A_C_OF),
  expected = c(1123.499, 900.000),
  tolerance = 0.01
)

expect_equal(
  sptf_bd167(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
  expected = c(2637.446, 2594.620),
  tolerance = 0.01
)

expect_equal(
  sptf_bd168(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
  expected = c(1479.290, 1265.823),
  tolerance = 0.01
)

expect_equal(
  sptf_bd169(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
  expected = c(1464.129, 1327.140),
  tolerance = 0.01
)

expect_equal(
  sptf_bd170(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
  expected = c(1490.313, 1338.688),
  tolerance = 0.01
)

expect_equal(
  sptf_bd171(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
  expected = c(1608.752, 1478.197),
  tolerance = 0.01
)

expect_equal(
  sptf_bd172(A_SOM_LOI = A_SOM_LOI),
  expected = c(1452.644, 1306.336),
  tolerance = 0.01
)

expect_equal(
  sptf_bd173(A_SOM_LOI = A_SOM_LOI),
  expected = c(1494.768, 1289.491),
  tolerance = 0.01
)

expect_equal(
  sptf_bd174(A_SOM_LOI = A_SOM_LOI),
  expected = c(1283.005, 1052.995),
  tolerance = 0.01
)

expect_equal(
  sptf_bd175(A_SOM_LOI = A_SOM_LOI),
  expected = c(1517.386, 1241.614),
  tolerance = 0.01
)

expect_equal(
  sptf_bd176(A_SOM_LOI = A_SOM_LOI),
  expected = c(1081.2191, 856.7809),
  tolerance = 0.01
)

expect_equal(
  sptf_bd177(
    A_SOM_LOI = A_SOM_LOI,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI,
    A_SAND_M50 = A_SAND_M50
  ),
  expected = c(1487.138, 1298.589),
  tolerance = 0.01
)

expect_equal(
  sptf_bd178(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
  expected = c(1507.443, 1310.764),
  tolerance = 0.01
)

expect_equal(
  sptf_bd179(
    A_SOM_LOI = A_SOM_LOI,
    A_C_OF = A_C_OF,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI,
    A_SAND_MI = A_SAND_MI,
    A_PH_WA = A_PH_WA
  ),
  expected = c(1147.6059, 732.7198),
  tolerance = 0.01
)

expect_equal(
  sptf_bd180(
    A_SOM_LOI = A_SOM_LOI,
    A_SAND_MI = A_SAND_MI,
    A_CLAY_MI = A_CLAY_MI
  ),
  expected = c(1240.886, 1104.773),
  tolerance = 0.01
)

expect_equal(
  sptf_bd181(
    A_SOM_LOI = A_SOM_LOI,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI
  ),
  expected = c(1628, 1402),
  tolerance = 0.01
)
