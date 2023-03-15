# test bulk density functions
require(testthat)


# test for all functions of bulk density
# if not passing test it will indicate what collumn (function) name is not right

test_that("estimation bulk density", {
  expect_equal(
    data.table(
      bd1= sptf_bd1(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd2= sptf_bd2(A_SOM_LOI = c(2,5)),
      bd3= sptf_bd3(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd4= sptf_bd4(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd5= sptf_bd5(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd6= sptf_bd6(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd7= sptf_bd7(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd8= sptf_bd8(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd9= sptf_bd9(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd10= sptf_bd10(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd11= sptf_bd11(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd12= sptf_bd12(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd13= sptf_bd13(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd14= sptf_bd14(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd15= sptf_bd15(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd16= sptf_bd16(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd17= sptf_bd17(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd18= sptf_bd18(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd19= sptf_bd19(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd20= sptf_bd20(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd21= sptf_bd21(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd22= sptf_bd22(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd23= sptf_bd23(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd24= sptf_bd24(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd25= sptf_bd25(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd26= sptf_bd26(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd27= sptf_bd27(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd28= sptf_bd28(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd29= sptf_bd29(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd30= sptf_bd30(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd31= sptf_bd31(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd32= sptf_bd32(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd33= sptf_bd33(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd34= sptf_bd34(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd35= sptf_bd35(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd36= sptf_bd36(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd37= sptf_bd37(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd38= sptf_bd38(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd39= sptf_bd39(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd40= sptf_bd40(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd41= sptf_bd41(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd42= sptf_bd42(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd43= sptf_bd43(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd44= sptf_bd44(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd45= sptf_bd45(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd46= sptf_bd46(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd47= sptf_bd47(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd48= sptf_bd48(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd49= sptf_bd49(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd50= sptf_bd50(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd51= sptf_bd51(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd52= sptf_bd52(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd53= sptf_bd53(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd54= sptf_bd54(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd55= sptf_bd55(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd56= sptf_bd56(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd57= sptf_bd57(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd58= sptf_bd58(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd59= sptf_bd59(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd60= sptf_bd60(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd61= sptf_bd61(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd62= sptf_bd62(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd63= sptf_bd63(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd64= sptf_bd64(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd65= sptf_bd65(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd66= sptf_bd66(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd67= sptf_bd67(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd68= sptf_bd68(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd69= sptf_bd69(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd70= sptf_bd70(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd71= sptf_bd71(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd72= sptf_bd72(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd73= sptf_bd73(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd74= sptf_bd74(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd75= sptf_bd75(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd76= sptf_bd76(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd77= sptf_bd77(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd78= sptf_bd78(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd79= sptf_bd79(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd80= sptf_bd80(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd81= sptf_bd81(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd82= sptf_bd82(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd83= sptf_bd83(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd84= sptf_bd84(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd85= sptf_bd85(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd86= sptf_bd86(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd87= sptf_bd87(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd88= sptf_bd88(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd89= sptf_bd89(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd90= sptf_bd90(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd91= sptf_bd91(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd92= sptf_bd92(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd93= sptf_bd93(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd94= sptf_bd94(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd95= sptf_bd95(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd96= sptf_bd96(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd97= sptf_bd97(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd98= sptf_bd98(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd99= sptf_bd99(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd100= sptf_bd100(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd101= sptf_bd101(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd102= sptf_bd102(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd103= sptf_bd103(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd104= sptf_bd104(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd105= sptf_bd105(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd106= sptf_bd106(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd107= sptf_bd107(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd108= sptf_bd108(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd109= sptf_bd109(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd110= sptf_bd110(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd111= sptf_bd111(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd112= sptf_bd112(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd113= sptf_bd113(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd114= sptf_bd114(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd115= sptf_bd115(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd116= sptf_bd116(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd117= sptf_bd117(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd118= sptf_bd118(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd119= sptf_bd119(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd120= sptf_bd120(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd121= sptf_bd121(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd122= sptf_bd122(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd123= sptf_bd123(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd124= sptf_bd124(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd125= sptf_bd125(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd126= sptf_bd126(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd127= sptf_bd127(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd128= sptf_bd128(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd129= sptf_bd129(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd130= sptf_bd130(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd131= sptf_bd131(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd132= sptf_bd132(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd133= sptf_bd133(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd134= sptf_bd134(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd135= sptf_bd135(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd136= sptf_bd136(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd137= sptf_bd137(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd138= sptf_bd138(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd139= sptf_bd139(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd140= sptf_bd140(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd141= sptf_bd141(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd142= sptf_bd142(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd143= sptf_bd143(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd144= sptf_bd144(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd145= sptf_bd145(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd146= sptf_bd146(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd147= sptf_bd147(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd148= sptf_bd148(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd149= sptf_bd149(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd150= sptf_bd150(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd151= sptf_bd151(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd152= sptf_bd152(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd153= sptf_bd153(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd154= sptf_bd154(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd155= sptf_bd155(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd156= sptf_bd156(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd157= sptf_bd157(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd158= sptf_bd158(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd159= sptf_bd159(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd160= sptf_bd160(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd161= sptf_bd161(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd162= sptf_bd162(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd163= sptf_bd163(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd164= sptf_bd164(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd165= sptf_bd165(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd166= sptf_bd166(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd167= sptf_bd167(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd168= sptf_bd168(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd169= sptf_bd169(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd170= sptf_bd170(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd171= sptf_bd171(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd172= sptf_bd172(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd173= sptf_bd173(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd174= sptf_bd174(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd175= sptf_bd175(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd176= sptf_bd176(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd177= sptf_bd177(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd178= sptf_bd178(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd179= sptf_bd179(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd180= sptf_bd180(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
      bd181= sptf_bd181(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15))
      
    ),
    expected = data.table(
      bd1 = c(1343.750, 1176.612),
      bd2 = c(1343.750, 1176.612)
      ),
    tolerance = 0.01)
})




data.table(
  bd1= sptf_bd1(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15)),
  bd2= sptf_bd2(A_SOM_LOI = c(2,5)),
  bd3= sptf_bd3(A_SOM_LOI = c(2,5)),
  bd4= sptf_bd4(A_C_OF = c(20,200)),
  bd5= sptf_bd5(A_C_OF = c(20,200)),
  bd6= sptf_bd6(A_SOM_LOI = c(2,5)),
  bd7= sptf_bd7(A_C_OF = c(20,200)),
  bd8= sptf_bd8(A_SOM_LOI = c(2,5)),
  bd9= sptf_bd9(A_C_OF = c(20,200)),
  bd10= sptf_bd10(A_C_OF = c(20,200)),
  bd11= sptf_bd11(A_SOM_LOI = c(2,5)),
  bd12= sptf_bd12(A_SOM_LOI = c(2,5)),
  bd13= sptf_bd13(A_SOM_LOI = c(2,5)),
  bd14= sptf_bd14(A_C_OF = c(20,200), A_CLAY_MI = c(10,15), A_SILT_MI = c(23, 10)),
  bd15= sptf_bd15(A_C_OF = c(20,200), A_CLAY_MI = c(10,15)),
  bd16= sptf_bd16(A_SOM_LOI = c(2,5)),
  bd17= sptf_bd17(A_C_OF = c(20,200), A_CLAY_MI = c(10,15), A_SILT_MI = c(23, 10)),
  bd18= sptf_bd18(A_C_OF = c(20,200), A_CLAY_MI = c(10,15), A_SAND_MI = c(12, 34), A_SILT_MI = c(23, 10)),
  bd19= sptf_bd19(A_C_OF = c(20,200), A_CLAY_MI = c(10,15), A_SAND_MI = c(12, 34), A_SILT_MI = c(23, 10)),
  bd20= sptf_bd20(A_C_OF = c(20,200), A_CLAY_MI = c(10,15), A_SAND_MI = c(12, 34), A_SILT_MI = c(23, 10)),
  bd21= sptf_bd21(A_C_OF = c(20,200), A_CLAY_MI = c(10,15), A_SILT_MI = c(23, 10)),
  bd22= sptf_bd22(A_SOM_LOI = c(2,5)),
  bd23= sptf_bd23(A_SOM_LOI = c(2,5)),
  bd24= sptf_bd24(A_SOM_LOI = c(2,5)),
  bd25= sptf_bd25(A_C_OF = c(20,200), A_CLAY_MI = c(10,15), A_SILT_MI = c(23, 10), A_H2O_T105 = c(15, 20), B_DEPTH = c(0.25, 1)),
  bd26= sptf_bd26(A_SOM_LOI = c(2,5)),
  bd27= sptf_bd27(A_SOM_LOI = c(2,5)),
  bd28= sptf_bd28(A_SAND_MI = c(12, 34), B_DEPTH = c(0.3, 1)),
  bd29= sptf_bd29(A_C_OF = c(20,200),  B_DEPTH = c(0.3, 1)),
  bd30= sptf_bd30(A_C_OF = c(20,200), A_CLAY_MI = c(10,15)),
  bd31= sptf_bd31(A_CLAY_MI = c(10,15)),
  bd32= sptf_bd32(A_C_OF = c(20,200)),
  bd33= sptf_bd33(A_SOM_LOI = c(2,5), A_CLAY_MI = c(10,15), A_SAND_MI = c(12, 34), A_SILT_MI = c(23, 10)),
  bd34= sptf_bd34(A_SOM_LOI = c(2,5)),
)


# estimate the bulk density by the pedotransfer functions

dt <- data.table(
  A_SOM_LOI= c(2,5),A_C_OF= c(20, 200),A_CLAY_MI= c(10, 20),A_SILT_MI = c(23, 10),
  A_SAND_MI= c(15, 25),B_DEPTH= c(0.3, 1),A_H2O_T105= c(20, 30),B_ALTITUDE = c(10, 20),
  B_SLOPE_DEGREE= c(3, 8),B_SLOPE_ASPECT= c(3,8),A_PH_WA = c(6,7),
  A_CACO3_MI= c(0,3),A_N_RT= c(1200, 2000)
)
dt[, p1 := sptf_bd1(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
dt[, p2 := sptf_bd2(A_SOM_LOI = A_SOM_LOI)]
dt[, p3 := sptf_bd3(A_SOM_LOI = A_SOM_LOI)]
dt[, p4 := sptf_bd4(A_C_OF = A_C_OF)]
dt[, p5 := sptf_bd5(A_C_OF = A_C_OF)]
dt[, p6 := sptf_bd6(A_SOM_LOI = A_SOM_LOI)]
dt[, p7 := sptf_bd7(A_C_OF = A_C_OF)]
dt[, p8 := sptf_bd8(A_SOM_LOI = A_SOM_LOI)]
dt[, p9 := sptf_bd9(A_C_OF = A_C_OF)]
dt[, p10 := sptf_bd10(A_C_OF = A_C_OF)]
dt[, p11 := sptf_bd11(A_SOM_LOI = A_SOM_LOI)]
dt[, p12 := sptf_bd12(A_SOM_LOI = A_SOM_LOI)]
dt[, p13 := sptf_bd13(A_SOM_LOI = A_SOM_LOI)]
dt[, p14 := sptf_bd14(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI)]
dt[, p15 := sptf_bd15(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
dt[, p16 := sptf_bd16(A_SOM_LOI = A_SOM_LOI)]
dt[, p17 := sptf_bd17(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI)]
dt[, p18 := sptf_bd18(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_SILT_MI = A_SILT_MI)]
dt[, p19 := sptf_bd19(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_SILT_MI = A_SILT_MI)]
dt[, p20 := sptf_bd20(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_SILT_MI = A_SILT_MI)]
dt[, p21 := sptf_bd21(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI)]
dt[, p22 := sptf_bd22(A_SOM_LOI = A_SOM_LOI)]
dt[, p23 := sptf_bd23(A_SOM_LOI = A_SOM_LOI)]
dt[, p24 := sptf_bd24(A_SOM_LOI = A_SOM_LOI)]
dt[, p25 := sptf_bd25(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SAND_MI,A_H2O_T105 = A_H2O_T105, B_DEPTH = B_DEPTH)]
dt[, p26 := sptf_bd26(A_SOM_LOI = A_SOM_LOI)]
dt[, p27 := sptf_bd27(A_SOM_LOI = A_SOM_LOI)]
dt[, p28 := sptf_bd28(A_SAND_MI = A_SAND_MI, B_DEPTH = B_DEPTH)]
dt[, p29 := sptf_bd29(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI,B_DEPTH = B_DEPTH)]
dt[, p30 := sptf_bd30(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
dt[, p31 := sptf_bd31(A_CLAY_MI = A_CLAY_MI)]
dt[, p32 := sptf_bd32(A_C_OF = A_C_OF)]
dt[, p33 := sptf_bd33(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_SILT_MI = A_SILT_MI)]
dt[, p34 := sptf_bd34(A_SOM_LOI = A_SOM_LOI)]
dt[, p35 := sptf_bd35(A_SOM_LOI = A_SOM_LOI)]
dt[, p36 := sptf_bd36(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI)]
dt[, p37 := sptf_bd37(A_C_OF = A_C_OF)]
dt[, p38 := sptf_bd38(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
dt[, p39 := sptf_bd39(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_H2O_T105 = A_H2O_T105,B_DEPTH = B_DEPTH)]
dt[, p40 := sptf_bd40(A_SOM_LOI = A_SOM_LOI,A_SAND_MI = A_SAND_MI,B_DEPTH = B_DEPTH)]
dt[, p41 := sptf_bd41(A_SOM_LOI = A_SOM_LOI)]
dt[, p42 := sptf_bd42(A_C_OF = A_C_OF)]
dt[, p43 := sptf_bd43(A_C_OF = A_C_OF)]
dt[, p44 := sptf_bd44(A_C_OF = A_C_OF)]
dt[, p45 := sptf_bd45(A_C_OF = A_C_OF,A_SAND_MI = A_SAND_MI)]
dt[, p46 := sptf_bd46(A_C_OF = A_C_OF,A_SAND_MI = A_SAND_MI)]
dt[, p47 := sptf_bd47(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
dt[, p48 := sptf_bd48(A_C_OF = A_C_OF)]
dt[, p49 := sptf_bd49(A_SOM_LOI = A_SOM_LOI)]
dt[, p50 := sptf_bd50(A_SOM_LOI = A_SOM_LOI)]
dt[, p51 := sptf_bd51(A_SOM_LOI = A_SOM_LOI)]
dt[, p52 := sptf_bd52(A_SOM_LOI = A_SOM_LOI)]
dt[, p53 := sptf_bd53(A_SOM_LOI = A_SOM_LOI)]
dt[, p54 := sptf_bd54(A_SOM_LOI = A_SOM_LOI)]
dt[, p55 := sptf_bd55(A_SOM_LOI = A_SOM_LOI)]
dt[, p56 := sptf_bd56(A_SOM_LOI = A_SOM_LOI)]
dt[, p57 := sptf_bd57(A_SOM_LOI = A_SOM_LOI)]
dt[, p58 := sptf_bd58(A_SOM_LOI = A_SOM_LOI)]
dt[, p59 := sptf_bd59(A_SOM_LOI = A_SOM_LOI)]
dt[, p60 := sptf_bd60(A_SOM_LOI = A_SOM_LOI)]
dt[, p61 := sptf_bd61(A_SOM_LOI = A_SOM_LOI)]
dt[, p62 := sptf_bd62(A_SOM_LOI = A_SOM_LOI)]
dt[, p63 := sptf_bd63(A_SOM_LOI = A_SOM_LOI)]
dt[, p64 := sptf_bd64(A_SOM_LOI = A_SOM_LOI)]
dt[, p65 := sptf_bd65(A_C_OF = A_C_OF)]
dt[, p66 := sptf_bd66(A_C_OF = A_C_OF)]
dt[, p67 := sptf_bd67(A_C_OF = A_C_OF)]
dt[, p68 := sptf_bd68(A_C_OF = A_C_OF)]
dt[, p69 := sptf_bd69(A_CLAY_MI = A_CLAY_MI,B_DEPTH = B_DEPTH)]
dt[, p70 := sptf_bd70(A_C_OF = A_C_OF,A_CLAY_MI=A_CLAY_MI, A_SILT_MI=A_SILT_MI, B_DEPTH=B_DEPTH, B_ALTITUDE=B_ALTITUDE, B_SLOPE_DEGREE=B_SLOPE_DEGREE, B_SLOPE_ASPECT=B_SLOPE_ASPECT)]
dt[, p71 := sptf_bd71(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,B_SLOPE_DEGREE = B_SLOPE_DEGREE)]
dt[, p72 := sptf_bd72(A_C_OF = A_C_OF)]
dt[, p73 := sptf_bd73(A_C_OF = A_C_OF)]
dt[, p74 := sptf_bd74(A_C_OF = A_C_OF)]
dt[, p75 := sptf_bd75(A_C_OF = A_C_OF)]
dt[, p76 := sptf_bd76(A_C_OF = A_C_OF)]
dt[, p77 := sptf_bd77(A_C_OF = A_C_OF)]
dt[, p78 := sptf_bd78(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_PH_WA = A_PH_WA)]
dt[, p79 := sptf_bd79(A_C_OF = A_C_OF,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI,A_PH_WA = A_PH_WA)]
dt[, p80 := sptf_bd80(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_CACO3_MI = A_CACO3_MI)]
dt[, p81 := sptf_bd81(A_C_OF = A_C_OF)]
dt[, p82 := sptf_bd82(A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
dt[, p83 := sptf_bd83(A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
dt[, p84 := sptf_bd84(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI)]
dt[, p85 := sptf_bd85(A_C_OF = A_C_OF)]
dt[, p86 := sptf_bd86(A_C_OF = A_C_OF,A_SAND_MI = A_SAND_MI,A_CLAY_MI = A_CLAY_MI)]
dt[, p87 := sptf_bd87(A_SOM_LOI = A_SOM_LOI,A_SAND_MI = A_SAND_MI,B_DEPTH = B_DEPTH)]
dt[, p88 := sptf_bd88(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_CACO3_MI = A_CACO3_MI,A_PH_WA = A_PH_WA)]
dt[, p89 := sptf_bd89(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI)]
dt[, p90 := sptf_bd90(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
dt[, p91 := sptf_bd91(A_SOM_LOI = A_SOM_LOI)]
dt[, p92 := sptf_bd92(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
dt[, p93 := sptf_bd93(A_C_OF = A_C_OF)]
dt[, p94 := sptf_bd94(A_SOM_LOI = A_SOM_LOI,A_SAND_MI = A_SAND_MI,A_CLAY_MI = A_CLAY_MI)]
dt[, p95 := sptf_bd95(A_C_OF = A_C_OF)]
dt[, p96 := sptf_bd96(A_SOM_LOI = A_SOM_LOI)]
dt[, p97 := sptf_bd97(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI)]
dt[, p98 := sptf_bd98(A_C_OF = A_C_OF)]
dt[, p99 := sptf_bd99(A_C_OF = A_C_OF)]
dt[, p100 := sptf_bd100(A_N_RT = A_N_RT,A_C_OF = A_C_OF)]
dt[, p101 := sptf_bd101(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI)]
dt[, p102 := sptf_bd102(A_C_OF = A_C_OF)]
dt[, p103 := sptf_bd103(A_C_OF = A_C_OF)]
dt[, p104 := sptf_bd104(A_C_OF = A_C_OF)]
dt[, p105 := sptf_bd105(A_C_OF = A_C_OF, B_DEPTH = B_DEPTH)]
dt[, p106 := sptf_bd106(A_C_OF = A_C_OF, B_DEPTH = B_DEPTH)]
dt[, p107 := sptf_bd107(A_C_OF = A_C_OF, B_DEPTH = B_DEPTH)]
dt[, p108 := sptf_bd108(A_C_OF = A_C_OF, B_DEPTH = B_DEPTH)]
dt[, p109 := sptf_bd109(A_C_OF = A_C_OF, B_DEPTH = B_DEPTH)]
dt[, p110 := sptf_bd110(A_C_OF = A_C_OF, B_DEPTH = B_DEPTH)]
dt[, p111 := sptf_bd111(A_C_OF = A_C_OF)]
dt[, p112 := sptf_bd112(A_C_OF = A_C_OF)]
dt[, p113 := sptf_bd113(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA)]
dt[, p114 := sptf_bd114(A_C_OF = A_C_OF)]
dt[, p115 := sptf_bd115(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_SILT_MI = A_SILT_MI)]
dt[, p116 := sptf_bd116(A_C_OF = A_C_OF)]
dt[, p117 := sptf_bd117(A_C_OF = A_C_OF)]
dt[, p118 := sptf_bd118(A_C_OF = A_C_OF)]
dt[, p119 := sptf_bd119(A_C_OF = A_C_OF)]
dt[, p120 := sptf_bd120(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI)]
dt[, p121 := sptf_bd121(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_CACO3_MI = A_CACO3_MI, A_PH_WA = A_PH_WA,B_ALTITUDE = B_ALTITUDE,B_SLOPE_DEGREE = B_SLOPE_DEGREE)]
dt[, p122 := sptf_bd122(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
dt[, p123 := sptf_bd123(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
dt[, p124 := sptf_bd124(A_C_OF = A_C_OF)]
dt[, p125 := sptf_bd125(A_C_OF = A_C_OF)]
dt[, p126 := sptf_bd126(A_C_OF = A_C_OF, A_SILT_MI = A_SILT_MI, B_DEPTH = B_DEPTH)]
dt[, p127 := sptf_bd127(A_C_OF = A_C_OF, B_DEPTH = B_DEPTH)]
dt[, p128 := sptf_bd128(A_C_OF = A_C_OF)]
dt[, p129 := sptf_bd129(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI,B_DEPTH = B_DEPTH)]
dt[, p130 := sptf_bd130(A_C_OF = A_C_OF)]
dt[, p131 := sptf_bd131(A_C_OF = A_C_OF)]
dt[, p132 := sptf_bd132(A_C_OF = A_C_OF)]
dt[, p133 := sptf_bd133(A_C_OF = A_C_OF, B_DEPTH = B_DEPTH, B_SLOPE_DEGREE = B_SLOPE_DEGREE)]
dt[, p134 := sptf_bd134(A_SOM_LOI = A_SOM_LOI)]
dt[, p135 := sptf_bd135(A_SOM_LOI = A_SOM_LOI)]
dt[, p136 := sptf_bd136(A_C_OF = A_C_OF)]
dt[, p137 := sptf_bd137(A_SOM_LOI = A_SOM_LOI,A_SILT_MI = A_SILT_MI,A_PH_WA = A_PH_WA,B_DEPTH = B_DEPTH)]
dt[, p138 := sptf_bd138(A_SOM_LOI = A_SOM_LOI, A_SILT_MI = A_SILT_MI)]
dt[, p139 := sptf_bd139(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SILT_MI)]
dt[, p140 := sptf_bd140(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
dt[, p141 := sptf_bd141(A_C_OF = A_C_OF, A_N_RT = A_N_RT,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
dt[, p142 := sptf_bd142(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
dt[, p143 := sptf_bd143(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
dt[, p144 := sptf_bd144(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
dt[, p145 := sptf_bd145(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
dt[, p146 := sptf_bd146(A_C_OF = A_C_OF)]
dt[, p147 := sptf_bd147(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI,B_DEPTH = B_DEPTH)]
dt[, p148 := sptf_bd148(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
dt[, p149 := sptf_bd149(A_C_OF = A_C_OF)]
dt[, p150 := sptf_bd150(A_C_OF = A_C_OF,B_DEPTH = B_DEPTH)]
dt[, p151 := sptf_bd151(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
dt[, p152 := sptf_bd152(A_SOM_LOI = A_SOM_LOI)]
dt[, p153 := sptf_bd153(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
dt[, p154 := sptf_bd154(A_C_OF = A_C_OF, A_PH_WA = A_PH_WA)]
dt[, p155 := sptf_bd155(A_C_OF = A_C_OF, A_SILT_MI = A_SILT_MI)]
dt[, p156 := sptf_bd156(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
dt[, p157 := sptf_bd157(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI)]
dt[, p158 := sptf_bd158(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI)]
dt[, p159 := sptf_bd159(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
dt[, p160 := sptf_bd160(A_SOM_LOI = A_SOM_LOI)]
dt[, p161 := sptf_bd161(A_SOM_LOI = A_SOM_LOI)]
dt[, p162 := sptf_bd162(A_C_OF = A_C_OF)]
dt[, p163 := sptf_bd163(A_C_OF = A_C_OF)]
dt[, p164 := sptf_bd164(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
dt[, p165 := sptf_bd165(A_SOM_LOI = A_SOM_LOI)]
dt[, p166 := sptf_bd166(A_C_OF = A_C_OF)]
# ptf167 is soil density (i.e. soil weight without pore space), but not bulk density. Excluded.
dt[, p167 := sptf_bd167(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
dt[, p168 := sptf_bd168(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
dt[, p169 := sptf_bd169(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
dt[, p170 := sptf_bd170(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
dt[, p171 := sptf_bd171(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
dt[, p172 := sptf_bd172(A_SOM_LOI = A_SOM_LOI)]
dt[, p173 := sptf_bd173(A_SOM_LOI = A_SOM_LOI)]
dt[, p174 := sptf_bd174(A_SOM_LOI = A_SOM_LOI)]
dt[, p175 := sptf_bd175(A_SOM_LOI = A_SOM_LOI)]
dt[, p176 := sptf_bd176(A_SOM_LOI = A_SOM_LOI)]
dt[, p177 := sptf_bd177(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_M50 = A_SAND_M50)]
dt[, p178 := sptf_bd178(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
dt[, p179 := sptf_bd179(A_SOM_LOI = A_SOM_LOI,A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI,A_PH_WA = A_PH_WA)]
dt[, p180 := sptf_bd180(A_SOM_LOI = A_SOM_LOI,A_SAND_MI = A_SAND_MI,A_CLAY_MI = A_CLAY_MI)]
dt[, p181 := sptf_bd181(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI)]

