# predict function

#' Predict the bulk density with the best combination of existing ptfs from literature.
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#' @param B_LU_PTFCLASS (character) The land use categorie (options: agriculture, grassland, cropland, forest, nature)
#' @param B_LOC_COUNTRY (character) The country code 
#' @param nmax (integer) the maximum number of ptfs to be included (default nmax = 5)
#'
#' @details 
#' Some of the ptfs require additional information. If given, the relevant ptf's are used, otherwise they are ignored.
#' These include the total N content (A_N_RT, unit mg/kg), the pH (A_PH_WA), the carbonate content (A_CACO3_MI, unit \%), the moisture content (A_H2O_T105, \%), the slope (B_SLOPE_DEGREE, unit degrees), the aspect (B_SLOPE_ASPECT, unit degrees) and the altidue (B_ALTITUDE, unit m).
#' When added, please ensure to use the correct element name and units. 
#' 
#' When depth is missing, the function assumes that topsoils are used.
#' 
#' @import data.table
#' 
#' @export
ptf_bd <- function(A_SOM_LOI = NA, A_C_OF = NA, 
                   A_CLAY_MI = NA, A_SAND_MI = NA, A_SILT_MI = NA, 
                   B_LU_PTFCLASS = NA,
                   B_DEPTH = 0.3, 
                   B_LOC_COUNTRY = 'NL', 
                   nmax = 5, ...){
  
  # combine all input objects not given as default function arguments
  obj <- list(...)
  obj <- as.data.table(obj)
  
  # read in internal table
  ptf.mods <- as.data.table(soilptf::sptf_bulkdensity)
  ptf.mods[,c('reference','url','soilproperties') := NULL]
  ptf.countries <- as.data.table(soilptf::sptf_countries)
  
  # subset the table for the requested country
  cont.sel <- unique(ptf.countries[country_code %in% B_LOC_COUNTRY, continent_code])
  ptf.mods <- ptf.mods[country %in% B_LOC_COUNTRY | continent %in% cont.sel]
  
  # number of sites to predict
  arg.length <- max(length(A_SOM_LOI), length(A_C_OF),length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI),
                    length(B_DEPTH),length(B_LU_PTFCLASS),length(B_LOC_COUNTRY))
  
  # make internal table
  dt <- data.table(id = 1: arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   B_DEPTH = B_DEPTH,
                   B_LOC_COUNTRY = B_LOC_COUNTRY,
                   B_LU_PTFCLASS = B_LU_PTFCLASS
                   )
  
  # estimate missing variables for texture being dependent on each other
  dt[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),.SDcols = c('A_CLAY_MI','A_SAND_MI','A_SILT_MI')]
  dt[num_obs == 2 & is.na(A_CLAY_MI), A_CLAY_MI := 100 - A_SAND_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SAND_MI), A_SAND_MI := 100 - A_CLAY_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SILT_MI), A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
  
  # add USDA soil classification
  dt[num_obs >= 2, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # estimate missing SOM variables
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 1.724]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 / 1.724]
  
  # add continent
  dt <- merge(dt,ptf.countries[,.(country_code,B_LOC_CONT = continent_code)], by.x = 'B_LOC_COUNTRY',by.y = 'country_code',all.x = TRUE)
  
  # add extra variables given as input
  checkmate::assert_data_table(obj,nrows = nrow(dt),null.ok = TRUE)
  dt <- cbind(dt,obj)
  
  # add all possible inputs as NA when missing
  cols <- c('A_PH_WA','A_CACO3_MI','A_N_RT','A_H2O_T105','A_SAND_M50','B_SLOPE_DEGREE','B_SLOPE_ASPECT','B_ALTITUDE')
  cols <- cols[!cols %in% colnames(dt)]
  dt[,c(cols) := NA_real_]
  
  # estimate the bulk density by the pedotransfer functions
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
  dt[, p100 := sptf_bd100(A_N_RT = A_N_RT)]
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
  dt[, p115 := sptf_bd115(A_C_OF = A_C_OF)]
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
  
  # melt the data
  dt2 <- melt(dt, 
             id.vars = c('id','B_LOC_COUNTRY','B_LOC_CONT','B_LU_PTFCLASS','B_SOILTYPE'),
             measure = patterns('^p'),
             variable.name = 'ptf_id')
  dt2[,ptf_id := as.integer(gsub('p','',ptf_id))]
  
  # merge with PTF properties
  dt2 <- merge(dt2,ptf.mods,by = 'ptf_id')
  
  # select only relevant cases
  dt2 <- dt2[!is.na(value) & value > 500 & value < 3000]
  
  # add applicability factor
  dt2[,ap := 0]
  dt2[B_LOC_COUNTRY == country_code, ap := ap + 1]
  dt2[B_LOC_CONT == continent_code, ap := ap + 1]
  dt2[B_LU_PTFCLASS == landuse | landuse == 'variable' | is.na(landuse),ap := ap + 1]
  
  # add default r2 for ptfs that are unknown
  dt2[is.na(r2), r2 := 0.7]
  
  # add order to filter the best ones
  dt2[, oid := frank(-ap,ties.method = 'first',na.last = 'keep'),by = id]

  # estimate the mean and SD of the bulk density for nmax models
  out <- dt2[oid <= nmax, list(bd.mean = weighted.mean(x = value, w = r2),
                               bd.sd = sd(x = value)), by = 'id']
  
  return(out)
}
