#' Predict the bulk density with existing ptfs from literature
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
#' @param ... other arguments
#' 
#' @details 
#' Some of the ptfs require additional information. If given, the relevant ptf's are used, otherwise they are ignored.
#' These include the total N content (A_N_RT, unit mg/kg), the pH (A_PH_WA), the carbonate content (A_CACO3_IF, unit \%), the moisture content (A_H2O_T105, \%), the slope (B_SLOPE_DEGREE, unit degrees), the aspect (B_SLOPE_ASPECT, unit degrees) and the altidue (B_ALTITUDE, unit m).
#' When added, please ensure to use the correct element name and units. 
#' 
#' When depth is missing, the function assumes that topsoils are used.
#' 
#' @import data.table
#' 
#' @importFrom stats weighted.mean
#' 
#' @export
ptf_bd_old <- function(A_SOM_LOI = NA_real_, A_C_OF = NA_real_, 
                   A_CLAY_MI = NA_real_, A_SAND_MI = NA_real_, A_SILT_MI = NA_real_, 
                   B_LU_PTFCLASS = NA_real_,
                   B_DEPTH = 0.3, 
                   B_LOC_COUNTRY = 'NL', 
                   nmax = 5,
                   ...){
  
  # combine all input objects not given as default function arguments
  obj <- list(...)
  obj <- as.data.table(obj)
  if(length(obj)==0){obj <- NULL}
  
  # add visual bindings
  country_code = continent_code = num_obs = B_SOILTYPE = . = NULL
  p1 = p2 = p3 = p4 = p5 = p6 = p7 = p8 = p9 = p10 = p11 = p12 = p13 = p14 = p15= p16 = p17 = p18 = p19 = NULL
  p20 = p21 = p22 = p23 = p24 = p25 = p26 = p27 = p28 = p29 = p30 = p31 = p32 = p33 = p34 = p35 = p36 = p37 = p38 = p39 = NULL
  p40 = p41 = p42 = p43 = p44 = p45 = p46 = p47 = p48 = p49 = p50 = p51 = p52 = p53 = p54 = p55 = p56 = p57 = p58 = p59 = NULL
  p20 = p21 = p22 = p23 = p24 = p25 = p26 = p27 = p28 = p29 = NULL
  p60 = p61 = p62 = p63 = p64 = p65 = p66 = p67 = p68 = p69 = NULL
  p70 = p71 = p72 = p73 = p74 = p75 = p76 = p77 = p78 = p79= NULL
  p80 = p81 = p82 = p83 = p84 = p85 = p86 = p87 = p88 = p89 = NULL
  p90 = p91 = p92 = p93 = p94 = p95 = p96 = p97 = p98 = p99 = NULL
  p100 = p101 = p102 = p103 = p104 = p105 = p106 = p107 = p108 = p109 = NULL
  p110 = p111 = p112 = p113 = p114 = p115 = p116 = p117 = p118 = p119 = NULL
  p120 = p121 = p122 = p123 = p124 = p125 = p126 = p127 = p128 = p129 = NULL
  p130 = p131 = p132 = p133 = p134 = p135 = p136 = p137 = p138 = p139 = NULL
  p140 = p141 = p142 = p143 = p144 = p145 = p146 = p147 = p148 = p149 = NULL
  p150 = p151 = p152 = p153 = p154 = p155 = p156 = p157 = p158 = p159 = NULL
  p160 = p161 = p162 = p163 = p164 = p165 = p166 = p167 = p168 = p169 = NULL
  p170 = p171 = p172 = p173 = p174 = p175 = p176 = p177 = p178 = p179 = NULL
  p180 = p181 = p182 = p183 = p184 = p185 = p186 = p187 = p188 = p189 = NULL
  landuse = value = ptf_id = patterns = ap = depth = r2 = oid = id = soiltype = NULL
  B_LOC_CONT = A_DEPTH = A_CACO3_IF = NULL
  
  # read in internal table
  ptf.mods <- as.data.table(soilptf::sptf_bulkdensity)
  ptf.mods[,c('reference','url','soilproperties') := NULL]
  ptf.countries <- as.data.table(soilptf::sptf_countries)

  # # subset the table for the requested country
  cont.sel <- unique(ptf.countries[country_code %in% B_LOC_COUNTRY, continent_code])
  ptf.mods <- ptf.mods[country_code %in% B_LOC_COUNTRY | continent_code %in% cont.sel]
  
  # number of sites to predict
  arg.length <- max(length(A_SOM_LOI), length(A_C_OF),length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI),
                    length(A_DEPTH),length(B_LU_PTFCLASS),length(B_LOC_COUNTRY))
  
  # make internal table
  dt <- data.table(id = 1: arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_DEPTH = A_DEPTH,
                   B_LOC_COUNTRY = B_LOC_COUNTRY,
                   B_LU_PTFCLASS = B_LU_PTFCLASS
                   )
  
  # estimate missing variables for texture being dependent on each other
  dt[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),.SDcols = c('A_CLAY_MI','A_SAND_MI','A_SILT_MI')]
  dt[num_obs == 2 & is.na(A_CLAY_MI), A_CLAY_MI := 100 - A_SAND_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SAND_MI), A_SAND_MI := 100 - A_CLAY_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SILT_MI), A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
  
  # add USDA soil classification
  dt2[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # estimate missing SOM variables
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 1.724]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 / 1.724]
  
  # add continent
  dt <- merge(dt,ptf.countries[,.(country_code,B_LOC_CONT = continent_code)], by.x = 'B_LOC_COUNTRY',by.y = 'country_code',all.x = TRUE)
  
  # add extra variables given as input
  checkmate::assert_data_table(obj,nrows = nrow(dt),null.ok = TRUE)
  dt <- cbind(dt,obj)
  
  # add visual bindings when no input is given
  if(!'A_H2O_T105' %in% colnames(dt)){A_H2O_T105 = NULL}
  if(!'A_PH_WA' %in% colnames(dt)){A_PH_WA = NULL}
  if(!'A_CACO3_MI' %in% colnames(dt)){A_CACO3_MI = NULL}
  if(!'A_N_RT' %in% colnames(dt)){A_N_RT = NULL}
  if(!'A_SAND_M50' %in% colnames(dt)){A_SAND_M50 = NULL}
  if(!'B_SLOPE_DEGREE' %in% colnames(dt)){B_SLOPE_DEGREE = NULL}
  if(!'B_SLOPE_ASPECT' %in% colnames(dt)){B_SLOPE_ASPECT = NULL}
  if(!'B_ALTITUDE' %in% colnames(dt)){B_ALTITUDE = NULL}
  
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
  dt[, p25 := sptf_bd25(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SAND_MI,A_H2O_T105 = A_H2O_T105, A_DEPTH = A_DEPTH)]
  dt[, p26 := sptf_bd26(A_SOM_LOI = A_SOM_LOI)]
  dt[, p27 := sptf_bd27(A_SOM_LOI = A_SOM_LOI)]
  dt[, p28 := sptf_bd28(A_SAND_MI = A_SAND_MI, A_DEPTH = A_DEPTH)]
  dt[, p29 := sptf_bd29(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI,A_DEPTH = A_DEPTH)]
  dt[, p30 := sptf_bd30(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p31 := sptf_bd31(A_CLAY_MI = A_CLAY_MI)]
  dt[, p32 := sptf_bd32(A_C_OF = A_C_OF)]
  dt[, p33 := sptf_bd33(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_SILT_MI = A_SILT_MI)]
  dt[, p34 := sptf_bd34(A_SOM_LOI = A_SOM_LOI)]
  dt[, p35 := sptf_bd35(A_SOM_LOI = A_SOM_LOI)]
  dt[, p36 := sptf_bd36(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI)]
  dt[, p37 := sptf_bd37(A_C_OF = A_C_OF)]
  dt[, p38 := sptf_bd38(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
  dt[, p39 := sptf_bd39(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_H2O_T105 = A_H2O_T105,A_DEPTH = A_DEPTH)]
  dt[, p40 := sptf_bd40(A_SOM_LOI = A_SOM_LOI,A_SAND_MI = A_SAND_MI,A_DEPTH = A_DEPTH)]
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
  dt[, p69 := sptf_bd69(A_CLAY_MI = A_CLAY_MI,A_DEPTH = A_DEPTH)]
  dt[, p70 := sptf_bd70(A_C_OF = A_C_OF,A_CLAY_MI=A_CLAY_MI, A_SILT_MI=A_SILT_MI, A_DEPTH=A_DEPTH, B_ALTITUDE=B_ALTITUDE, B_SLOPE_DEGREE=B_SLOPE_DEGREE, B_SLOPE_ASPECT=B_SLOPE_ASPECT)]
  dt[, p71 := sptf_bd71(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,B_SLOPE_DEGREE = B_SLOPE_DEGREE)]
  dt[, p72 := sptf_bd72(A_C_OF = A_C_OF)]
  dt[, p73 := sptf_bd73(A_C_OF = A_C_OF)]
  dt[, p74 := sptf_bd74(A_C_OF = A_C_OF)]
  dt[, p75 := sptf_bd75(A_C_OF = A_C_OF)]
  dt[, p76 := sptf_bd76(A_C_OF = A_C_OF)]
  dt[, p77 := sptf_bd77(A_C_OF = A_C_OF)]
  dt[, p78 := sptf_bd78(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_PH_WA = A_PH_WA)]
  dt[, p79 := sptf_bd79(A_C_OF = A_C_OF,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI,A_PH_WA = A_PH_WA)]
  dt[, p80 := sptf_bd80(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_CACO3_IF = A_CACO3_IF)]
  dt[, p81 := sptf_bd81(A_C_OF = A_C_OF)]
  dt[, p82 := sptf_bd82(A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
  dt[, p83 := sptf_bd83(A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
  dt[, p84 := sptf_bd84(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI)]
  dt[, p85 := sptf_bd85(A_C_OF = A_C_OF)]
  dt[, p86 := sptf_bd86(A_C_OF = A_C_OF,A_SAND_MI = A_SAND_MI,A_CLAY_MI = A_CLAY_MI)]
  dt[, p87 := sptf_bd87(A_SOM_LOI = A_SOM_LOI,A_SAND_MI = A_SAND_MI,A_DEPTH = A_DEPTH)]
  dt[, p88 := sptf_bd88(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_CACO3_IF = A_CACO3_IF,A_PH_WA = A_PH_WA)]
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
  dt[, p105 := sptf_bd105(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
  dt[, p106 := sptf_bd106(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
  dt[, p107 := sptf_bd107(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
  dt[, p108 := sptf_bd108(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
  dt[, p109 := sptf_bd109(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
  dt[, p110 := sptf_bd110(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
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
  dt[, p121 := sptf_bd121(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_CACO3_IF = A_CACO3_IF, A_PH_WA = A_PH_WA,B_ALTITUDE = B_ALTITUDE,B_SLOPE_DEGREE = B_SLOPE_DEGREE)]
  dt[, p122 := sptf_bd122(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
  dt[, p123 := sptf_bd123(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
  dt[, p124 := sptf_bd124(A_C_OF = A_C_OF)]
  dt[, p125 := sptf_bd125(A_C_OF = A_C_OF)]
  dt[, p126 := sptf_bd126(A_C_OF = A_C_OF, A_SILT_MI = A_SILT_MI, A_DEPTH = A_DEPTH)]
  dt[, p127 := sptf_bd127(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
  dt[, p128 := sptf_bd128(A_C_OF = A_C_OF)]
  dt[, p129 := sptf_bd129(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI,A_DEPTH = A_DEPTH)]
  dt[, p130 := sptf_bd130(A_C_OF = A_C_OF)]
  dt[, p131 := sptf_bd131(A_C_OF = A_C_OF)]
  dt[, p132 := sptf_bd132(A_C_OF = A_C_OF)]
  dt[, p133 := sptf_bd133(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH, B_SLOPE_DEGREE = B_SLOPE_DEGREE)]
  dt[, p134 := sptf_bd134(A_SOM_LOI = A_SOM_LOI)]
  dt[, p135 := sptf_bd135(A_SOM_LOI = A_SOM_LOI)]
  dt[, p136 := sptf_bd136(A_C_OF = A_C_OF)]
  dt[, p137 := sptf_bd137(A_SOM_LOI = A_SOM_LOI,A_SILT_MI = A_SILT_MI,A_PH_WA = A_PH_WA,A_DEPTH = A_DEPTH)]
  dt[, p138 := sptf_bd138(A_SOM_LOI = A_SOM_LOI, A_SILT_MI = A_SILT_MI)]
  dt[, p139 := sptf_bd139(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SILT_MI)]
  dt[, p140 := sptf_bd140(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
  dt[, p141 := sptf_bd141(A_C_OF = A_C_OF, A_N_RT = A_N_RT,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
  dt[, p142 := sptf_bd142(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
  dt[, p143 := sptf_bd143(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
  dt[, p144 := sptf_bd144(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
  dt[, p145 := sptf_bd145(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
  dt[, p146 := sptf_bd146(A_C_OF = A_C_OF)]
  dt[, p147 := sptf_bd147(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI,A_DEPTH = A_DEPTH)]
  dt[, p148 := sptf_bd148(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
  dt[, p149 := sptf_bd149(A_C_OF = A_C_OF)]
  dt[, p150 := sptf_bd150(A_C_OF = A_C_OF,A_DEPTH = A_DEPTH)]
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
  #dt[, p167 := sptf_bd167(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
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
              id.vars = c('id','A_SOM_LOI', "A_C_OF", "A_CLAY_MI", "A_SAND_MI", "A_SILT_MI", "A_DEPTH"),
              measure = patterns('^p'),
              variable.name = 'ptf_id')
  dt2[,ptf_id := as.integer(gsub('p','',ptf_id))]
  
  # merge with PTF properties
  dt2 <- merge(dt2,ptf.mods,by = 'ptf_id')
  
  # select only relevant cases
  dt2 <- dt2[!is.na(value) & value > 500 & value < 3000]
  
  # add applicability factor given country, continent, depth, land use, and soil type
  dt2[,ap := 0]
  dt2[B_LOC_COUNTRY == country_code, ap := ap + 1]
  dt2[B_LOC_CONT == continent_code, ap := ap + 1]
  dt2[B_LU_PTFCLASS == landuse | landuse == 'variable' | is.na(landuse),ap := ap + 1]
  dt2[B_DEPTH < 30 & depth < 50, ap := ap + 0.5]
  dt2[grepl('sand',B_SOILTYPE) & grepl('sand|variable',soiltype) | is.na(soiltype), ap := ap + 0.33]
  dt2[grepl('clay',B_SOILTYPE) & grepl('clay|variable',soiltype) | is.na(soiltype), ap := ap + 0.33]
  dt2[grepl('loam',B_SOILTYPE) & grepl('loam|variable',soiltype) | is.na(soiltype), ap := ap + 0.33]
  dt2[A_SOM_LOI > 20 & grepl('peat|organic',soiltype), ap := ap + 1]
  
  # add default r2 for ptfs that are unknown
  dt2[is.na(r2), r2 := 0.7]
  
  # add order to filter the best ones
  dt2[, oid := frank(-ap,ties.method = 'first',na.last = 'keep'),by = id]
  
  # estimate the mean and SD of the bulk density for nmax models
  out <- dt2[oid <= nmax, list(bd.mean = weighted.mean(x = value, w = r2),
                               bd.sd = sd(x = value)), by = 'id']
  
  return(out)
  
}

  
#' Predict the bulk density with existing ptfs from literature
#'
#' @param dt (data table) Data table which includes at least:
#' A_SOM_LOI or A_C_OF
#' 2 of A_CLAY_MI, A_SAND_MI, A_SILT_MI
#' A_DEPTH
#' id
#'
#' @details 
#' This function returns a melted form of data table, containing values of predicted BD with different PTFs
#' When depth is missing, the function assumes that topsoils are used.
#' 
#' @import data.table
#' 
#' @importFrom stats weighted.mean
#' 
#' @export
ptf_bd_all <- function(dt){
        
    ## add visual binding ---
    p1 = p2 = p3 = p4 = p5 = p6 = p7 = p8 = p9 = p10 = p11 = p12 = p13 = p14 = NULL
    p15 = p16 = p17 = p18 = p19 = p20 = p21 = p22 = p23 = p24 = p25 = p26 = NULL
    p27 = p28 = p29 = p30 = p31 = p32 = p33 = p34 = p35 = p36 = p37 = p38 = NULL
    p39 = p40 = p41 = p42 = p43 = p44 = p45 = p46 = p47 = p48 = p49 = p50 = NULL
    p51 = p52 = p53 = p54 = p55 = p56 = p57 = p58 = p59 = p60 = p61 = p62 = NULL
    p63 = p64 = p65 = p66 = p67 = p68 = p69 = p70 = p71 = p72 = p73 = p74 = NULL
    p75 = p76 = p77 = p78 = p79 = p80 = p81 = p82 = p83 = p84 = p85 = p86 = NULL
    p87 = p88 = p89 = p90 = p91 = p92 = p93 = p94 = p95 = p96 = p97 = p98 = NULL
    p99 = p100 = p101 = p102 = p103 = p104 = p105 = p106 = p107 = p108 = p109 =NULL
    p110 = p111 = p112 = p113 = p114 = p115 = p116 = p117 = p118 = p119 = p120 = NULL
    p121 = p122 = p123 = p124 = p125 = p126 = p127 = p128 = p129 = p130 = p131 = NULL
    p132 = p133 = p134 = p135 = p136 = p137 = p138 = p139 = p140 = p141 = p142 = NULL
    p143 = p144 = p145 = p146 = p147 = p148 = p149 = p150 = p151 = p152 = p153 = NULL
    p154 = p155 = p156 = p157 = p158 = p159 = p160 = p161 = p162 = p163 = p164 = NULL
    p165 = p166 = p167 = p168 = p169 = p170 = p171 = p172 = p173 = p174 = p175 = NULL
    p176 = p177 = p178 = p179 = p180 = p181 = NULL
    num_obs = A_CLAY_MI = A_SAND_MI = A_SILT_MI = A_SOM_LOI = A_C_OF = A_H20_T105 = NULL
    A_DEPTH = B_ALTITUDE = B_SLOPE_DEGREE = B_SLOPE_ASPECT = A_PH_WA = A_CACO3_IF = NULL
    A_N_RT = A_SAND_M50 = A_H2O_T105 = ptf_id = patterns = NULL
        
    dt <- copy(dt)
     
    # add all possible inputs as NA when missing
    cols <- c('A_CLAY_MI','A_SAND_MI','A_SILT_MI', 'A_C_OF', 'A_DEPTH',
              'A_PH_WA','A_CACO3_IF','A_N_RT','A_H2O_T105','A_SAND_M50','B_SLOPE_DEGREE','B_SLOPE_ASPECT','B_ALTITUDE')
    cols <- cols[!cols %in% colnames(dt)]
    dt[,c(cols) := NA_real_]
    
    # estimate missing variables for texture being dependent on each other
    dt[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),.SDcols = c('A_CLAY_MI','A_SAND_MI','A_SILT_MI')]
    dt[num_obs == 2 & is.na(A_CLAY_MI), A_CLAY_MI := 100 - A_SAND_MI - A_SILT_MI]
    dt[num_obs == 2 & is.na(A_SAND_MI), A_SAND_MI := 100 - A_CLAY_MI - A_SILT_MI]
    dt[num_obs == 2 & is.na(A_SILT_MI), A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
    
    # estimate missing SOM variables
    dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 1.724]
    dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 / 1.724]
        
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
    dt[, p25 := sptf_bd25(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SAND_MI,A_H2O_T105 = A_H2O_T105, A_DEPTH = A_DEPTH)]
    dt[, p26 := sptf_bd26(A_SOM_LOI = A_SOM_LOI)]
    dt[, p27 := sptf_bd27(A_SOM_LOI = A_SOM_LOI)]
    dt[, p28 := sptf_bd28(A_SAND_MI = A_SAND_MI, A_DEPTH = A_DEPTH)]
    dt[, p29 := sptf_bd29(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI,A_DEPTH = A_DEPTH)]
    dt[, p30 := sptf_bd30(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
    dt[, p31 := sptf_bd31(A_CLAY_MI = A_CLAY_MI)]
    dt[, p32 := sptf_bd32(A_C_OF = A_C_OF)]
    dt[, p33 := sptf_bd33(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_SILT_MI = A_SILT_MI)]
    dt[, p34 := sptf_bd34(A_SOM_LOI = A_SOM_LOI)]
    dt[, p35 := sptf_bd35(A_SOM_LOI = A_SOM_LOI)]
    dt[, p36 := sptf_bd36(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI)]
    dt[, p37 := sptf_bd37(A_C_OF = A_C_OF)]
    dt[, p38 := sptf_bd38(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
    dt[, p39 := sptf_bd39(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_H2O_T105 = A_H2O_T105,A_DEPTH = A_DEPTH)]
    dt[, p40 := sptf_bd40(A_SOM_LOI = A_SOM_LOI,A_SAND_MI = A_SAND_MI,A_DEPTH = A_DEPTH)]
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
    dt[, p69 := sptf_bd69(A_CLAY_MI = A_CLAY_MI,A_DEPTH = A_DEPTH)]
    dt[, p70 := sptf_bd70(A_C_OF = A_C_OF,A_CLAY_MI=A_CLAY_MI, A_SILT_MI=A_SILT_MI, A_DEPTH=A_DEPTH, B_ALTITUDE=B_ALTITUDE, B_SLOPE_DEGREE=B_SLOPE_DEGREE, B_SLOPE_ASPECT=B_SLOPE_ASPECT)]
    dt[, p71 := sptf_bd71(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,B_SLOPE_DEGREE = B_SLOPE_DEGREE)]
    dt[, p72 := sptf_bd72(A_C_OF = A_C_OF)]
    dt[, p73 := sptf_bd73(A_C_OF = A_C_OF)]
    dt[, p74 := sptf_bd74(A_C_OF = A_C_OF)]
    dt[, p75 := sptf_bd75(A_C_OF = A_C_OF)]
    dt[, p76 := sptf_bd76(A_C_OF = A_C_OF)]
    dt[, p77 := sptf_bd77(A_C_OF = A_C_OF)]
    dt[, p78 := sptf_bd78(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_PH_WA = A_PH_WA)]
    dt[, p79 := sptf_bd79(A_C_OF = A_C_OF,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI,A_PH_WA = A_PH_WA)]
    dt[, p80 := sptf_bd80(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_CACO3_IF = A_CACO3_IF)]
    dt[, p81 := sptf_bd81(A_C_OF = A_C_OF)]
    dt[, p82 := sptf_bd82(A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
    dt[, p83 := sptf_bd83(A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
    dt[, p84 := sptf_bd84(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI)]
    dt[, p85 := sptf_bd85(A_C_OF = A_C_OF)]
    dt[, p86 := sptf_bd86(A_C_OF = A_C_OF,A_SAND_MI = A_SAND_MI,A_CLAY_MI = A_CLAY_MI)]
    dt[, p87 := sptf_bd87(A_SOM_LOI = A_SOM_LOI,A_SAND_MI = A_SAND_MI,A_DEPTH = A_DEPTH)]
    dt[, p88 := sptf_bd88(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_CACO3_IF = A_CACO3_IF,A_PH_WA = A_PH_WA)]
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
    dt[, p105 := sptf_bd105(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
    dt[, p106 := sptf_bd106(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
    dt[, p107 := sptf_bd107(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
    dt[, p108 := sptf_bd108(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
    dt[, p109 := sptf_bd109(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
    dt[, p110 := sptf_bd110(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
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
    dt[, p121 := sptf_bd121(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_CACO3_IF = A_CACO3_IF, A_PH_WA = A_PH_WA,B_ALTITUDE = B_ALTITUDE,B_SLOPE_DEGREE = B_SLOPE_DEGREE)]
    dt[, p122 := sptf_bd122(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
    dt[, p123 := sptf_bd123(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
    dt[, p124 := sptf_bd124(A_C_OF = A_C_OF)]
    dt[, p125 := sptf_bd125(A_C_OF = A_C_OF)]
    dt[, p126 := sptf_bd126(A_C_OF = A_C_OF, A_SILT_MI = A_SILT_MI, A_DEPTH = A_DEPTH)]
    dt[, p127 := sptf_bd127(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH)]
    dt[, p128 := sptf_bd128(A_C_OF = A_C_OF)]
    dt[, p129 := sptf_bd129(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI,A_DEPTH = A_DEPTH)]
    dt[, p130 := sptf_bd130(A_C_OF = A_C_OF)]
    dt[, p131 := sptf_bd131(A_C_OF = A_C_OF)]
    dt[, p132 := sptf_bd132(A_C_OF = A_C_OF)]
    dt[, p133 := sptf_bd133(A_C_OF = A_C_OF, A_DEPTH = A_DEPTH, B_SLOPE_DEGREE = B_SLOPE_DEGREE)]
    dt[, p134 := sptf_bd134(A_SOM_LOI = A_SOM_LOI)]
    dt[, p135 := sptf_bd135(A_SOM_LOI = A_SOM_LOI)]
    dt[, p136 := sptf_bd136(A_C_OF = A_C_OF)]
    dt[, p137 := sptf_bd137(A_SOM_LOI = A_SOM_LOI,A_SILT_MI = A_SILT_MI,A_PH_WA = A_PH_WA,A_DEPTH = A_DEPTH)]
    dt[, p138 := sptf_bd138(A_SOM_LOI = A_SOM_LOI, A_SILT_MI = A_SILT_MI)]
    dt[, p139 := sptf_bd139(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SILT_MI)]
    dt[, p140 := sptf_bd140(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
    dt[, p141 := sptf_bd141(A_C_OF = A_C_OF, A_N_RT = A_N_RT,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
    dt[, p142 := sptf_bd142(A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
    dt[, p143 := sptf_bd143(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
    dt[, p144 := sptf_bd144(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI)]
    dt[, p145 := sptf_bd145(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI)]
    dt[, p146 := sptf_bd146(A_C_OF = A_C_OF)]
    dt[, p147 := sptf_bd147(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI,A_SAND_MI = A_SAND_MI,A_SILT_MI = A_SILT_MI,A_DEPTH = A_DEPTH)]
    dt[, p148 := sptf_bd148(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
    dt[, p149 := sptf_bd149(A_C_OF = A_C_OF)]
    dt[, p150 := sptf_bd150(A_C_OF = A_C_OF,A_DEPTH = A_DEPTH)]
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
    #dt[, p167 := sptf_bd167(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI)]
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
                id.vars = c('id','A_SOM_LOI', "A_C_OF", "A_CLAY_MI", "A_SAND_MI", "A_SILT_MI", "A_DEPTH"),
                measure = patterns('^p'),
                variable.name = 'ptf_id')
    dt2[,ptf_id := as.integer(gsub('p','',ptf_id))]
    
    # # merge with PTF properties
    # dt2 <- merge(dt2,ptf.mods,by = 'ptf_id')
    
    return(dt2)
        
}

  

#' Predict the bulk density with the best combination of existing ptfs from literature.
#' (modified version, using the support function ptf_bd_all)
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_DEPTH (numeric) The depth of the sampled soil layer (m)
#' @param B_LU_PTFCLASS (character) The land use categorie (options: agriculture, grassland, cropland, forest, nature)
#' @param B_LOC_COUNTRY (character) The country code 
#' @param nmax (integer) the maximum number of ptfs to be included (default nmax = 5)
#' @param ... other arguments
#' 
#' @details 
#' Some of the ptfs require additional information. If given, the relevant ptf's are used, otherwise they are ignored.
#' These include the total N content (A_N_RT, unit mg/kg), the pH (A_PH_WA), the carbonate content (A_CACO3_IF, unit \%), the moisture content (A_H2O_T105, \%), the slope (B_SLOPE_DEGREE, unit degrees), the aspect (B_SLOPE_ASPECT, unit degrees) and the altidue (B_ALTITUDE, unit m).
#' When added, please ensure to use the correct element name and units. 
#' 
#' When depth is missing, the function assumes that topsoils are used.
#' 
#' @import data.table
#' 
#' @export
ptf_bd <- function(A_SOM_LOI = NA_real_, A_C_OF = NA_real_, 
                   A_CLAY_MI = NA_real_, A_SAND_MI = NA_real_, A_SILT_MI = NA_real_, 
                   B_LU_PTFCLASS = NA_real_,
                   A_DEPTH = 0.3, 
                   B_LOC_COUNTRY = 'NL', 
                   nmax = 5, ...){
  
  num_obs = A_CLAY_MI = A_SAND_MI = A_SILT_MI = A_SOM_LOI = A_C_OF = A_H20_T105 = NULL
  A_DEPTH = B_ALTITUDE = B_SLOPE_DEGREE = B_SLOPE_ASPECT = A_PH_WA = A_CACO3_IF = NULL
  A_N_RT = A_SAND_M50 = NULL
  ptf_id = value = ap = B_LOC_CONT = landuse = depth = soiltype = r2 = oid = id =NULL
  country_code = continent_code = num_obs = B_SOILTYPE = . = NULL
  
  # combine all input objects not given as default function arguments
  obj <- list(...)
  obj <- as.data.table(obj)
  if(length(obj)==0){obj <- NULL}
  
  # read in internal table
  ptf.mods <- as.data.table(soilptf::sptf_bulkdensity)
  ptf.mods[,c('reference','url','soilproperties') := NULL]
  ptf.countries <- as.data.table(soilptf::sptf_countries)
  
  # number of sites to predict
  arg.length <- max(length(A_SOM_LOI), length(A_C_OF),length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI),
                    length(A_DEPTH),length(B_LU_PTFCLASS),length(B_LOC_COUNTRY))
  
  # make internal table
  dt <- data.table(id = 1: arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_DEPTH = A_DEPTH,
                   B_LOC_COUNTRY = B_LOC_COUNTRY,
                   B_LU_PTFCLASS = B_LU_PTFCLASS
  )
  
  # calculate BD with different PTF's
  dt2 <- ptf_bd_all(dt)
  
  # add B_LOC_COUNTRY and B_LU_PTFCLASS
  dt2 <- merge(dt2, dt[, .(id, B_LOC_COUNTRY, B_LU_PTFCLASS)], by = "id", all.x = T)
  
  # add USDA soil classification
  dt2[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # add continent
  dt2 <- merge(dt2,ptf.countries[,.(country_code,B_LOC_CONT = continent_code)], by.x = 'B_LOC_COUNTRY',by.y = 'country_code',all.x = TRUE)
  
  # add extra variables given as input
  checkmate::assert_data_table(obj,nrows = nrow(dt),null.ok = TRUE)
  dt <- cbind(dt,obj)

  # # subset the table for the requested country
  # cont.sel <- unique(ptf.countries[country_code %in% B_LOC_COUNTRY, continent_code])
  # ptf.mods <- ptf.mods[country_code %in% B_LOC_COUNTRY | continent_code %in% cont.sel]
  
  # merge with PTF properties
  dt2 <- merge(dt2,ptf.mods,by = 'ptf_id')
  
  # select only relevant cases
  dt2 <- dt2[!is.na(value) & value > 500 & value < 3000]
  

  # add applicability factor given country, continent, depth, land use, and soil type
  dt2[,ap := 0]
  dt2[B_LOC_COUNTRY == country_code, ap := ap + 1]
  dt2[B_LOC_CONT == continent_code, ap := ap + 1]
  dt2[B_LU_PTFCLASS == landuse | landuse == 'variable' | is.na(landuse),ap := ap + 1]
  dt2[A_DEPTH < 30 & depth < 50, ap := ap + 0.5]
  dt2[grepl('sand',B_SOILTYPE) & grepl('sand|variable',soiltype) | is.na(soiltype), ap := ap + 0.33]
  dt2[grepl('clay',B_SOILTYPE) & grepl('clay|variable',soiltype) | is.na(soiltype), ap := ap + 0.33]
  dt2[grepl('loam',B_SOILTYPE) & grepl('loam|variable',soiltype) | is.na(soiltype), ap := ap + 0.33]
  dt2[A_SOM_LOI > 20 & grepl('peat|organic',soiltype), ap := ap + 1]
  
  # add default r2 for ptfs that are unknown
  dt2[is.na(r2), r2 := 0.7]
  
  # add order to filter the best ones
  dt2[, oid := frank(-ap,ties.method = 'first',na.last = 'keep'),by = id]
  
  # estimate the mean and SD of the bulk density for nmax models
  out <- dt2[oid <= nmax, list(bd.mean = weighted.mean(x = value, w = r2),
                               bd.sd = sd(x = value)), by = 'id']
  
  return(out)
}


#' Predict the bulk density with the best combination of existing ptfs from literature.
#'  (modified version, using the support function ptf_bd_all)
#'
#' @param A_DEPTH (numeric) The depth of the sampled soil layer (m)
#' @param B_LU_PTFCLASS (character) The land use categorie (options: agriculture, grassland, cropland, forest, nature)
#' @param B_LOC_COUNTRY (character) The country code 
#' @param nboot (integer) the number of bootstrapped samples (x 1000) to predict soil density
#'
#' @details 
#' Some of the ptfs require additional information. If given, the relevant ptf's are used, otherwise they are ignored.
#' These include the total N content (A_N_RT, unit mg/kg), the pH (A_PH_WA), the carbonate content (A_CACO3_IF, unit \%), the moisture content (A_H2O_T105, \%), the slope (B_SLOPE_DEGREE, unit degrees), the aspect (B_SLOPE_ASPECT, unit degrees) and the altidue (B_ALTITUDE, unit m).
#' When added, please ensure to use the correct element name and units. 
#' 
#' When depth is missing, the function assumes that topsoils are used.
#' 
#' @import data.table
#' @importFrom stats lm median rnorm sd weighted.mean
#' @importFrom stats lm rnorm sd
#' 
#' @export
ptf_bd_lm <- function(B_LU_PTFCLASS = NA_character_,
                      A_DEPTH = 0.3, 
                      B_LOC_COUNTRY = 'NL', 
                      nboot = 10){
  
  # add visual binding 
  A_CLAY_MI = A_SAND_MI = A_SILT_MI = A_SOM_LOI = A_C_OF = A_H20_T105 = NULL
  A_DEPTH = B_ALTITUDE = B_SLOPE_DEGREE = B_SLOPE_ASPECT = A_PH_WA = A_CACO3_IF = NULL
  A_N_RT = A_SAND_M50 = B_SOILTYPE_AGR = B_SOILTYPE = id = NULL
  patterns = ptf_id = country_code = continent_code = . = ptf_id = nsample = r2 = NULL
  nrep = value = error = NULL
    
  # read in internal table
  ptf.mods <- as.data.table(soilptf::sptf_bulkdensity)
  ptf.mods[,c('reference','url','soilproperties') := NULL]
  ptf.countries <- as.data.table(soilptf::sptf_countries)
  
  # subset the table for the requested country
  cont.sel <- unique(ptf.countries[country_code %in% B_LOC_COUNTRY, continent_code])
  ptf.mods <- ptf.mods[country_code %in% B_LOC_COUNTRY | continent_code %in% cont.sel]
  
  # make internal table
  dt <- data.table(id = 1: 40,
                   A_SOM_LOI = seq(0.1,20,0.5),
                   A_CLAY_MI = 7.5,
                   A_SAND_MI = 60,
                   A_DEPTH = A_DEPTH,
                   B_LOC_COUNTRY = B_LOC_COUNTRY,
                   B_LU_PTFCLASS = B_LU_PTFCLASS)
  
  # estimate missing variables
  dt[!is.na(A_SOM_LOI), A_C_OF := A_SOM_LOI * 10 / 1.724]
  dt[,A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
  
  # add USDA soil classification
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # add continent
  dt <- merge(dt,ptf.countries[,.(country_code,B_LOC_CONT = continent_code)], by.x = 'B_LOC_COUNTRY',by.y = 'country_code',all.x = TRUE)
  
  # add all possible inputs as NA when missing
  cols <- c('A_PH_WA','A_CACO3_IF','A_N_RT','A_H2O_T105','A_SAND_M50','B_SLOPE_DEGREE','B_SLOPE_ASPECT','B_ALTITUDE')
  cols <- cols[!cols %in% colnames(dt)]
  dt[,c(cols) := NA_real_]
  
  # calculate BD with different PTF's
  dt2 <- ptf_bd_all(dt)
  
  # select all pedotransfer functions
  # note YF: replicate PTF: the higher N and/or R2 is, more often the PTF is repeated (so that it has more weight in the LM)
  nsampledb <- nboot * 1000
  bd_all <- ptf.mods[,.(ptf_id,nsample,r2)]
  bd_all[is.na(nsample), nsample := 50]
  bd_all[is.na(r2), r2 := 0.7]
  cf <- nsampledb / (sum(round(bd_all$nsample * bd_all$r2)) * nrow(dt))
  bd_all[,nrep := pmax(1,round(nsample * bd_all$r2 * cf))]
  bd_all <- bd_all[rep(bd_all[,.I],nrep)]
  bd_all <- bd_all[,.(ptf_id,nsample,r2)]
  
  # make all options
  cj = CJ(1:nrow(dt),1:nrow(bd_all))
  dt <- cbind(dt[cj[[1]],],bd_all[cj[[2]],])
  
  # merge BD values
  dt <- merge(dt, dt2[, .(id, ptf_id, value)], by = c("id", "ptf_id"), all.x = T)
  
  
  # update ptf_id
  dt[,ptf_id := as.integer(gsub('p','',ptf_id))]
  
  # select only relevant cases
  dt <- dt[!is.na(value) & value > 300 & value < 3000]
  
  # estimate 1-r2 = unexplained variance
  dt[,error := sqrt((1-r2) * value)]
  dt[,value := value + rnorm(.N,mean = 0, sd = error),by='ptf_id']
  
  # make a linear model for soil density
  m1 <- lm(value~A_C_OF + I(A_C_OF^2),data=dt)
  
  out <- m1
  
  return(out)
}

#' Predict the Water Holding Capacity with existing ptfs from literature.
#'
#' @param dt (data.table) Data table which includes
#' A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' A_CLAY_MI (numeric) The clay content of the soil (\%).
#' A_SAND_MI (numeric) The sand content of the soil (\%).
#' A_SILT_MI (numeric) The silt content of the soil (\%).
#' D_BDS (numeric) Soil bulk density (kg/m3)
#' A_DEPTH (numeric) The depth of the sampled soil layer (m)
#' topsoil (boolean) Whether top soil (1) or not (0)
#' 
#' 
#' @details 
#' This function returns a melted form of data table, containing values of predicted water holding capacity with different PTFs
#' 
#' @import data.table
#' 
#' @export
ptf_whc_all <- function(dt){
  
  # add visual binding
  p1 = p2 = p3 = p4 = p5 = p6 = p7 = p8 = p9 = p10 = p11 = p12 = p13 = p14 = NULL
  p15 = p16 = A_CLAY_MI = A_SAND_MI = A_SILT_MI = A_SOM_LOI = A_C_OF = A_H20_T105 = NULL 
  A_DEPTH = B_ALTITUDE = B_SLOPE_DEGREE = B_SLOPE_ASPECT = A_PH_WA = A_CACO3_IF = NULL
  A_N_RT = A_SAND_M50 = B_SOILTYPE_AGR = D_BDS = topsoil = NULL
  patterns = ptf_id = country_code = continent_code = . = ptf_id = nsample = r2 = NULL
  nrep = value = error = patterns = num_obs = NULL
  
  # make local copy
  dt <- copy(dt)
  
  # add all possible inputs as NA when missing
  cols <- c('A_SOM_LOI', 'A_C_OF', 'A_CLAY_MI','A_SAND_MI','A_SILT_MI', 'D_BDS',
            'A_DEPTH', 'topsoil')
  cols <- cols[!cols %in% colnames(dt)]
  dt[,c(cols) := NA_real_]
  
  # estimate missing variables for texture being dependent on each other
  dt[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),.SDcols = c('A_CLAY_MI','A_SAND_MI','A_SILT_MI')]
  dt[num_obs == 2 & is.na(A_CLAY_MI), A_CLAY_MI := 100 - A_SAND_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SAND_MI), A_SAND_MI := 100 - A_CLAY_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SILT_MI), A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
  
  # estimate missing SOM variables
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 1.724]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 / 1.724]
  
  # insert default value for topsoil and A_DEPTH, when missing
  dt[is.na(A_DEPTH), A_DEPTH := 0.3]
  dt[is.na(topsoil), topsoil := 1]
  
  
  # estimate the WHC by the pedotransfer functions
  mp_wp = 1500
  mp_fc = 33
  #mp_fc = 10
  
  dt[, p1 := sptf_whc1(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p2 := sptf_whc2(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI)] 
  dt[, p3 := sptf_whc3(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI, mp_wp = mp_wp, mp_fc = mp_fc)] 
  dt[, p4 := sptf_whc4(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI, D_BDS = D_BDS, A_DEPTH = 30, mp_wp = mp_wp)] 
  dt[, p5 := sptf_whc5(A_SILT_MI = A_SILT_MI, A_CLAY_MI = A_CLAY_MI, D_BDS = D_BDS, A_SOM_LOI = A_SOM_LOI, 
                       topsoil = 1, mp_wp = mp_wp, mp_fc = mp_fc)] 
  dt[, p6 := sptf_whc6(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI, D_BDS = D_BDS, A_C_OF = A_C_OF,
                       mp_wp = mp_wp, mp_fc = mp_fc)] 
  # PTF7: This PTF uses the package 'euptf2'. The function 'euptf2::euptfFun' does not work when the data frame has only 1 row ?!
  dt[, p7 := sptf_whc7(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI,
                       D_BDS = D_BDS, A_C_OF = A_C_OF, A_DEPTH = 30, mp_wp = mp_wp, mp_fc = mp_fc)] 
  dt[, p8 := sptf_whc8(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI,  D_BDS = D_BDS, A_C_OF = A_C_OF,
                       mp_wp = mp_wp, mp_fc = mp_fc)] 
  # PTF9: field capacity (mp_fc) should be either 33 or 10 kPa.
  dt[, p9 := sptf_whc9(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_C_OF = A_C_OF,  mp_fc = mp_fc)] 
  # PTF10: field capacity (mp_fc) should be either 33 or 10 kPa.
  dt[, p10 := sptf_whc10(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_SAND_MI = A_SAND_MI, A_C_OF = A_C_OF,
                         D_BDS = D_BDS, mp_fc = mp_fc)]
  # # PTF11: Calculation of wrc parameters may be wrong. The calculated water content is out of normal range.
  # dt[, p11 := sptf_whc11(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, D_BDS = D_BDS,
  #                        mp_wp = mp_wp, mp_fc = mp_fc)] 
  # # PTF12: Calculation of wrc parameters may be wrong. The calculated water content is out of normal range.
  # dt[, p12 := sptf_whc12(A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, D_BDS = D_BDS,
  #                        mp_wp = mp_wp, mp_fc = mp_fc)]
  dt[, p13 := sptf_whc13(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI, D_BDS = D_BDS, A_C_OF = A_C_OF, 
                         mp_wp = mp_wp, mp_fc = mp_fc)]
  dt[, p14 := sptf_whc14(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_SOM_LOI = A_SOM_LOI, 
                         A_SAND_M50 = 150, topsoil = 1, mp_wp = mp_wp, mp_fc = mp_fc)] 
  dt[, p15 := sptf_whc15(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_SOM_LOI = A_SOM_LOI, 
                         A_SAND_M50 = 150, topsoil = 1, mp_wp = mp_wp, mp_fc = mp_fc)] 
  # # PTF16: table (soilptf::sptf_bouwsteen) is not properly loaded within the function. To be fixed.
  dt[, p16 := sptf_whc16(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_SOM_LOI = A_SOM_LOI,
                         A_SAND_M50 = 150, mp_wp = mp_wp, mp_fc = mp_fc)]

  
  
  # melt the data
  dt2 <- melt(dt, 
              id.vars = c('id','A_SOM_LOI', "A_C_OF", "A_CLAY_MI", "A_SAND_MI", "A_SILT_MI", "D_BDS", "A_DEPTH"),
              measure = patterns('^p'),
              variable.name = 'ptf_id')
  dt2[,ptf_id := as.integer(gsub('p','',ptf_id))]

  
  return(dt2)

}

#' Predict the Cation Exchange Capacity with existing ptfs from literature.
#'
#' @param dt (data.table) Data table which includes
#' A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' A_CLAY_MI (numeric) The clay content of the soil (\%).
#' A_SAND_MI (numeric) The sand content of the soil (\%).
#' A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @details 
#' This function returns a melted form of data table, containing values of predicted CEC with different PTFs
#' 
#' @import data.table
#' 
#' @export
ptf_cec_all <- function(dt){
  
  # add visual binding

  # make local copy
  dt <- copy(dt)
  
  # add all possible inputs as NA when missing
  cols <- c('A_SOM_LOI', 'A_C_OF', 'A_CLAY_MI','A_SAND_MI','A_SILT_MI', 'D_BDS')
  cols <- cols[!cols %in% colnames(dt)]
  dt[,c(cols) := NA_real_]
  
  # estimate missing variables for texture being dependent on each other
  dt[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),.SDcols = c('A_CLAY_MI','A_SAND_MI','A_SILT_MI')]
  dt[num_obs == 2 & is.na(A_CLAY_MI), A_CLAY_MI := 100 - A_SAND_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SAND_MI), A_SAND_MI := 100 - A_CLAY_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SILT_MI), A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
  
  # estimate missing SOM variables
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 1.724]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 / 1.724]
  
  # if bulk density is missing, estimate this from A_C_OF
  # dt[is.na(D_BDS), D_BDS := 1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF]
  
  # estimate CEC (mmol+/kg)
  dt[, p1 := sptf_cec1(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p2 := sptf_cec2(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_PH_KCL = A_PH_KCL)]
  dt[, p3 := sptf_cec3(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_PH_CC = A_PH_CC)]
  dt[, p4 := sptf_cec4(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_PH_CC = A_PH_CC)]
  dt[, p5 := sptf_cec5(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p6 := sptf_cec6(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_PH_CC = A_PH_CC)]
  dt[, p7 := sptf_cec7(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,B_LU_PTFCLASS = B_LU_PTFCLASS,A_PH_CC = A_PH_CC)]
  dt[, p8 := sptf_cec8(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_CN_FR = A_CN_FR,B_LU_PTFCLASS = B_LU_PTFCLASS,A_PH_CC = A_PH_CC)]
  dt[, p9 := sptf_cec9(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,B_LU_PTFCLASS = B_LU_PTFCLASS,A_PH_CC = A_PH_CC)]
  dt[, p10 := sptf_cec10(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,B_LU_PTFCLASS = B_LU_PTFCLASS,A_PH_CC = A_PH_CC)]
  dt[, p11 := sptf_cec11(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,B_LU_PTFCLASS = B_LU_PTFCLASS,A_PH_CC = A_PH_CC)]
  dt[, p12 := sptf_cec12(A_C_OF = A_C_OF, B_LU_PTFCLASS = B_LU_PTFCLASS)]
  dt[, p13 := sptf_cec13(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p14 := sptf_cec14(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p15 := sptf_cec15(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p16 := sptf_cec16(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA)]
  dt[, p17 := sptf_cec17(A_SOM_LOI = A_SOM_LOI, B_LU_PTFCLASS = B_LU_PTFCLASS)]
  dt[, p18 := sptf_cec18(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p19 := sptf_cec19(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p20 := sptf_cec20(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA)]
  dt[, p21 := sptf_cec21(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI)]
  dt[, p22 := sptf_cec22(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI)]
  dt[, p23 := sptf_cec23(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_PH_CC = A_PH_CC)]
  dt[, p24 := sptf_cec24(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_PH_CC = A_PH_CC)]
  dt[, p25 := sptf_cec25(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_PH_CC = A_PH_CC)]
  dt[, p26 := sptf_cec26(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_PH_WA = A_PH_WA,B_SOILCLASS_USDA=NA_character_)]
  dt[, p27 := sptf_cec27(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI,B_CLIM_CAT1=B_CLIM_CAT1)]
  dt[, p28 := sptf_cec28(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_PH_CC = A_PH_CC)]
  dt[, p29 := sptf_cec29(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_CACO3_MI=A_CACO3_MI,A_PH_CC = A_PH_CC)]
  dt[, p30 := sptf_cec30(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p31 := sptf_cec31(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,B_SOILCLASS_USDA=NA_character_)]
  dt[, p32 := sptf_cec32(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_PH_CC = A_PH_CC)]
  dt[, p33 := sptf_cec33(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_PH_CC = A_PH_CC)]
  dt[, p34 := sptf_cec34(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_PH_CC = A_PH_CC,B_LU_PTFCLASS = B_LU_PTFCLASS)]
  dt[, p35 := sptf_cec35(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_PH_CC = A_PH_CC)]
  dt[, p36 := sptf_cec36(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p37 := sptf_cec37(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p38 := sptf_cec38(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p39 := sptf_cec39(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p40 := sptf_cec40(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p41 := sptf_cec41(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p42 := sptf_cec42(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI)]
  dt[, p43 := sptf_cec43(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_PH_CC = A_PH_CC)]
  dt[, p44 := sptf_cec44(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_PH_CC = A_PH_CC)]
  dt[, p45 := sptf_cec45(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA)]
  dt[, p46 := sptf_cec46(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p47 := sptf_cec47(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,B_LU_PTFCLASS = B_LU_PTFCLASS)]
  dt[, p48 := sptf_cec48(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p49 := sptf_cec49(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI)]
  dt[, p50 := sptf_cec50(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_CACO3_MI=A_CACO3_MI,A_PH_CC = A_PH_CC)]
  dt[, p51 := sptf_cec51(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA)]
  dt[, p52 := sptf_cec52(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA)]
  dt[, p53 := sptf_cec53(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA)]
  dt[, p54 := sptf_cec54(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA,B_LU_PTFCLASS=B_LU_PTFCLASS)]
  dt[, p55 := sptf_cec55(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA)]
  dt[, p56 := sptf_cec56(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA)]
  dt[, p57 := sptf_cec57(A_SOM_LOI = A_SOM_LOI, A_SAND_MI = A_SAND_MI, A_PH_WA = A_PH_WA)]
  dt[, p58 := sptf_cec58(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_CACO3_MI=A_CACO3_MI)]
  dt[, p59 := sptf_cec59(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p60 := sptf_cec60(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p61 := sptf_cec61(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p62 := sptf_cec62(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_CACO3_MI=A_CACO3_MI)]
  dt[, p63 := sptf_cec63(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p64 := sptf_cec64(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p65 := sptf_cec65(A_C_OF = A_C_OF)]
  dt[, p66 := sptf_cec66(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p67 := sptf_cec67(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p68 := sptf_cec68(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p69 := sptf_cec69(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p70 := sptf_cec70(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p71 := sptf_cec71(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p72 := sptf_cec72(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA)]
  dt[, p73 := sptf_cec73(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA)]
  dt[, p74 := sptf_cec74(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA,B_SOILCLASS_USDA = NA_character_)]
  
  # melt the data
  dt2 <- melt(dt, 
              id.vars = c('id','A_SOM_LOI', "A_C_OF", "A_CLAY_MI", "A_SAND_MI", "A_SILT_MI", "D_BDS", "A_DEPTH"),
              measure = patterns('^p'),
              variable.name = 'ptf_id',
              value.name = 'cec')
  dt2[,ptf_id := as.integer(gsub('p','',ptf_id))]
  
  # return value
  return(dt2)
  
}

#' Predict the pH buffer Capacity with existing ptfs from literature.
#'
#' @param dt (data.table) Data table which includes
#' A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' A_CLAY_MI (numeric) The clay content of the soil (\%).
#' A_SAND_MI (numeric) The sand content of the soil (\%).
#' A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @details 
#' This function returns a melted form of data table, containing values of predicted pH buffer capacity with different PTFs
#' 
#' @import data.table
#' 
#' @export
ptf_phbc_all <- function(dt){
  
  # add visual binding
  
  # make local copy
  dt <- copy(dt)
  
  # add all possible inputs as NA when missing
  cols <- c('A_SOM_LOI', 'A_C_OF', 'A_CLAY_MI','A_SAND_MI','A_SILT_MI')
  cols <- cols[!cols %in% colnames(dt)]
  dt[,c(cols) := NA_real_]
  
  # estimate missing variables for texture being dependent on each other
  dt[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),.SDcols = c('A_CLAY_MI','A_SAND_MI','A_SILT_MI')]
  dt[num_obs == 2 & is.na(A_CLAY_MI), A_CLAY_MI := 100 - A_SAND_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SAND_MI), A_SAND_MI := 100 - A_CLAY_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SILT_MI), A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
  
  # estimate missing SOM variables
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 1.724]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 / 1.724]
  
  # if bulk density is missing, estimate this from A_C_OF
  # dt[is.na(D_BDS), D_BDS := 1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF]
  
  # estimate the pH buffer capacity
  dt[, p1 := sptf_phbc1(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p2 := sptf_phbc2(A_C_OF = A_C_OF)]
  dt[, p3 := sptf_phbc3(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p4 := sptf_phbc4(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p5 := sptf_phbc5(A_C_OF = A_C_OF)]
  dt[, p6 := sptf_phbc6(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p7 := sptf_phbc7(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_PH_WA=A_PH_WA)]
  dt[, p8 := sptf_phbc8(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  
  # melt the data
  dt2 <- melt(dt, 
              id.vars = 'id',
              measure = patterns('^p'),
              variable.name = 'ptf_id',
              value.name = 'phbc')
  dt2[,ptf_id := as.integer(gsub('p','',ptf_id))]
  
  # return value
  return(dt2)
  
}

#' Predict the Mean Weight Diameter (mwd) of soil aggregates with existing ptfs from literature.
#'
#' @param dt (data.table) Data table which includes
#' A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' A_CLAY_MI (numeric) The clay content of the soil (\%).
#' A_SAND_MI (numeric) The sand content of the soil (\%).
#' A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @details 
#' This function returns a melted form of data table, containing values of predicted Mean Weight Diameter (mwd) of soil aggregates with different PTFs
#' 
#' @import data.table
#' 
#' @export
ptf_mwd_all <- function(dt){
  
  # add visual binding
  
  # make local copy
  dt <- copy(dt)
  
  # add all possible inputs as NA when missing
  cols <- c('A_SOM_LOI', 'A_C_OF', 'A_CLAY_MI','A_SAND_MI','A_SILT_MI')
  cols <- cols[!cols %in% colnames(dt)]
  dt[,c(cols) := NA_real_]
  
  # estimate missing variables for texture being dependent on each other
  dt[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),.SDcols = c('A_CLAY_MI','A_SAND_MI','A_SILT_MI')]
  dt[num_obs == 2 & is.na(A_CLAY_MI), A_CLAY_MI := 100 - A_SAND_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SAND_MI), A_SAND_MI := 100 - A_CLAY_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SILT_MI), A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
  
  # estimate missing SOM variables
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 1.724]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 / 1.724]
  
  # if bulk density is missing, estimate this from A_C_OF
  # dt[is.na(D_BDS), D_BDS := 1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF]
  
  # estimate the mean weight diameter of Soil Aggregates
  dt[, p1 := sptf_mwd1(A_SOM_LOI = A_SOM_LOI)]
  dt[, p2 := sptf_mwd2(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI)]
  dt[, p3 := sptf_mwd3(A_C_OF = A_C_OF, A_CEC_CO=A_CEC_CO,A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,
                        A_PH_WA = A_PH_WA, A_CACO3_MI = A_CACO3_MI)]
  dt[, p4 := sptf_mwd4(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_CACO3_MI = A_CACO3_MI)]
  dt[, p5 := sptf_mwd5(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_PH_WA = A_PH_WA)]
  dt[, p6 := sptf_mwd6(A_C_OF = A_C_OF)]
  dt[, p7 := sptf_mwd7(A_C_OF = A_C_OF,A_SAND_MI=A_SAND_MI)]
  dt[, p8 := sptf_mwd8(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_PH_WA = A_PH_WA, A_CACO3_MI = A_CACO3_MI)]
  dt[, p9 := sptf_mwd9(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_PH_WA = A_PH_WA,B_LU_PTFCLASS=B_LU_PTFCLASS)]
  dt[, p10 := sptf_mwd10(A_C_OF = A_C_OF)]
  dt[, p11 := sptf_mwd11(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p12 := sptf_mwd12(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI)]
  dt[, p13 := sptf_mwd13(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p14 := sptf_mwd14(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_PH_WA = A_PH_WA, A_CACO3_MI = A_CACO3_MI)]
  dt[, p15 := sptf_mwd15(A_C_OF = A_C_OF, A_PH_WA = A_PH_WA)]
  
  # melt the data
  dt2 <- melt(dt, 
              id.vars = 'id',
              measure = patterns('^p'),
              variable.name = 'ptf_id',
              value.name = 'mwd')
  dt2[,ptf_id := as.integer(gsub('p','',ptf_id))]
  
  # return value
  return(dt2)
  
}

#' Predict the percentage Water Stable Aggregates (wsa) with existing ptfs from literature.
#'
#' @param dt (data.table) Data table which includes
#' A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' A_CLAY_MI (numeric) The clay content of the soil (\%).
#' A_SAND_MI (numeric) The sand content of the soil (\%).
#' A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @details 
#' This function returns a melted form of data table, containing values of predicted percentage Water Stable Aggregates with different PTFs
#' 
#' @import data.table
#' 
#' @export
ptf_wsa_all <- function(dt){
  
  # add visual binding
  
  # make local copy
  dt <- copy(dt)
  
  # add all possible inputs as NA when missing
  cols <- c('A_SOM_LOI', 'A_C_OF', 'A_CLAY_MI','A_SAND_MI','A_SILT_MI')
  cols <- cols[!cols %in% colnames(dt)]
  dt[,c(cols) := NA_real_]
  
  # estimate missing variables for texture being dependent on each other
  dt[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),.SDcols = c('A_CLAY_MI','A_SAND_MI','A_SILT_MI')]
  dt[num_obs == 2 & is.na(A_CLAY_MI), A_CLAY_MI := 100 - A_SAND_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SAND_MI), A_SAND_MI := 100 - A_CLAY_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SILT_MI), A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
  
  # estimate missing SOM variables
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 1.724]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 / 1.724]
  
  # if bulk density is missing, estimate this from A_C_OF
  # dt[is.na(D_BDS), D_BDS := 1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF]
  
  # estimate the percentage Water Stable Aggregates (%)
  dt[, p1 := sptf_wsa1(A_C_OF = A_C_OF)]
  dt[, p2 := sptf_wsa2(A_SOM_LOI = A_SOM_LOI)]
  dt[, p3 := sptf_wsa3(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,
                       A_K_AA = A_K_AA, A_PH_WA = A_PH_WA)]
  dt[, p4 := sptf_wsa4(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI)]
  dt[, p5 := sptf_wsa5(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI)]
  dt[, p6 := sptf_wsa6(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI, A_CACO3_MI=A_CACO3_MI)]
  dt[, p7 := sptf_wsa7(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI, 
                      A_PH_WA = A_PH_WA,A_CACO3_MI = A_CACO3_MI)]
  dt[, p8 := sptf_wsa8(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p9 := sptf_wsa9(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA)]  
  
  # melt the data
  dt2 <- melt(dt, 
              id.vars = 'id',
              measure = patterns('^p'),
              variable.name = 'ptf_id',
              value.name = 'wsa')
  dt2[,ptf_id := as.integer(gsub('p','',ptf_id))]
  
  # return value
  return(dt2)
  
}

#' Predict the hot water extractable carbon level (mg/kg) from soil properties via existing ptfs from literature.
#'
#' @param dt (data.table) Data table which includes
#' A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' A_CLAY_MI (numeric) The clay content of the soil (\%).
#' A_SAND_MI (numeric) The sand content of the soil (\%).
#' A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @details 
#' This function returns a melted form of data table, containing values of predicted HWC with different PTFs
#' 
#' @import data.table
#' 
#' @export
ptf_hwc_all <- function(dt){
  
  # add visual binding
  
  # make local copy
  dt <- copy(dt)
  
  # add all possible inputs as NA when missing
  cols <- c('A_SOM_LOI', 'A_C_OF', 'A_CLAY_MI','A_SAND_MI','A_SILT_MI')
  cols <- cols[!cols %in% colnames(dt)]
  dt[,c(cols) := NA_real_]
  
  # estimate missing variables for texture being dependent on each other
  dt[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),.SDcols = c('A_CLAY_MI','A_SAND_MI','A_SILT_MI')]
  dt[num_obs == 2 & is.na(A_CLAY_MI), A_CLAY_MI := 100 - A_SAND_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SAND_MI), A_SAND_MI := 100 - A_CLAY_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SILT_MI), A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
  
  # estimate missing SOM variables
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 1.724]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 / 1.724]
  
  # if bulk density is missing, estimate this from A_C_OF
  # dt[is.na(D_BDS), D_BDS := 1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF]
  
  # estimate the percentage hot water carbon (mg/kg)
  dt[, p1 := sptf_hwc1(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p2 := sptf_hwc2(A_C_OF = A_C_OF)]
  dt[, p3 := sptf_hwc3(A_C_OF = A_C_OF, A_PH_CC = A_PH_CC)]
  dt[, p4 := sptf_hwc4(A_C_OF = A_C_OF)]
  dt[, p5 := sptf_hwc5(A_C_OF = A_C_OF, A_P_AL = A_P_AL, A_PH_CC = A_PH_CC)]
  dt[, p6 := sptf_hwc6(A_C_OF = A_C_OF,  A_CLAY_MI = A_CLAY_MI,A_P_AL = A_P_AL)]
  dt[, p7 := sptf_hwc7(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  dt[, p8 := sptf_hwc8(A_C_OF = A_C_OF)]
  dt[, p9 := sptf_hwc9(A_C_OF = A_C_OF, A_PH_CC = A_PH_CC)]
  dt[, p10 := sptf_hwc10(A_C_OF = A_C_OF, A_PH_CC = A_PH_CC)]
  dt[, p11 := sptf_hwc11(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  
  # melt the data
  dt2 <- melt(dt, 
              id.vars = 'id',
              measure = patterns('^p'),
              variable.name = 'ptf_id',
              value.name = 'hwc')
  dt2[,ptf_id := as.integer(gsub('p','',ptf_id))]
  
  # return value
  return(dt2)
  
}

#' Predict the soil shear strength from soil properties via existing ptfs from literature.
#'
#' @param dt (data.table) Data table which includes
#' A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' A_CLAY_MI (numeric) The clay content of the soil (\%).
#' A_SAND_MI (numeric) The sand content of the soil (\%).
#' A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @details 
#' This function returns a melted form of data table, containing values of predicted soil shear strength with different PTFs
#' 
#' @import data.table
#' 
#' @export
ptf_sss_all <- function(dt){
  
  # add visual binding
  
  # make local copy
  dt <- copy(dt)
  
  # add all possible inputs as NA when missing
  cols <- c('A_SOM_LOI', 'A_C_OF', 'A_CLAY_MI','A_SAND_MI','A_SILT_MI','A_CACO3_MI')
  cols <- cols[!cols %in% colnames(dt)]
  dt[,c(cols) := NA_real_]
  
  # estimate missing variables for texture being dependent on each other
  dt[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),.SDcols = c('A_CLAY_MI','A_SAND_MI','A_SILT_MI')]
  dt[num_obs == 2 & is.na(A_CLAY_MI), A_CLAY_MI := 100 - A_SAND_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SAND_MI), A_SAND_MI := 100 - A_CLAY_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SILT_MI), A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
  
  # estimate missing SOM variables
  dt[is.na(A_SOM_LOI) & !is.na(A_C_OF), A_SOM_LOI := A_C_OF * 0.1 * 1.724]
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 / 1.724]
  
  # if bulk density is missing, estimate this from A_C_OF
  # dt[is.na(D_BDS), D_BDS := 1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF]
  
  # estimate the soil shear strength
  dt[, p1 := sptf_sss1(A_SOM_LOI = A_SOM_LOI, A_CACO3_MI = A_CACO3_MI)]
  dt[, p2 := sptf_sss2(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  dt[, p3 := sptf_sss3(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]
  
  # melt the data
  dt2 <- melt(dt, 
              id.vars = 'id',
              measure = patterns('^p'),
              variable.name = 'ptf_id',
              value.name = 'hwc')
  dt2[,ptf_id := as.integer(gsub('p','',ptf_id))]
  
  # return value
  return(dt2)
  
}

#' Predict the Potentially Mineralizable Nitrogen with existing ptfs from literature.
#'
#' @param dt (data.table) Data table which includes
#' A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' A_CLAY_MI (numeric) The clay content of the soil (\%).
#' A_SAND_MI (numeric) The sand content of the soil (\%).
#' A_SILT_MI (numeric) The silt content of the soil (\%).
#' A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' A_PH_CC (numeric) The pH-CaCl2 of the soil
#' A_CEC_CO (numeric) The cation exchange capacity (mmol+ / kg) 
#'  
#' @details 
#' This function returns a melted form of data table, containing values of predicted PMN with different PTFs
#' 
#' @import data.table
#' 
#' @export
ptf_pmn_all <- function(dt){
  
  # add visual bindings
  A_C_OF = A_CLAY_MI = A_SAND_MI = A_SILT_MI = A_N_RT = A_PH_CC = A_CEC_CO = NULL
  num_obs = A_SOM_LOI = p1_p = p1 = p2 = p3 = p4 = p5 = p6 = p7 = p8 = p9 = p10 = NULL
  patterns = ptf_id = NULL
  
  # make local copy
  dt <- copy(dt)
  
  # add all possible inputs as NA when missing
  cols <- c('A_C_OF', 'A_CLAY_MI','A_SAND_MI','A_SILT_MI',
            'A_N_RT', 'A_PH_CC', 'A_CEC_CO')
  cols <- cols[!cols %in% colnames(dt)]
  dt[,c(cols) := NA_real_]
  
  # estimate missing variables for texture being dependent on each other
  dt[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x))),.SDcols = c('A_CLAY_MI','A_SAND_MI','A_SILT_MI')]
  dt[num_obs == 2 & is.na(A_CLAY_MI), A_CLAY_MI := 100 - A_SAND_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SAND_MI), A_SAND_MI := 100 - A_CLAY_MI - A_SILT_MI]
  dt[num_obs == 2 & is.na(A_SILT_MI), A_SILT_MI := 100 - A_CLAY_MI - A_SAND_MI]
  
  # estimate missing SOM variables
  dt[!is.na(A_SOM_LOI) & is.na(A_C_OF), A_C_OF := A_SOM_LOI * 10 / 1.724]
  
  
  # Calculate PMN (mg/kg), 30 dC for 14 days
  dt[, p1_p := sptf_pmn1(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_N_RT = A_N_RT, A_PH_CC = A_PH_CC)]
  dt[, p1 := correct_incubationperiod(p1_p, 14)] # convert Nmin to 7 days 
  dt[, p1_p := NULL]
  # Calculate Nmin (mg/kg) for t days at 35 dC
  dt[, p2 := sptf_pmn2(A_C_OF = A_C_OF, A_SILT_MI = A_SILT_MI, A_N_RT = A_N_RT, A_PH_CC = A_PH_CC, t = 7)] 
  # Calculate Nmin (mg/kg) for 7 days at 40 dC
  dt[, p3 := sptf_pmn3(A_N_RT = A_N_RT, A_CLAY_MI = A_CLAY_MI)] 
  # Calculate Nmin (mg/kg) for 7 days at 40 dC
  dt[, p4 := sptf_pmn4(A_N_RT = A_N_RT, A_CLAY_MI = A_CLAY_MI)] 
  # Calculate Nmin (mg/kg) based on 1-pool model for t days
  dt[, p5 := sptf_pmn5(A_N_RT = A_N_RT, A_C_OF = A_C_OF, A_CEC_CO = A_CEC_CO, t = 7)] 
  # Calculate Nmin (mg/kg) based on 2-pool model for t days
  dt[, p6 := sptf_pmn6(A_N_RT = A_N_RT, A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, 
                       A_SILT_MI = A_SILT_MI, A_PH_CC = A_PH_CC, t = 7, RES = 1)] 
  # Calculate Nmin (mg/kg) based on 1-pool model for t days
  dt[, p7 := sptf_pmn7(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, t = 7)] 
  # Calculate Nmin (mg/kg) based on 1-pool model for t days
  dt[, p8 := sptf_pmn8(A_N_RT = A_N_RT, A_CEC_CO = A_CEC_CO, A_SILT_MI = A_SILT_MI, t = 7)] 
  # Calculate Nmin (mg/kg) based on 1-pool model for t days
  dt[, p9 := sptf_pmn9(A_N_RT = A_N_RT,  CULT = 1, t = 7)] 
  # Calculate Nmin (mg/kg) based on 1-pool model for t days
  dt[, p10 := sptf_pmn10(A_N_RT = A_N_RT,  t = 7)] 
  
  # extend with empirical functions derived from Dutch datasets
  dt[, p11 := sptf_pmn11(A_C_OF = A_C_OF,  A_CLAY_MI = A_CLAY_MI)] 
  dt[, p12 := sptf_pmn12(A_C_OF = A_C_OF)]
  dt[, p13 := sptf_pmn13(A_C_OF = A_C_OF)]
  dt[, p14 := sptf_pmn14(A_C_OF = A_C_OF, A_P_AL = A_P_AL, A_PH_CC = A_PH_CC)]
  dt[, p15 := sptf_pmn15(A_C_OF = A_C_OF,  A_CLAY_MI = A_CLAY_MI,A_P_AL = A_P_AL)]
  dt[, p16 := sptf_pmn16(A_C_OF = A_C_OF)]
  dt[, p17 := sptf_pmn17(A_C_OF = A_C_OF, A_PH_CC = A_PH_CC)]
  dt[, p18 := sptf_pmn18(A_C_OF = A_C_OF, A_PH_CC = A_PH_CC)]
  dt[, p19 := sptf_pmn19(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI)]
  
  # melt the data
  dt2 <- melt(dt, 
              id.vars = 'id',
              measure = patterns('^p'),
              variable.name = 'ptf_id',
              value.name = 'pmn')
  dt2[,ptf_id := as.integer(gsub('p','',ptf_id))]
  
  
  return(dt2)
  
}

