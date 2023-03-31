#' Estimate soil texture class B_TEXTURE_USDA 
#' 
#' @inheritParams sptf_bd0
#' 
#' @return Texture according to the USDA classification
#' 
#' @export 
sptf_textureclass <- function(A_CLAY_MI, A_SILT_MI, A_SAND_MI){
  
  # check inputs
  arg.length <- max(length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI))
  check_numeric('A_CLAY_MI', A_CLAY_MI, FALSE, arg.length)
  check_numeric('A_SAND_MI', A_SAND_MI, FALSE, arg.length)
  check_numeric('A_SILT_MI', A_SILT_MI, FALSE, arg.length)
  checkmate::assert_true(sum(A_CLAY_MI, A_SAND_MI, A_SILT_MI) <= 100)
  
  # make internal table with shorter names
  dt <- data.table(cl = A_CLAY_MI,
                   sa = A_SAND_MI,
                   si = A_SILT_MI,
                   value = NA_character_)
  
  # find USDA classification
  dt[cl>40 & sa <=45 & si<=40, value := 'clay']
  dt[cl>40 & sa <=20 & si<=60 & si>40, value := 'silty clay']
  dt[cl>35 & cl <=55 & sa<=65 & sa>45 & si<20, value := 'sandy clay']
  dt[cl>20 & cl <=35 & sa>45 & sa<80 & si <=27.5,value := 'sandy clay loam']
  dt[cl>27.5 & cl<=40 & sa>20 & sa<=45 & si>15 & si<=52.5, value := 'clay loam']
  dt[cl>27.5 & cl<=40 & sa<=20 & si>40 & si<=72.5, value := 'silty clay loam']
  dt[cl<=27.5& sa<=50 & si>50 & si<=80, value := 'silty loam']
  dt[cl<=20 & cl>12.5 & sa<=7.5 & si>80 & si<=87.5, value := 'silty loam']
  dt[cl<=12.5& sa <=20 & si>80, value := 'silt']
  dt[cl<=27.5& cl>7.5 & sa <=52.5 & sa>22.5& si<=50 & si>27.5, value := 'loam']
  dt[cl<=7.5 & sa<=52.5 & sa>42.5 & si>40 & si<=50, value := 'sandy loam']
  dt[cl<=20 & sa>52.5 & sa<=70 & si>10 & si<=47.5,value := 'sandy loam']
  dt[cl>10 & cl<=20 & sa<=80 & sa>70 & si<=20, value := 'sandy loam']
  dt[cl<=10 & si<=15 & sa>85 & cl<=10-si*10/15, value := 'sand']
  dt[cl<=15 & si<=30 & sa>70 & cl> 10-si*10/15 & cl<=15-si*15/30, value := 'loamy sand']
  dt[cl<=20 & si<=50 & sa>70 & cl>15-si*15/30, value := 'sandy loam']
  dt[is.na(value), value :='sandy loam']
  
  # select value 
  value <- dt[,value]
  
  # return USDA soil classification
  return(value)
  
}