#' Estimate soil texture class B_TEXTURE_USDA 
#' 
#' @param A_CLAY_MI (numeric) Clay content  (\%)
#' @param A_SILT_MI (numeric) Silt content (\%)
#' @param A_SAND_MI (numeric) Silt content (\%)
#' 
#' @return Texture according to the USDA classification
#' 
#' @export 
sptf_textureclass <- function(A_CLAY_MI, A_SILT_MI, A_SAND_MI){
  
  # check inputs
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE)
  
  # set shorter object names
  cl <- A_CLAY_MI
  sa <- A_SAND_MI
  si <- A_SILT_MI
  
  # find USDA classification
  if (cl>40   & sa <=45  & si<=40) {
      value="clay"
    } else if (cl>40   & sa <=20  & si<=60    & si>40) {
      value="silty clay"
    } else if (cl>35   & cl <=55  & sa<=65    & sa>45  & si<20) {
      value="sandy clay"
    } else if (cl>20   & cl <=35  & sa>45     & sa<80  & si <=27.5) {
      value="sandy clay loam"
    } else if (cl>27.5 & cl<=40   & sa>20     & sa<=45 & si>15 & si<=52.5) {
      value="clay loam"
    } else if (cl>27.5 & cl<=40   & sa<=20    & si>40  & si<=72.5){
      value="silty clay loam"
    } else if (cl<=27.5& sa<=50   & si>50     & si<=80){
      value="Silty loam"
    } else if (cl<=20  & cl>12.5  & sa<=7.5   & si>80  & si<=87.5){
      value="silty loam"
    } else if (cl<=12.5& sa <=20  & si>80){
      value="silt"
    } else if (cl<=27.5& cl>7.5   & sa <=52.5 & sa>22.5& si<=50 & si>27.5){
      value="loam"
    } else if (cl<=7.5 & sa<=52.5 & sa>42.5   & si>40  & si<=50){
      value="sandy loam"
    } else if (cl<=20  & sa>52.5  & sa<=70    & si>10  & si<=47.5){
      value="sandy loam"
    } else if (cl>10   & cl<=20   & sa<=80    & sa>70  & si<=20){
      value="sandy loam"
    } else if (cl<=10  & si<=15   & sa>85     & cl<=10-si*10/15){
      value="sand"
    } else if (cl<=15  & si<=30   & sa>70     & cl> 10-si*10/15 & cl<=15-si*15/30){
      value="Loamy sand"
    } else if (cl<=20  & si<=50   & sa>70     & cl>15-si*15/30){
      value="sandy loam"
    } else {
      value="sandy loam"
    }
    
  # return USDA soil classification
  return(value)
  
}