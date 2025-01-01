# analysis for paper
require(data.table)

# for mineral soils only: 20% OS = 10% OC = 100 g/kg
dt  <- CJ(A_CLAY_MI = seq(0,50,5),
          A_SAND_MI = seq(0,100,5),
          A_C_OF = seq(0.5,100,1),
          A_PH_CC = seq(4,6,0.5))
dt <- dt[A_CLAY_MI + A_SAND_MI <= 100]
dt[,id := .I]
set.seed(124)
dt[,A_CN_FR := rnorm(.N,12,2)]
dt[,A_N_RT := A_C_OF * 1000 / A_CN_FR]

# ---- helper fun to extract coefficients from lm model for the label with model description -----------
hpf <- function(tpr,m1,r,cfin){
  
  vals <- coefficients(m1)
  pr <- paste(tpr," == ",round(vals[1],r[1]))
  if(length(vals)>1){
    for(i in 2:length(vals)){ pr <- paste(pr,
                                          fifelse(vals[i]<0,'-','+'),
                                          round(abs(vals[i]),r[i]),
                                          cfin[i-1])}
  } else {
    pr <- paste0(pr,cfin)
  }
  
  return(pr)}

# inverse helper function to derive the lowest SOC to achieve the target in soil health indicator
fmod <- function(model,var,a_clay_mi){
  
  dti <- data.table(A_CLAY_MI = a_clay_mi,var)
  dti <- unique(dti)
  nd <- CJ(A_C_OF = seq(0.1,100,0.01),A_CLAY_MI = unique(a_clay_mi))
  nd <- merge(dti,nd,by='A_CLAY_MI')
  nd[, p1 := predict(model,newdata = nd)]
  nd <- nd[is.finite(p1)]
  nd[,diff := abs(p1-var)]
  nd[,sel := fifelse(diff == min(diff),1,0),by='A_CLAY_MI']
  nd <- nd[sel==1]
  
  dti <- data.table(id = 1:length(a_clay_mi),A_CLAY_MI = a_clay_mi)
  dti <- merge(dti,nd[,.(A_CLAY_MI,A_C_OF)],by='A_CLAY_MI',allow.cartesian = TRUE,all.x=T)
  setorder(dti,id)
  
  return(dti[,A_C_OF])
}


# ---- derive functions ----

## --- bulk density ----

  # predict the cation exchange capacity
  dt1.bd <- ptf_bd_all(dt)
  dt1.bd <- dt1.bd[!is.na(bd)]
  dt1.bd <- dt1.bd[is.finite(bd) & bd > 0 & bd <= 3000]
  dt1.bd <- dt1.bd[,list(bd.mean = mean(bd,na.rm=T),
                         bd.se = sd(bd,na.rm=T)/sqrt(sum(!is.na(bd)))),by='id']
  dt1 <- merge(dt,dt1.bd,by='id')
  m.bd <- lm(bd.mean~A_C_OF*A_CLAY_MI + I(log(A_C_OF)),data=dt1)
  p.bd <- predict(m.bd,newdata = data.frame(A_C_OF = dt1$A_C_OF,A_CLAY_MI = dt1$A_CLAY_MI))
  l.bd <- hpf('BD',m.bd,r=c(0,3,2,2,4),cfin=c('* C','* CLAY','* log(C)','* C * CLAY'))
  
  # optimum density for rootability (Ros, 2023)
  dt1[, odens := (1.75 - 0.009 * A_CLAY_MI) * 1000]
  dt1[, cdens := fmod(model = m.bd,var = odens,A_CLAY_MI)]

  
## --- CEC ----
  
  # the cation exchange capacity
  dt[, A_CN_FR := pmax(6,A_CN_FR,na.rm=T)]
  dt1.cec <- ptf_cec_all(dt)
  dt1.cec <- dt1.cec[!is.na(cec)]
  dt1.cec <- dt1.cec[is.finite(cec) & cec > 0 & cec <= 1000]
  dt1.cec <- dt1.cec[,list(cec.mean = mean(cec,na.rm=T),
                           cec.se = sd(cec,na.rm=T)/sqrt(sum(!is.na(cec)))),by='id']
  dt1 <- merge(dt1,dt1.cec,by='id')
  m.cec <- lm(cec.mean~A_C_OF*A_CLAY_MI,data=dt1)
  p.cec <- predict(m.cec,newdata = data.frame(A_C_OF = dt1$A_C_OF,A_CLAY_MI = dt1$A_CLAY_MI))
  l.cec <- hpf('CEC',m.cec,r=c(0,2,2,4),cfin=c('* C','* CLAY','* C * CLAY'))
  
  # optimum CEC level for soil fertility (van Erp, 2001; Ros et al, 2023)
  dt1[, ocec := 100]
  dt1[, ccec := fmod(model = m.cec, var = 100,A_CLAY_MI)]

## --- WSA ----
  
  # water stable aggregates
  dt1.wsa <- ptf_wsa_all(dt)
  dt1.wsa <- dt1.wsa[!is.na(wsa)]
  dt1.wsa <- dt1.wsa[is.finite(wsa) & wsa > 0 & wsa <= 1000]
  dt1.wsa <- dt1.wsa[,list(wsa.mean = mean(wsa,na.rm=T),
                           wsa.se = sd(wsa,na.rm=T)/sqrt(sum(!is.na(wsa)))),by='id']
  dt1 <- merge(dt1,dt1.wsa,by='id')
  m.wsa <- lm(wsa.mean~A_C_OF*A_CLAY_MI+I(A_CLAY_MI^2),data=dt1)
  p.wsa <- predict(m.wsa,newdata = data.frame(A_C_OF = dt1$A_C_OF,A_CLAY_MI = dt1$A_CLAY_MI))
  l.wsa <- hpf('WSA',m.wsa,r=c(0,2,2,4,4),cfin=c('* C','* CLAY','* CLAY^2','* C * CLAY'))
  
  # optimum level water stable aggregates (from SHI, Moebius-Clune, 2017)
  # The larger the MWD and GMD values are, the higher the average particle size agglomeration of soil aggregates are, and the stronger the stability of soil structure is
  dt1[, owsa := 75]
  dt1[, cwsa := fmod(model = m.wsa, var = owsa,A_CLAY_MI)]
  
## ---- MWD ------
  
  # see hamel for interpretation optimum
  dt1.mwd <- ptf_mwd_all(dt)
  dt1.mwd <- dt1.mwd[!is.na(mwd)]
  dt1.mwd <- dt1.mwd[is.finite(mwd) & mwd > 0 & mwd <= 15]
  dt1.mwd <- dt1.mwd[,list(mwd.mean = mean(mwd,na.rm=T),
                           mwd.se = sd(mwd,na.rm = T)/sqrt(sum(!is.na(mwd)))),by='id']
  dt1 <- merge(dt1,dt1.mwd,by='id',all.x = TRUE)
  m.mwd <- lm(mwd.mean~A_C_OF+A_CLAY_MI,data=dt1)
  p.mwd <- predict(m.mwd,newdata = data.frame(A_C_OF = dt1$A_C_OF,A_CLAY_MI = dt1$A_CLAY_MI))
  l.mwd <- hpf('MWD',m.mwd,r=c(2,4,4),cfin=c('* C','* CLAY'))
  
  # optimum MWD value using method of Le Bissonnais (1996), cited Clergue et al. (2003)
  # lower values are unstable, and high risk on crusttability
  dt1[, omwd := 1.3]
  dt1[, cmwd := fmod(model = m.mwd, var = omwd,A_CLAY_MI)]
  
## --- WHC -----
  
  # derive water holding capacity
  dt1.whc <- ptf_whc_all(dt)
  dt1.whc <- dt1.whc[!is.na(whc)]
  dt1.whc <- dt1.whc[is.finite(whc) & whc > 0 & whc <= 1]
  dt1.whc <- dt1.whc[,list(whc.mean = mean(whc,na.rm=T),
                           whc.se = sd(whc,na.rm = T)/sqrt(sum(!is.na(whc)))),by='id']
  dt1 <- merge(dt1,dt1.whc,by='id')
  m.whc <- lm(whc.mean~I(A_C_OF^0.5)+A_CLAY_MI,data=dt1)
  p.whc <- predict(m.whc,newdata = data.frame(A_C_OF = dt1$A_C_OF,A_CLAY_MI = dt1$A_CLAY_MI))
  l.whc <- hpf('WHC',m.whc,r=c(4,4,5),cfin=c('* C^0.5','* CLAY'))
  
  # more is better, optimum score (medium to high boundery) at 0.3 g / g (Moebius-Clune, 2017)
  dt1[, owhc := 0.45]
  dt1[, cwhc := fmod(model = m.whc, var = owhc, A_CLAY_MI)]
  
## --- PAW -----
  
  # derive water plant available water
  dt1.paw <- ptf_paw_all(dt)
  dt1.paw <- dt1.paw[!is.na(paw)]
  dt1.paw <- dt1.paw[is.finite(paw) & paw > 0 & paw <= 1]
  dt1.paw <- dt1.paw[,list(paw.mean = mean(paw,na.rm=T),
                           paw.se = sd(paw,na.rm = T)/sqrt(sum(!is.na(paw)))),by='id']
  dt1 <- merge(dt1,dt1.paw,by='id')
  m.paw <- lm(paw.mean~ I(A_C_OF^0.5)+A_CLAY_MI*A_C_OF,data=dt1)
  p.paw <- predict(m.paw,newdata = data.frame(A_C_OF = dt1$A_C_OF,A_CLAY_MI = dt1$A_CLAY_MI))
  l.paw <- hpf('PAW',m.paw,r=c(2,4,4,5,8),cfin=c('* C^0.5','* CLAY','* C','* CLAY * C'))
  
  # more is better, optimum score at 0.3 g / g (Moebius-Clune, 2017)
  dt1[, opaw := fifelse(A_CLAY_MI < 40, 0.18, 0.21)]
  dt1[, cpaw := fmod(model = m.paw,var = opaw, A_CLAY_MI)]
  
## --- PMN -----
  
  # potentially mineralizable N
  dt1.pmn <- ptf_pmn_all(dt)
  dt1.pmn <- dt1.pmn[!is.na(pmn)]
  dt1.pmn <- dt1.pmn[is.finite(pmn) & pmn > 0 & pmn <= 1000]
  dt1.pmn <- dt1.pmn[,list(pmn.mean = mean(pmn,na.rm=T),
                           pmn.se = sd(pmn,na.rm=T)/sqrt(sum(!is.na(pmn)))),by='id']
  dt1 <- merge(dt1,dt1.pmn,by='id')
  m.pmn <- lm(pmn.mean~ A_C_OF*A_CLAY_MI+I(A_C_OF^2),data=dt1)
  p.pmn <- predict(m.pmn,newdata = data.frame(A_C_OF = dt1$A_C_OF,A_CLAY_MI = dt1$A_CLAY_MI))
  l.pmn <- hpf('PMN',m.pmn,r=c(2,4,4,4,4),cfin=c('*C','* CLAY','*C^2','* C * CLAY'))
  
  # more is better, optimum score at 30 mg N /kg (Moebius-Clune, 2017)
  dt1[, opmn := 30]
  dt1[, cpmn := fmod(model = m.pmn,var = opmn,A_CLAY_MI)]
  
# ---- HWC -----
  
  # hot water carbon
  dt1.hwc <- ptf_hwc_all(dt)
  dt1.hwc <- dt1.hwc[!is.na(hwc)]
  dt1.hwc <- dt1.hwc[is.finite(hwc) & hwc > 0 & hwc <= 6000]
  dt1.hwc <- dt1.hwc[,list(hwc.mean = mean(hwc,na.rm=T),
                           hwc.se = sd(hwc,na.rm = T)/sqrt(sum(!is.na(hwc)))),by='id']
  dt1 <- merge(dt1,dt1.hwc,by='id')
  m.hwc <- lm(hwc.mean~ A_C_OF*A_CLAY_MI+I(A_C_OF^2),data=dt1)
  p.hwc <- predict(m.hwc,newdata = data.frame(A_C_OF = dt1$A_C_OF,A_CLAY_MI = dt1$A_CLAY_MI))
  l.hwc <- hpf('HWC',m.hwc,r=c(0,1,4,4,4),cfin=c('*C','* CLAY','*C^2','* C * CLAY'))

  dt1[, ohwc := 500]
  dt1[, chwc := fmod(model = m.hwc,var = ohwc,A_CLAY_MI)]
  
# --- metals -----
  
  # shi_metal available in dev
  dt1.met <- shi_metals(A_PH_CC = dt1$A_PH_CC,
                        A_SOM_LOI = dt1$A_C_OF * 2 / 10, 
                        A_CLAY_MI = dt1$A_CLAY_MI,
                        type ='me_crit')
  dt1 <- merge(dt1,dt1.met,by='id')
  m.cd <- lm(ccd~A_C_OF-1,data=dt1)
  m.cu <- lm(ccu~A_C_OF + I(A_C_OF^2),data=dt1)
  m.pb <- lm(cpb~A_C_OF + I(A_C_OF^2),data=dt1)
  m.zn <- lm(czn~A_C_OF + I(A_C_OF^2),data=dt1)
  l.cu <- hpf('Cu',m.cu,r=c(2,2,5),cfin=c('*C','*C^2'))
  l.pb <- hpf('Pb',m.pb,r=c(2,2,5),cfin=c('*C','*C^2'))
  l.cd <- hpf('Cd',m.cd,r=3,cfin='*C')
  l.zn <- hpf('Zn',m.pb,r=c(2,2,5),cfin=c('*C','*C^2'))
  
  
  
  