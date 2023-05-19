# derive ptf for optimum SOC levels
# Ros, 27 april 2023

# require packages
require(data.table); require(soilptf)
require(terra);require(sf)
require(ggplot2);require(patchwork)

rm(list=ls())

# --- step 1. develop meta-ptfs ----
# make a virtual dataset for a sandy soil varying in SOC levels, and derive optimum meta-ptfs per soil function
  
  # helper fun to extract coefficients from lm model for the label with model description
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

  # for mineral soils only: 20% OS = 10% OC = 100 g/kg
  dt1 <- data.table(A_C_OF = seq(0.5,100,1), A_CLAY_MI = 3.5, A_SAND_MI = 60, A_PH_CC = 5.4)
  dt1[,id := .I]
  set.seed(124)
  dt1[,A_CN_FR := rnorm(.N,12,2)]
  dt1[,A_N_RT := A_C_OF * 1000 / A_CN_FR]
  
  # the cation exchange capacity
  dt1.bd <- ptf_bd_all(dt1)
  dt1.bd <- dt1.bd[,list(bd.mean = mean(bd,na.rm=T),
                         bd.se = sd(bd,na.rm=T)/sqrt(sum(!is.na(bd)))),by='id']
  dt1 <- merge(dt1,dt1.bd,by='id')
  m.bd <- lm(bd.mean~A_C_OF + I(log(A_C_OF)),data=dt1)
  p.bd <- predict(m.bd,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.bd <- hpf('BD',m.bd,r=c(0,3,0),cfin=c('*C','*log(C)'))
  
  # the cation exchange capacity
  dt1.cec <- ptf_cec_all(dt1)
  dt1.cec <- dt1.cec[,list(cec.mean = mean(cec,na.rm=T),
                           cec.se = sd(cec,na.rm=T)/sqrt(sum(!is.na(cec)))),by='id']
  dt1 <- merge(dt1,dt1.cec,by='id')
  m.cec <- lm(cec.mean~A_C_OF,data=dt1)
  p.cec <- predict(m.cec,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.cec <- hpf('CEC',m.cec,r=c(1,2),cfin=c('*C'))

  # water stable aggregates
  dt1.wsa <- ptf_wsa_all(dt1)
  dt1.wsa <- dt1.wsa[,list(wsa.mean = mean(wsa,na.rm=T),
                           wsa.se = sd(wsa,na.rm=T)/sqrt(sum(!is.na(wsa)))),by='id']
  dt1 <- merge(dt1,dt1.wsa,by='id')
  m.wsa <- lm(wsa.mean~A_C_OF,data=dt1)
  p.wsa <- predict(m.wsa,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.wsa <- hpf('WSA',m.wsa,r=c(0,3),cfin='*C')
  
  # see hamel for interpretation optimum
  dt1.mwd <- ptf_mwd_all(dt1)
  dt1.mwd <- dt1.mwd[,list(mwd.mean = mean(mwd,na.rm=T),
                           mwd.se = sd(mwd,na.rm = T)/sqrt(sum(!is.na(mwd)))),by='id']
  dt1 <- merge(dt1,dt1.mwd,by='id')
  m.mwd <- lm(mwd.mean~A_C_OF,data=dt1)
  p.mwd <- predict(m.mwd,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.mwd <- hpf('MWD',m.mwd,r=c(3,3),cfin='*C')
  
  # soil shearing
  dt1.sss <- ptf_sss_all(dt1)
  dt1.sss <- dt1.sss[,list(sss.mean = mean(sss,na.rm=T),
                           sss.se = sd(sss,na.rm = T)/sqrt(sum(!is.na(sss)))),by='id']
  dt1 <- merge(dt1,dt1.sss,by='id')
  m.sss <- lm(sss.mean~A_C_OF + I(A_C_OF^2),data=dt1)
  p.sss <- predict(m.sss,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.sss <- hpf('SSS',m.sss,r=c(1,3,4),cfin=c('*C','*C^2'))
  
  # derive water holding capacity
  dt1.whc <- ptf_whc_all(dt1)
  dt1.whc <- dt1.whc[,list(whc.mean = mean(whc,na.rm=T),
                           whc.se = sd(whc,na.rm = T)/sqrt(sum(!is.na(whc)))),by='id']
  dt1 <- merge(dt1,dt1.whc,by='id')
  m.whc <- lm(whc.mean~I(A_C_OF^0.5),data=dt1)
  p.whc <- predict(m.whc,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.whc <- hpf('WHC',m.whc,r=c(4,4),cfin=c('*C^0.5'))

  # derive water plant available water
  dt1.paw <- ptf_paw_all(dt1)
  dt1.paw <- dt1.paw[,list(paw.mean = mean(paw,na.rm=T),
                           paw.se = sd(paw,na.rm = T)/sqrt(sum(!is.na(paw)))),by='id']
  dt1 <- merge(dt1,dt1.paw,by='id')
  m.paw <- lm(paw.mean~ I(A_C_OF^0.5),data=dt1)
  p.paw <- predict(m.paw,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.paw <- hpf('PAW',m.paw,r=c(4,4),cfin=c('*C^0.5'))

  # potentially mineralizable N
  dt1.pmn <- ptf_pmn_all(dt1)
  dt1.pmn <- dt1.pmn[,list(pmn.mean = mean(pmn,na.rm=T),
                           pmn.se = sd(pmn,na.rm=T)/sqrt(sum(!is.na(pmn)))),by='id']
  dt1 <- merge(dt1,dt1.pmn,by='id')
  m.pmn <- lm(pmn.mean~ A_C_OF+I(A_C_OF^2),data=dt1)
  p.pmn <- predict(m.pmn,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.pmn <- hpf('PMN',m.pmn,r=c(2,4,4),cfin=c('*C','*C^2'))

  # hot water carbon
  dt1.hwc <- ptf_hwc_all(dt1)
  dt1.hwc <- dt1.hwc[,list(hwc.mean = mean(hwc,na.rm=T),
                           hwc.se = sd(hwc,na.rm = T)/sqrt(sum(!is.na(hwc)))),by='id']
  dt1 <- merge(dt1,dt1.hwc,by='id')
  m.hwc <- lm(hwc.mean~ A_C_OF+I(A_C_OF^2),data=dt1)
  p.hwc <- predict(m.hwc,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.hwc <- hpf('HWC',m.hwc,r=c(0,1,3),cfin=c('*C','*C^2'))
  
  # disase supressiveness
  dt1[,ods := OBIC::evaluate_logistic(A_C_OF * 2 * 0.1, b = 1.2, x0 = 1.7,v = 0.4)]
  l.ods <- paste('DR ==','1/(1 + exp(-1.2 * (0.2*C - 1.7)))^(1/0.4)')
  
  # pH buffer capacity
  dt1.phbc <- ptf_phbc_all(dt1)
  dt1.phbc <- dt1.phbc[,list(phbc.mean = mean(phbc,na.rm=T),
                             phbc.se = sd(phbc,na.rm=T)/sqrt(sum(!is.na(phbc)))),by='id']
  dt1 <- merge(dt1,dt1.phbc,by='id')
  m.phbc <- lm(phbc.mean~ A_C_OF,data=dt1)
  p.phbc <- predict(m.phbc,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.phbc <- hpf('pHBC',m.phbc,r=c(3,3),cfin='*C')
  
  # metal freundlich coefficient
  dt1.metal <- ptf_metals_all(dt1)
  dt1.metal <- dt1.metal[,list(metal.mean = mean(metal,na.rm=T),
                               metal.se = sd(metal,na.rm=T)/sqrt(sum(!is.na(metal)))),by='id']
  dt1 <- merge(dt1,dt1.metal,by='id')
  m.metal <- lm(metal.mean~ I(A_C_OF^0.5),data=dt1)
  p.metal <- predict(m.metal,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.metal <- hpf('Kf',m.metal,r=c(1,1),cfin=c('*C^0.5'))
  
  # carbon decomposition, minip, and assume 10% uncertainty
  dt1.cdec <- ptf_cdec_all(dt1)
  dt1.cdec <- dt1.cdec[,list(cdec.mean = mean(cdec,na.rm=T),
                             cdec.se = mean(cdec,na.rm=T)*0.10),by='id']
  dt1 <- merge(dt1,dt1.cdec,by='id')
  m.cdec <- lm(cdec.mean~A_C_OF-1,data=dt1)
  p.cdec <- predict(m.cdec,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.cdec <- hpf('C-decomposition',m.cdec,r=2,cfin='*C')
  
  dt1[, cyield := sptf_yield1(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI)]
  m.cyield <- lm(cyield~A_C_OF + I(A_C_OF^2),data=dt1)
  l.cyield <- hpf('yield',m.cyield,r=c(2,2,4),cfin=c('*C','*C^2'))
  
  # individual metals
  dt2 <- shi_metals(A_PH_CC = dt1$A_PH_CC,
                    A_SOM_LOI = dt1$A_C_OF * 2 / 10, 
                    A_CLAY_MI = dt1$A_CLAY_MI,
                    type ='me_crit')
  dt1 <- merge(dt1,dt2,by='id')
  m.cd <- lm(ccd~A_C_OF-1,data=dt1)
  m.cu <- lm(ccu~A_C_OF + I(A_C_OF^2),data=dt1)
  m.pb <- lm(cpb~A_C_OF + I(A_C_OF^2),data=dt1)
  m.zn <- lm(czn~A_C_OF-1,data=dt1)
  l.cu <- hpf('Cu',m.cu,r=c(2,2,5),cfin=c('*C','*C^2'))
  l.pb <- hpf('Pb',m.pb,r=c(2,2,5),cfin=c('*C','*C^2'))
  l.cd <- hpf('Cd',m.cd,r=3,cfin='*C')
  l.zn <- hpf('Zn',m.zn,r=3,cfin='*C')
  
  tabfuns <- data.table(parm = c('yield','phbc','cec','cd','cu', 'zn','pb','metals',
                                 'hwc','ods','pmn','cdec','dens','whc',
                                 'paw','wsa','mwd','sss'),
                        model = c(l.cyield,l.phbc,l.cec,l.cd,l.cu,l.zn,l.pb,l.metal,
                                  l.hwc,l.ods,l.pmn,l.cdec,l.bd,l.whc,l.paw,l.wsa,
                                  l.mwd,l.sss))
    
  fwrite(tabfuns,file = 'D:/ESA/04 articles/2023/som_critical_levels/230508 tabel functions.csv')
  
  
# optimum crop yield (see Young et al., 2021)
# here is the target: dt1[, cyield1 := 0.5 + (1.75 - 0.5) * (A_CLAY_MI - 4)/(38 - 4)]
# model of Oldfield, 118 kg N/ha default

# --- step 2. define optimum SOC (old) ----
 # here an example is given for the dataset prepared above, and its converted to a simple function to apply this procedure on new datasets.
 # note that this function is not yet optimized for speed
 # see optimcarbon_fx 
  # inverse helper function to derive the lowest SOC to achieve the target in soil health indicator
  fmod <- function(model,var){
    
    nd <- data.table(A_C_OF = seq(0,100,0.01))
    nd[, p1 := predict(model,newdata = nd)]
    nd[,diff := round(abs(p1-var),3)]
    setorder(nd,diff,A_C_OF)
    nd <- nd[1]
    return(nd[,A_C_OF])
  }

  # optima for soil health indicators
  dt2 <- dt1[1,.(id,A_C_OF,A_CLAY_MI)]
  dt2[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) ]

  # optimum density for rootability (Ros, 2023)
  dt2[, odens := (1.75 - 0.009 * A_CLAY_MI) * 1000]
  dt2[, cdens := fmod(m.bd,1682.5)]
  
  # optimum CEC level for soil fertility (van Erp, 2001; Ros et al, 2023)
  dt2[, ocec := 100]
  dt2[, ccec := fmod(m.cec,100)]
  
  # optimum level water stable aggregates (from SHI, Moebius-Clune, 2017)
  # The larger the MWD and GMD values are, the higher the average particle size agglomeration of soil aggregates are, and the stronger the stability of soil structure is
  dt2[, owsa := 75]
  dt2[, cwsa := fmod(m.wsa,owsa),by=id]

  # optimum MWD value using method of Le Bissonnais (1996), cited Clergue et al. (2003)
  # lower values are unstable, and high risk on crusttability
  dt2[, omwd := 1.3]
  dt2[, cmwd := fmod(m.mwd,1.3),by=id]
  
  # for shear strength no optimum
  dt2[, osss := NA_real_]
  dt2[, csss := NA_real_]
  
  # more is better, optimum score (medium to high boundery) at 0.3 g / g (Moebius-Clune, 2017)
  dt2[, owhc := 0.45]
  dt2[, cwhc := fmod(m.whc,owhc),by=id]
  
  # more is better, optimum score at 0.3 g / g (Moebius-Clune, 2017)
  dt2[, opaw := fifelse(A_CLAY_MI < 40, 0.18, 0.21)]
  dt2[, cpaw := fmod(m.paw,opaw),by=id]
  
  # more is better, optimum score at 30 mg N /kg (Moebius-Clune, 2017)
  dt2[, opmn := 30]
  dt2[, cpmn := fmod(m.pmn,30),by=id]
  
  # more is better, reference value of BLN arable systems
  dt2[, ohwc := 500]
  dt2[, chws := fmod(m.hwc,ohwc),by=id]
  
  # optimum pH buffer capacity
  dt2[, ophbc :=  1 * 1000* (1000000 / (0.22 * bd * 100 * 100)) * 2/74.09]
  dt2[, cphbc := fmod(m.phbc,ophbc),by=id]
 
  # optimum C decomposition is not higher than what can be added by annual grassland over 10 years
  # kg EOS * 0.5 = kg C / ha
  dt2[, ocdec := (3975 + 60 * 50) * 0.5 *1000 * 10/ ((2125 - 1.056 * A_C_OF - 257 * log(A_C_OF))*100*100*0.3)]
  dt2[, ccdec := fmod(m.cdec,ocdec),by=id]
  
  # optimum metal sorption, by default the highest one
  dt2[, ometals := 100]
  dt2[, cmetals := NA_real_]
  
  # optimum crop yield based on Oldfield et al. (2009)
  dt2[, oyield := 3]
  dt2[, cyield := fmod(m.cyield,3),by=id]
  
  # melt
  dt3 <- melt(dt2,
              id.vars = c('id','A_C_OF','A_CLAY_MI'),
              measure=patterns(target="^o", copt = "^c"))
  vars <- c('dens','cec','wsa','mwd','sss','whc','paw','pmn','hwc','phbc','cdec','metals','yield')
  dt3[,parm := vars[variable]]
  
  dt4 <- dt3[!is.na(copt)]
  dt4[grepl('hwc|pmn|dec',parm),parmc := 'darkgreen']
  dt4[grepl('yield',parm),parmc := 'black']
  dt4[grepl('wsa|mwd|dens|whc|paw',parm),parmc := 'skyblue']
  dt4[grepl('phbc|cec',parm),parmc := 'orange']
  dt4[,parm := factor(parm,levels = rev(c('yield','phbc','cec',
                                         'hwc','pmn','cdec','wsa','mwd','dens','whc','paw'
                                         )))]
  dt4[,wf := .N,by=parmc]
  dt4[,copt_av := weighted.mean(copt,w = wf)]
  
  # plot the optimum SOC per soil health indicator
  pfs <-  ggplot(data = dt4, aes(x= parm, y=copt)) +
          geom_segment( aes(x=parm, xend=parm, y=0, yend=copt), color=dt4$parmc) +
          geom_hline(yintercept = dt4$copt_av, linetype="dotted", color = "black", linewidth=1)+
          geom_point(color = dt4$parmc, size=4, alpha=0.6) +
          theme_bw() +
          coord_flip() + ylab('Optimum SOC (g/kg)') + xlab('') +
          ggtitle('Optimum SOC levels per soil indicator')
  ggsave(filename = "D:/ESA/04 articles/2023/som_critical_levels/230427_opt_funcions.png",
         plot = pfs, width = 10, height = 10, units = c("cm"), dpi = 1200)


# --- step 3. add functionality in optimcarbon function ----
optimcarbon <- function(xs, dtr){
    
    # make internal data.table  
    dt.oc <- copy(dtr)[xs]
    
    dt.oc[, id := 1:.N]
    
    # estimate bulk density (Ros & de Vries, 2023)
    dt.oc[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) ]
    
    # optimum density for rootability (Ros, 2023)
    dt.oc[, odens := (1.75 - 0.009 * A_CLAY_MI) * 1000]
    dt.oc[, cdens := fmod(m.bd,odens),by=id]
    
    # optimum CEC level for soil fertility (van Erp, 2001; Ros et al, 2023)
    dt.oc[, ocec := 100]
    dt.oc[, ccec := fmod(m.cec,100),by=id]
    
    # optimum level water stable aggregates (from SHI, Moebius-Clune, 2017)
    # The larger the MWD and GMD values are, the higher the average particle size agglomeration of soil aggregates are, and the stronger the stability of soil structure is
    dt.oc[, owsa := 75]
    dt.oc[, cwsa := fmod(m.wsa,owsa),by=id]
    
    # optimum MWD value using method of Le Bissonnais (1996), cited Clergue et al. (2003)
    # lower values are unstable, and high risk on crusttability
    dt.oc[, omwd := 1.3]
    dt.oc[, cmwd := fmod(m.mwd,1.3),by=id]
    
    # for shear strength no optimum
    dt.oc[, osss := NA_real_]
    dt.oc[, csss := NA_real_]
    
    # more is better, optimum score (medium to high boundery) at 0.3 g / g (Moebius-Clune, 2017)
    dt.oc[, owhc := 0.45]
    dt.oc[, cwhc := fmod(m.whc,owhc),by=id]
    
    # more is better, optimum score at 0.3 g / g (Moebius-Clune, 2017)
    dt.oc[, opaw := fifelse(A_CLAY_MI < 40, 0.18, 0.21)]
    dt.oc[, cpaw := fmod(m.paw,opaw),by=id]
    
    # more is better, optimum score at 30 mg N /kg (Moebius-Clune, 2017)
    dt.oc[, opmn := 30]
    dt.oc[, cpmn := fmod(m.pmn,30),by=id]
    
    # more is better, reference value of BLN arable systems
    dt.oc[, ohwc := 500]
    dt.oc[, chws := fmod(m.hwc,ohwc),by=id]
    
    # optimum pH buffer capacity
    dt.oc[, ophbc :=  1 * 1000* (1000000 / (0.22 * bd * 100 * 100)) * 2/74.09]
    dt.oc[, cphbc := fmod(m.phbc,ophbc),by=id]
    
    # optimum C decomposition is not higher than what can be added by annual grassland
    # kg EOS * 0.5 = kg C / ha
    dt.oc[, ocdec := 3975 * 0.5 *1000 * 10/ ((2125 - 1.056 * A_C_OF - 257 * log(A_C_OF))*100*100*0.3)]
    dt.oc[, ccdec := fmod(m.cdec,ocdec),by=id]
    
    # optimum metal sorption, by default the highest one
    dt.oc[, ometals := 100]
    dt.oc[, cmetals := NA_real_]
    
    # optimum crop yield based on Oldfield et al. (2009)
    dt.oc[, oyield := 3]
    dt.oc[, cyield := fmod(m.cyield,3),by=id]
    
    # melt
    dt3 <- melt(dt.oc,
                id.vars = c('id','A_C_OF','A_CLAY_MI'),
                measure=patterns(target="^o", copt = "^c"))
    vars <- c('dens','cec','wsa','mwd','sss','whc','paw','pmn','hwc','phbc','cdec','metals','yield')
    dt3[,parm := vars[variable]]
    
    dt4 <- dt3[!is.na(copt)]
    dt4[grepl('hwc|pmn|dec',parm),parmc := 'darkgreen']
    dt4[grepl('yield',parm),parmc := 'black']
    dt4[grepl('wsa|mwd|dens|whc|paw',parm),parmc := 'skyblue']
    dt4[grepl('phbc|cec',parm),parmc := 'orange']
    dt4[,parm := factor(parm,levels = rev(c('yield','phbc','cec',
                                            'hwc','pmn','cdec','wsa','mwd','dens','whc','paw')))]
    dt4[,wf := .N,by=parmc]
    dt5 <- dt4[,list(copt_av = weighted.mean(copt,w = wf))]
    
    out <- dt5[,copt_av]
    
    # return out
    return(out)
}
  
  dt.oc <- copy(dt1[,.(A_C_OF,A_CLAY_MI,A_SAND_MI,A_PH_CC)])
  
  # analytical solver
  optimcarbon_fix <- function(A_C_OF,A_CLAY_MI,A_SAND_MI,A_PH_CC,type = 'copt'){
    
    # make internal data.table  
    dt.oc <- data.table(A_C_OF = A_C_OF,
                        A_CLAY_MI = A_CLAY_MI,
                        A_SAND_MI = A_SAND_MI,
                        A_PH_CC = A_PH_CC)
    
    dt.oc[, id := 1:.N]
    
    # estimate bulk density (Ros & de Vries, 2023)
    dt.oc[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) ]
    
    # optimum density for rootability (Ros, 2023)
    # https://math.stackexchange.com/questions/433717/how-to-solve-equations-with-logarithms-like-this-ax-b-logx-c-0
    dt.oc[, odens := (1.75 - 0.009 * A_CLAY_MI) * 1000]
    dt.oc[, fa := m.bd$coefficients[2]]
    dt.oc[, fc := m.bd$coefficients[1] - odens]
    dt.oc[, fb := m.bd$coefficients[3]]
    dt.oc[, cdens := (fb/fa) * pracma::lambertWp((fa/fb)*exp(-fc/fb))]
    dt.oc[,c('fa','fb','fc') := NULL]
    
    # optimum CEC level for soil fertility (van Erp, 2001; Ros et al, 2023)
    dt.oc[, ocec := 100]
    dt.oc[, ccec := (ocec - m.cec$coefficients[1])/m.cec$coefficients[2]]
    
    # optimum level water stable aggregates (from SHI, Moebius-Clune, 2017)
    # The larger the MWD and GMD values are, the higher the average particle size agglomeration of soil aggregates are, and the stronger the stability of soil structure is
    dt.oc[, owsa := 75]
    dt.oc[, cwsa := (owsa - m.wsa$coefficients[1])/m.wsa$coefficients[2]]
    
    # optimum MWD value using method of Le Bissonnais (1996), cited Clergue et al. (2003)
    # lower values are unstable, and high risk on crusttability
    dt.oc[, omwd := 1.3]
    dt.oc[, cmwd := (omwd - m.mwd$coefficients[1])/m.mwd$coefficients[2]]
    
    # for shear strength no optimum
    dt.oc[, osss := NA_real_]
    dt.oc[, csss := NA_real_]
    
    # more is better, optimum score (medium to high boundery) at 0.3 g / g (Moebius-Clune, 2017)
    dt.oc[, owhc := 0.45]
    dt.oc[, cwhc := ((owhc - m.whc$coefficients[1])/m.whc$coefficients[2])^2]
    
    # more is better, optimum score at 0.3 g / g (Moebius-Clune, 2017), border shi score 80 (from high to very high), 0.18 sand, 0.21 others
    # if border from medium to high, then sand 0.13 and others 0.16
    dt.oc[, opaw := pmax(0.18,0.21 + A_SAND_MI * (0.18 - 0.21)/50)]
    #dt.oc[, opaw := pmax(0.13,0.16 + A_SAND_MI * (0.13 - 0.16)/50)]
    dt.oc[, cpaw := ((opaw - m.paw$coefficients[1])/m.paw$coefficients[2])^2]
    
    # more is better, optimum score at 30 mg N /kg (Moebius-Clune, 2017)
    dt.oc[, opmn := 30]
    dt.oc[,fa := m.pmn$coefficients[3]]
    dt.oc[,fb := m.pmn$coefficients[2]]
    dt.oc[,fc := m.pmn$coefficients[1] - opmn]
    dt.oc[,cpmn1 := (-fb + sqrt(fb^2 - 4 * fa * fc))/(2*fa)]
    dt.oc[,cpmn2 := (-fb - sqrt(fb^2 - 4 * fa * fc))/(2*fa)]
    dt.oc[cpmn1 >= 0 & cpmn2 >= 0, cpmn := pmin(cpmn1,cpmn2)]
    dt.oc[cpmn1 >= 0 & cpmn2 < 0, cpmn := cpmn1]
    dt.oc[cpmn1 < 0 & cpmn2 >= 0, cpmn := cpmn2]
    dt.oc[,c('cpmn1','cpmn2','fa','fb','fc') := NULL]
    
    # more is better, reference value of BLN arable systems
    dt.oc[, ohwc := 500]
    dt.oc[,fa := m.hwc$coefficients[3]]
    dt.oc[,fb := m.hwc$coefficients[2]]
    dt.oc[,fc := m.hwc$coefficients[1] - ohwc]
    dt.oc[,chwc1 := (-fb + sqrt(fb^2 - 4 * fa * fc))/(2*fa)]
    dt.oc[,chwc2 := (-fb - sqrt(fb^2 - 4 * fa * fc))/(2*fa)]
    dt.oc[chwc1 >= 0 & chwc2 >= 0, chwc := pmin(chwc1,chwc2)]
    dt.oc[chwc1 >= 0 & chwc2 < 0, chwc := chwc1]
    dt.oc[chwc1 < 0 & chwc2 >= 0, chwc := chwc2]
    dt.oc[,c('chwc1','chwc2','fa','fb','fc') := NULL]
    
    # disease supressiveness
    dt.oc[,ods := 0.99]
    dt.oc[,b := 1.2]
    dt.oc[,x0 := 1.7]
    dt.oc[,v := 0.4]
    dt.oc[,cods := log(exp(log(1/ods)/(1/v))-1)/-b+x0]
    dt.oc[,c('b','x0','v') := NULL]
    
    # optimum pH buffer capacity
    dt.oc[, ophbc :=  1 * 1000* (1000000 / (0.22 * bd * 100 * 100)) * 2/74.09]
    dt.oc[, cphbc := (ophbc - m.phbc$coefficients[1])/m.phbc$coefficients[2]]
    
    # optimum C decomposition is not higher than what can be added by annual grassland
    # kg EOS * 0.5 = kg C / ha
    dt.oc[, ocdec := (3975 + 60 * 50) * 0.5 *1000 * 10/ ((2125 - 1.056 * A_C_OF - 257 * log(A_C_OF))*100*100*0.3)]
    dt.oc[, ccdec := ocdec/m.cdec$coefficients[1]]
    
    # optimum metal sorption, by default the highest one
    dt.oc[, ometals := 100]
    dt.oc[, cmetals := NA_real_]
    
    # derive critical SOC values for metal concentrations for Ecoogical Health (de Vries et al., 2008)
    dt.oc.metals <- shi_metals(A_PH_CC = dt.oc$A_PH_CC,
                               A_SOM_LOI = dt.oc$A_C_OF * 2 / 10, 
                               A_CLAY_MI = dt.oc$A_CLAY_MI
                               )
    
    dt.oc[,omcd := NA_real_]
    dt.oc[,cmcd := dt.oc.metals$a_cd_rt]
    dt.oc[,omcu := NA_real_]
    dt.oc[,cmcu := dt.oc.metals$a_cu_rt]
    dt.oc[,ompb := NA_real_]
    dt.oc[,cmpb := dt.oc.metals$a_pb_rt]
    dt.oc[,omzn := NA_real_]
    dt.oc[,cmzn := dt.oc.metals$a_zn_rt]
    
    # optimum SOC for crop yield based on Oldfield et al. (2009)
    dt.oc[, oyield := 3]
    #dt.oc[, cyieldo := fmod(m.cyield,3),by=id]
    dt.oc[,fa := m.cyield$coefficients[3]]
    dt.oc[,fb := m.cyield$coefficients[2]]
    dt.oc[,fc := m.cyield$coefficients[1] - oyield]
    dt.oc[,chwc1 := (-fb + sqrt(fb^2 - 4 * fa * fc))/(2*fa)]
    dt.oc[,chwc2 := (-fb - sqrt(fb^2 - 4 * fa * fc))/(2*fa)]
    dt.oc[chwc1 >= 0 & chwc2 >= 0, cyield1 := pmin(chwc1,chwc2)]
    dt.oc[chwc1 >= 0 & chwc2 < 0, cyield1 := chwc1]
    dt.oc[chwc1 < 0 & chwc2 >= 0, cyield1 := chwc2]
    dt.oc[,c('chwc1','chwc2','fa','fb','fc') := NULL]
    
    # optimum SOC for crop yield based on Korschens
    # 0.5% at 4% clay and 1.75% for 38% of clay
    dt.oc[, cyield2 := 10* pmax(0.5,pmin(1.75,0.5 + (A_CLAY_MI -4) * (1.75-0.5)/(38-4)))]
    dt.oc[, cyield := (cyield1 + cyield2)/2]
    dt.oc[,c('cyield1','cyield2'):=NULL]
    
    # melt
    dt3 <- melt(dt.oc,
                id.vars = c('id','A_C_OF','A_CLAY_MI'),
                measure=patterns(target="^o", copt = "^c"))
    vars <- c('dens','cec','wsa','mwd','sss','whc','paw','pmn','hwc','ods','phbc','cdec','metals','cd','cu','pb','zn','yield')
    dt3[,parm := vars[variable]]
    
    dt4 <- dt3[!is.na(copt)]
    dt4[grepl('hwc|pmn|dec|ods',parm),parmc := 'darkgreen']
    dt4[grepl('yield',parm),parmc := 'black']
    dt4[grepl('wsa|mwd|dens|whc|paw',parm),parmc := 'skyblue']
    dt4[grepl('phbc|cec|cu|pb|zn|cd',parm),parmc := 'orange']
    dt4[,parm := factor(parm,levels = rev(c('yield','phbc','cec','cd','cu','pb','zn',
                                            'hwc','ods','pmn','cdec','wsa','mwd','dens','whc','paw')))]
    # weging contributie of soil health indicators
    dt4[parm=='dens', wf := 2/8]
    dt4[grepl('whc|paw',parm), wf := 1/8]
    dt4[grepl('wsa|mwd',parm), wf := 1/8]
    dt4[parm=='cdec', wf := 1/8]
    dt4[parm=='hwc', wf := 4/8]
    dt4[parm=='pmn', wf := 3/8]
    dt4[parm=='cec', wf := 3/8]
    dt4[parm=='phbc', wf := 1/8]
    dt4[grepl('cu|pb|zn|cd',parm), wf := 1/32]
    dt4[parm=='yield', wf := 8/8]
    dt4[parm=='ods', wf := 1/8]
    
    dt4[grepl('hwc|pmn|dec|ods',parm),wf0 := 25]
    dt4[grepl('yield',parm),wf0 := 25]
    dt4[grepl('wsa|mwd|dens|whc|paw',parm),wf0 := 25]
    dt4[grepl('phbc|cec|cu|n|pb|cd',parm),wf0 := 25]
    
    dt4[,wf1 := wf0 * wf]
    
    dt5 <- dt4[,list(copt_av = weighted.mean(copt,w = wf1),
                     copt_se = sqrt(Hmisc::wtd.var(copt,weights = wf1,normwt = F))/sqrt(.N),
                     copt_oa = max(copt),
                     copt_qant = quantile(copt,0.75),
                     copt_median = median(copt)),by=id]
    
    if(type=='copt'){ out <- dt5[,.(copt_av,copt_se)]}
    if(type=='summary'){out <- dt5}
    if(type=='all'){out <- dt4}
    
    # return out
    return(out)
  }
  
  # predict a change in carbon (expressed in delta distance to /above target)
  predcarbon <- function(A_C_OF,A_CLAY_MI,A_SAND_MI,A_PH_CC,type = 'ddtt',type2 ='max',dsoc = 1){
    
    # make internal data.table  
    dp <- data.table(A_C_OF = A_C_OF,
                     A_CLAY_MI = A_CLAY_MI,
                     A_SAND_MI = A_SAND_MI,
                     A_PH_CC = A_PH_CC)
    
    # add id
    dp[, id := 1:.N]
    
    # estimate bulk density (Ros & de Vries, 2023)
    dp[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) ]
    
    # predict oc dependent crop yield, and SHI for current situation
    dp[, byield := predict(m.cyield,newdata = data.frame(A_C_OF))]
    dp[A_C_OF > 30, byield := 3.3]
    dp[, bcec := predict(m.cec,newdata = data.frame(A_C_OF))]
    dp[, bphbc := predict(m.phbc,newdata = data.frame(A_C_OF))]
    dp[, bmetals := predict(m.metal,newdata = data.frame(A_C_OF))]
    dp[, bcu := predict(m.cu,newdata = data.frame(A_C_OF))]
    dp[, bzn := predict(m.zn,newdata = data.frame(A_C_OF))]
    dp[, bpb := predict(m.pb,newdata = data.frame(A_C_OF))]
    dp[, bcd := predict(m.cd,newdata = data.frame(A_C_OF))]
    dp[, bpmn := predict(m.pmn,newdata = data.frame(A_C_OF))]
    dp[, bhwc := predict(m.hwc,newdata = data.frame(A_C_OF))]
    dp[, bods := OBIC::evaluate_logistic(A_C_OF*2/10, b = 1.2, x0 = 1.7,v = 0.4)]
    dp[, bcdec := predict(m.cdec,newdata = data.frame(A_C_OF))]
    dp[, bbd :=  predict(m.bd,newdata = data.frame(A_C_OF))]
    dp[, bwsa := predict(m.wsa,newdata = data.frame(A_C_OF))]
    dp[, bsss := predict(m.sss,newdata = data.frame(A_C_OF))]
    dp[, bmwd := predict(m.mwd,newdata = data.frame(A_C_OF))]
    dp[, bwhc := predict(m.whc,newdata = data.frame(A_C_OF))]
    dp[, bpaw := predict(m.paw,newdata = data.frame(A_C_OF))]
    
    # predict oc dependent crop yield, and SHI for situation with 1 g/kg C extra
    dp[, ebyield := predict(m.cyield,newdata = data.frame(A_C_OF = A_C_OF + dsoc))]
    dp[A_C_OF > 30, ebyield := 3.3]
    dp[, ebcec := predict(m.cec,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebphbc := predict(m.phbc,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebmetals := predict(m.metal,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebcu := predict(m.cu,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebzn := predict(m.zn,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebpb := predict(m.pb,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebcd := predict(m.cd,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebpmn := predict(m.pmn,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebhwc := predict(m.hwc,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebods := OBIC::evaluate_logistic((A_C_OF + dsoc)*2/10, b = 1.2, x0 = 1.7,v = 0.4)]
    dp[, ebcdec := predict(m.cdec,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebbd :=  predict(m.bd,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebwsa := predict(m.wsa,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebsss := predict(m.sss,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebmwd := predict(m.mwd,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebwhc := predict(m.whc,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    dp[, ebpaw := predict(m.paw,newdata = data.frame(A_C_OF =A_C_OF + dsoc))]
    
    # factor to estimate max
    cfmax <- 3
    
    # add max per SHI, cfmax x opt
    dp[, odens := 850]
    dp[, ocec := cfmax * 100]
    dp[, owsa := cfmax * 75]
    dp[, omwd := cfmax * 1.3]
    dp[, osss := NA_real_]
    dp[, owhc := cfmax * 0.45]
    dp[, opaw := cfmax * pmax(0.18,0.21 + A_SAND_MI * (0.18 - 0.21)/50)]
    dp[, opmn := cfmax * 30]
    dp[, ohwc := cfmax * 500]
    dp[, oods := 1.0]
    dp[, ophbc :=  cfmax * 1 * 1000* (1000000 / (0.22 * bd * 100 * 100)) * 2/74.09]
    dp[, ocdec := cfmax * (3975 + 60 * 50) * 0.5 *1000 * 10/ ((2125 - 1.056 * A_C_OF - 257 * log(A_C_OF))*100*100*0.3)]
    dp[, ometals := 100]

    dt2 <- shi_metals(A_PH_CC = dp$A_PH_CC,
                      A_SOM_LOI = dp$A_C_OF * 2 / 10, 
                      A_CLAY_MI = dp$A_CLAY_MI,
                      type ='me_crit')
    dp[,omcd := cfmax * dt2$ccd]
    dp[,omcu := cfmax * dt2$ccu]
    dp[,ompb := cfmax * dt2$cpb]
    dp[,omzn := cfmax * dt2$czn]
    dp[, oyield := cfmax * 3]

    if(type2 == 'max'){
      # estimate improvement
      dp[, iyield := (ebyield - byield)/oyield]
      dp[, icec := (ebcec - bcec)/ocec]
      dp[, iphbc := (ebphbc - bphbc)/ophbc]
      dp[, imetals := (ebmetals - bmetals)/ometals]
      dp[, icu := (ebcu - bcu)/omcu]
      dp[, izn := (ebzn - bzn)/omzn]
      dp[, ipb := (ebpb - bpb)/ompb]
      dp[, icd := (ebcd - bcd)/omcd]
      dp[, ipmn := (ebpmn - bpmn)/opmn]
      dp[, ihwc := (ebhwc - bhwc)/ohwc]
      dp[, iods := (ebods - bods)/oods]
      dp[, icdec := (ebcdec - bcdec)/ocdec]
      dp[, ibd := (ebbd - bbd)/-odens]
      dp[, iwsa := (ebwsa - bwsa)/owsa]
      dp[, isss := (ebsss - bsss)/osss]
      dp[, imwd := (ebmwd - bmwd)/omwd]
      dp[, iwhc := (ebwhc - bwhc)/owhc]
      dp[, ipaw := (ebpaw - bpaw)/opaw]
      
    } else {
      
      # estimate improvement
      dp[, iyield := (ebyield / byield)]
      dp[, icec := (ebcec / bcec)]
      dp[, iphbc := (ebphbc / bphbc)]
      dp[, imetals := (ebmetals / bmetals)]
      dp[, icu := (ebcu / bcu)]
      dp[, izn := (ebzn / bzn)]
      dp[, ipb := (ebpb / bpb)]
      dp[, icd := (ebcd / bcd)]
      dp[, ipmn := (ebpmn / bpmn)]
      dp[, ihwc := (ebhwc / bhwc)]
      dp[, iods := (ebods / bods)]
      dp[, icdec := (ebcdec / bcdec)]
      dp[, ibd := (ebbd / bbd)]
      dp[, iwsa := (ebwsa / bwsa)]
      dp[, isss := (ebsss / bsss)]
      dp[, imwd := (ebmwd / bmwd)]
      dp[, iwhc := (ebwhc / bwhc)]
      dp[, ipaw := (ebpaw / bpaw)]
      
    }
   
    
    # melt
    cols <- colnames(dp)[grepl('^i',colnames(dp))]
    cols <- cols[cols != 'id']
   
    dt3 <- melt(dp,
                id.vars = c('id','A_C_OF','A_CLAY_MI'),
                measure.vars = cols,
                variable.name = 'shi',
                value.name ='dist_to_max')
    
    dt4 <- dt3[!is.na(dist_to_max)]
    dt4[grepl('hwc|pmn|dec|ods',shi),parmc := 'darkgreen']
    dt4[grepl('yield',shi),parmc := 'black']
    dt4[grepl('wsa|mwd|dens|bd|whc|paw',shi),parmc := 'skyblue']
    dt4[grepl('phbc|cec|cu|pb|zn|cd',shi),parmc := 'orange']
    dt4[,shi := factor(shi,levels = rev(c('iyield','iphbc','icec','icd','icu','ipb','izn','imetals',
                                           'ihwc','iods','ipmn','icdec','iwsa','imwd','ibd',
                                          'iwhc','ipaw','isss')))]
    # weging contributie of soil health indicators
    dt4[shi=='ibd', wf := 2/8]
    dt4[grepl('whc|paw',shi), wf := 1/8]
    dt4[grepl('wsa|mwd|sss',shi), wf := 1/8]
    dt4[shi=='icdec', wf := 1/8]
    dt4[shi=='ihwc', wf := 4/8]
    dt4[shi=='ipmn', wf := 3/8]
    dt4[shi=='icec', wf := 3/8]
    dt4[shi=='iphbc', wf := 1/8]
    dt4[grepl('cu|pb|zn|cd',shi), wf := 1/32]
    dt4[shi=='iyield', wf := 8/8]
    dt4[shi=='iods', wf := 1/8]
    dt4[shi=='imetals', wf := 1/8]
    
    dt4[grepl('hwc|pmn|dec|ods',shi),wf0 := 25]
    dt4[grepl('yield',shi),wf0 := 25]
    dt4[grepl('wsa|mwd|dens|bd|whc|paw|sss',shi),wf0 := 25]
    dt4[grepl('phbc|cec|cu|n|pb|cd|metal',shi),wf0 := 25]
    
    dt4[,wf1 := wf0 * wf]
    
    dt5 <- dt4[,list(ddtt_av = weighted.mean(dist_to_max,w = wf1),
                     ddtt_se = sqrt(Hmisc::wtd.var(dist_to_max,weights = wf1,normwt = F))/sqrt(.N),
                     ddtt_oa = max(dist_to_max),
                     ddtt_qant = quantile(dist_to_max,0.75),
                     ddtt_median = median(dist_to_max)),by=id]
    
    if(type=='ddtt'){ out <- dt5[,.(ddtt_av,ddtt_se)]}
    if(type=='summary'){out <- dt5}
    if(type=='all'){out <- dt4}
    
    return(out)
    
  }
  
  
  
# --- step 4. SHI comparison -----------
  
  # default soil series with variable SOC
  dt1 <- data.table(A_C_OF = seq(0.5,100,1), A_CLAY_MI = 3.5, A_SAND_MI = 80, A_PH_CC = 5.4)
  dt1[,id := .I]
  set.seed(124)
  dt1[,A_CN_FR := rnorm(.N,12,2)]
  dt1[,A_N_RT := A_C_OF * 1000 / A_CN_FR]
  
  # make a course example
  dt.coarse <- copy(dt1)
  dt.fine <- copy(dt1)
  dt.fine[,A_CLAY_MI := 50]
  dt.fine[,A_SAND_MI := 20]
  
  # calculate optima
  dt.coarse[,c('copt','copt_se') := optimcarbon_fix(A_C_OF,A_CLAY_MI,A_SAND_MI,A_PH_CC)]
  dt.fine[,c('copt','copt_se') := optimcarbon_fix(A_C_OF,A_CLAY_MI,A_SAND_MI,A_PH_CC)]
  
  # make logistic shi
  # a = seq(0,8,0.01)
  # coarse shi
  # plot(a,OBIC::evaluate_logistic(a,b=1.9,x0=1.5,v=0.4),type='l',col='black')
  # medium shi
  # lines(a,OBIC::evaluate_logistic(a,b=1.5,x0=2.5,v=0.5),col='blue')
  # fine
  # lines(a,OBIC::evaluate_logistic(a,b=1.6,x0=3.5,v=0.4),col='red')
  
  # calculate SHI evaluation score
  dt.shi <- data.table(som = seq(0,8,0.01),
                       coarse = OBIC::evaluate_logistic(seq(0,8,0.01),b=1.9,x0=1.5,v=0.4),
                       fine = OBIC::evaluate_logistic(seq(0,8,0.01),b=1.6,x0=3.5,v=0.4))
  
  # convert optimum to soil score and adapt score for axis max
  dt.coarse[,a_c_of := dt1$A_C_OF]
  dt.coarse[,a_som_loi := a_c_of * 2 * 0.1]
  dt.coarse[, score := a_c_of/copt]
  dt.coarse[, score_sd := sqrt((copt_se/copt)^2 + (0.0001/a_c_of)^2) * score * sqrt(16)]
  dt.coarse[,pscore := pmin(1,score)]
  dt.coarse[,pscore_psd := pmin(1.2,score + score_sd)]
  dt.coarse[,pscore_msd := score- score_sd]
  dt.coarse[, texture := 'coarse']
  
  # convert optimum to soil score and adapt score for axis max
  dt.fine[,a_c_of := A_C_OF]
  dt.fine[,a_som_loi := a_c_of * 2 * 0.1]
  dt.fine[, score := a_c_of/copt]
  dt.fine[, score_sd := sqrt((copt_se/copt)^2 + (0.0001/a_c_of)^2) * score * sqrt(16)]
  dt.fine[,pscore := pmin(1,score)]
  dt.fine[,pscore_psd := pmin(1.2,score + score_sd)]
  dt.fine[,pscore_msd := score- score_sd]
  dt.fine[, texture := 'fine']
  
  # classification colouring for sOM
  df.class <- data.table(xmin = rep(0,5),
                         xmax = rep(8,5),
                         ymin = c(0,0.25,0.5,0.75,1.00),
                         ymax = c(0.25,0.5,0.75,1.0,1.25),
                         classUK = c('very low', 'low', 'moderate', 'high', 'very high'),
                         classNL = c('Vrij laag','Laag','Gemiddeld','Hoog','Vrij hoog'))
  df.class[,classUK := factor(classUK,levels=rev(c('very low', 'low', 'moderate', 'high', 'very high')))]
  
  # make a polygon for uncertainty  
  dt.poly.coarse <- rbind(dt.coarse[1:.N,.(a_som_loi,pscore=pmin(1,pscore_psd))],
                          dt.coarse[rev(1:.N),.(a_som_loi,pscore=pmin(1,pscore_msd))])
  dt.poly.coarse <- dt.poly.coarse[a_som_loi<=8]
  
  dt.poly.fine <- rbind(dt.fine[1:.N,.(a_som_loi,pscore=pmin(1,pscore_psd))],
                        dt.fine[rev(1:.N),.(a_som_loi,pscore=pmin(1,pscore_msd))])
  dt.poly.fine <- dt.poly.fine[a_som_loi<=8]
  
  legendti_col <- 'class'
  col5 <- c('#238b45','#238b45','#ffffbf','#fdae61','#d7191c') # '#abdda4'
  require(ggplot2)
  
  # plot evaluation score for sandy soil
  p1 <- ggplot(data = dt.coarse, aes(x = a_som_loi)) +theme_bw() +
        geom_rect(data = df.class, 
                  aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = classUK), 
                  alpha = 0.6, show.legend = FALSE) +
        geom_polygon(data = dt.poly.coarse,aes(x=a_som_loi,y=pscore),fill='gray75',alpha = 0.5,show.legend = F)+
        geom_line(data = dt.coarse, 
                  aes(y = pscore,group = texture,linetype=factor('1',levels=c('1','2')),
                      color='1'), 
                  linewidth = 0.8) + xlim(0,8) + 
          theme(legend.position = c(0.75,0.25)) + 
        geom_line(data = dt.coarse, aes(y = pmin(1,pscore_psd),linetype='2', color = '2'), linewidth = 0.4,show.legend = F)+
        geom_line(data = dt.coarse, aes(y = pmin(1,pscore_msd),linetype='2',color = '2'),linewidth = 0.4,show.legend = F)+
        geom_line(data = dt.shi,aes(x = som, y = coarse,linetype = '3',color='3'),linewidth = 0.8,show.legend = F)+  
        scale_fill_manual(name = legendti_col, values=col5) +
        scale_linetype_manual(values = c('solid','solid','dotdash'),
                              labels = c('mean (this study)','standard deviation (this study)','SHI (Cornell)'),
                              name ='SOM assessment') +
        scale_color_manual(values = c('gray25','gray75','gray25'),
                           labels = c('mean (this study)','standard deviation (this study)','SHI (Cornell)'),
                           name ='SOM assessment'
                           ) +
          scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1.0, by = 0.25)) +
          xlab('Soil organic matter (%)') + ylab('Score') +
        labs(title = "Agronomic SOM Assessment Score",
            subtitle = "for a coarse sandy soil") + ptl
  
    
  # plot evaluation score for fine textured soil
  p2 <- ggplot(data = dt.fine, aes(x = a_som_loi)) +theme_bw() +
        geom_rect(data = df.class, 
                  aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = classUK), 
                  alpha = 0.6, show.legend = FALSE) +
        geom_polygon(data = dt.poly.fine,aes(x=a_som_loi,y=pscore),fill='gray75',alpha = 0.5,show.legend = F)+
        geom_line(data = dt.fine, 
                  aes(y = pscore,linetype=factor('1',levels=c('1','2')),color='1'), 
                  linewidth = 0.8) + xlim(0,8) + 
        theme(legend.position = c(0.75,0.25)) + 
        geom_line(data = dt.fine, aes(y = pmin(1,pscore_psd),linetype='2', color = '2'), linewidth = 0.4,show.legend = F)+
        geom_line(data = dt.fine, aes(y = pmin(1,pscore_msd),linetype='2',color = '2'),linewidth = 0.4,show.legend = F)+
        geom_line(data = dt.shi,aes(x = som, y = fine,linetype = '3',color='3'),linewidth = 0.8,show.legend = F)+  
        scale_fill_manual(name = legendti_col, values=col5) +
        scale_linetype_manual(values = c('solid','solid','dotdash'),
                              labels = c('mean (this study)','standard deviation (this study)','SHI (Cornell)'),
                              name ='SOM assessment') +
        scale_color_manual(values = c('gray25','gray75','gray25'),
                           labels = c('mean (this study)','standard deviation (this study)','SHI (Cornell)'),
                           name ='SOM assessment') +
        scale_y_continuous(limits=c(0, 1), breaks=seq(0, 1.0, by = 0.25)) +
        xlab('Soil organic matter (%)') + ylab('Score') +
        labs(title = "Agronomic SOM Assessment Score",
         subtitle = "for a fine textured clay soil") + ptl
  
  p12 <- p1 | p2
  ggsave(plot=p12,filename = "D:/ESA/04 articles/2023/som_critical_levels/shi_comparison.png",
          width = 33, height = 17, units = c("cm"), dpi = 1200)
  
# --- step 5. plot per indicator -----
  
  # default soil series with variable SOC
  dt1 <- data.table(A_C_OF = c(7.5,7.5), A_CLAY_MI = c(3.5,25), A_SAND_MI = c(80,20), A_PH_CC = c(5.4,6.5))
  dt1[,id := .I]
  dt1[,A_N_RT := A_C_OF * 1000 / 12]
  
  d1.opt <- optimcarbon_fix(dt1$A_C_OF,
                            dt1$A_CLAY_MI,
                            dt1$A_SAND_MI,
                            dt1$A_PH_CC, 
                            type ='all')
  d1.opt[,parm := factor(parm,levels = rev(c('yield','phbc','cec','cd','cu', 'zn','pb',
                                          'hwc','ods','pmn','cdec','dens','whc','paw','wsa','mwd')))]
  d1.opt.sand <- d1.opt[id==1,]
  d1.opt.sand[,copt_av := weighted.mean(copt,w=wf1)]
  d1.opt.sand[,wfn := 100/.N,by='parmc']
  d1.opt.sand[,copt_av2 := weighted.mean(copt,w=wfn)]
  d1.opt.sand[, parmc := factor(parmc,levels= c('black','orange','darkgreen','skyblue'))]
  
  d1.opt.clay <- d1.opt[id==2,]
  d1.opt.clay[,copt_av := weighted.mean(copt,w=wf1)]
  d1.opt.clay[,wfn := 100/.N,by='parmc']
  d1.opt.clay[,copt_av2 := weighted.mean(copt,w=wfn)]
  d1.opt.clay[, parmc := factor(parmc,levels= c('black','orange','darkgreen','skyblue'))]
  
  
  # plot the optimum SOC per soil health indicator for sandy soil
  pf1 <-  ggplot(data = d1.opt.sand, aes(x= parm, y=copt,color=parmc)) +
          geom_segment( aes(x=parm, xend=parm, y=0, yend=copt)) +
          geom_hline(yintercept = d1.opt.sand$copt_av, linetype="dotted", 
                     color = "black", linewidth=1)+
          geom_point(size=4, alpha=0.6) +
          theme_bw() +
          scale_color_manual(values = c('black','orange','darkgreen','skyblue'),
                       labels = c('crop yield','chemical','biological','physical'),
                       name ='SOM assessment')+
          coord_flip() + ylab('Critical SOC (g/kg)') + xlab('') +
          theme(legend.position = c(0.8,0.8))+
          labs(title = "Agronomic SOM Assessment per Soil Health Indicator",
               subtitle = "for a coarse sandy soil (with 1.5% SOM, 3.5% clay and pH 5.4)") + ptl
  
  # plot the optimum SOC per soil health indicator for clay soil
  pf2 <-  ggplot(data = d1.opt.clay, aes(x= parm, y=copt,color=parmc)) +
          geom_segment( aes(x=parm, xend=parm, y=0, yend=copt)) +
          geom_hline(yintercept = d1.opt.clay$copt_av, linetype="dotted", 
                     color = "black", linewidth=1)+
          geom_point(size=4, alpha=0.6) +
          theme_bw() +
          scale_color_manual(values = c('black','orange','darkgreen','skyblue'),
                             labels = c('crop yield','chemical','biological','physical'),
                             name ='SOM assessment')+
          coord_flip() + ylab('Critical SOC (g/kg)') + xlab('') +
          theme(legend.position = c(0.8,0.8))+
          labs(title = "Agronomic SOM Assessment per Soil Health Indicator",
               subtitle = "for a finetextured clay soil (with 1.5% SOM, 25% clay and pH 6.5)") + ptl
                    
  pf12 <- pf1 | pf2 
  ggsave(filename = "D:/ESA/04 articles/2023/som_critical_levels/230506_opt_funcions.png",
         plot = pf12, width = 33, height = 17, units = c("cm"), dpi = 1200)
  
  
  
  
# --- step 6. plot figures of meta-ptfs ----
  
  # plot figures of the pedotransfer functions
  ptl <- theme(plot.subtitle=element_text(size=10, face="italic", color="black"),
               axis.text = element_text(size = 12,colour ='black'),
               axis.title = element_text(size = 12,colour ='black'),
               title = element_text(size=12,colour ='black'))
  
  p1 <- ggplot(data = dt1,aes(x = A_C_OF,y=cec.mean)) + geom_point() + geom_line()+
        geom_errorbar(aes(ymin=cec.mean - cec.se, ymax = cec.mean + cec.se),width = 0.2) +
        geom_line(aes(y = p.cec),col='red')+
        ylim(0,400) +
        annotate('text',x = 2.5, y = 350, label = l.cec,parse = T,size = 4,adj=0) + 
        xlab('organic carbon content (g/kg)') + ylab('CEC (mmol+/kg)') + theme_bw() +
        labs(title = "A. Relationship between CEC and SOC",
             subtitle = "derived from 75 ptfs for mineral soils") + ptl
  
  p2 <- ggplot(data = dt1,aes(x = A_C_OF,y=wsa.mean)) + geom_point() + geom_line()+
        geom_errorbar(aes(ymin=wsa.mean - wsa.se, ymax = wsa.mean + wsa.se),width = 0.2) +
        geom_line(aes(y = p.wsa),col='red')+
        ylim(0,250) +
        annotate('text',x = 2.5, y = 200, label = l.wsa,parse = T,size = 4,adj=0) + 
        xlab('organic carbon content (g/kg)') + ylab('WSA (%)') + theme_bw() +
        labs(title = "B. Relationship between WSA and SOC",
             subtitle = "derived from 9 ptfs for mineral soils") + ptl
  
  p3 <- ggplot(data = dt1,aes(x = A_C_OF,y=mwd.mean)) + geom_point() + geom_line()+
        geom_errorbar(aes(ymin=mwd.mean - mwd.se, ymax = mwd.mean + mwd.se),width = 0.2) +
        geom_line(aes(y = p.mwd),col='red')+
        ylim(0,5) +
        annotate('text',x = 2.5, y = 4, label = l.mwd,parse = T,size = 4,adj=0) + 
        xlab('organic carbon content (g/kg)') + ylab('MWD (mm)') + theme_bw() +
        labs(title = "C. Relationship between MWD and SOC",
             subtitle = "derived from 15 ptfs for mineral soils") + ptl
  
  p4 <- ggplot(data = dt1,aes(x = A_C_OF,y=sss.mean)) + geom_point() + geom_line()+
        geom_errorbar(aes(ymin=sss.mean - sss.se, ymax = sss.mean + sss.se),width = 0.2) +
        geom_line(aes(y = p.sss),col='red')+
        ylim(0,250) +
        annotate('text',x = 2.5, y = 180, label = l.sss,parse = T,size = 4,adj=0) + 
        xlab('organic carbon content (g/kg)') + ylab('SSS (kg cm-2)') + theme_bw() +
        labs(title = "D. Relationship between SSS and SOC",
             subtitle = "derived from 3 ptfs for mineral soils") + ptl
  
  p5 <- ggplot(data = dt1,aes(x = A_C_OF,y=whc.mean)) + geom_point() + geom_line()+
        geom_errorbar(aes(ymin=whc.mean - whc.se, ymax = whc.mean + whc.se),width = 0.2) +
        geom_line(aes(y = p.whc),col='red')+
        ylim(0.25,0.75) +
        annotate('text',x = 2.5, y = 0.6, label = l.whc,parse = T,size = 4,adj=0) + 
        xlab('organic carbon content (g/kg)') + ylab('WHC (-)') + theme_bw() +
        labs(title = "E. Relationship between WHC and SOC",
             subtitle = "derived from 10 ptfs for mineral soils") + ptl
  
  p6 <- ggplot(data = dt1,aes(x = A_C_OF,y=paw.mean)) + geom_point() + geom_line()+
        geom_errorbar(aes(ymin=paw.mean - paw.se, ymax = paw.mean + paw.se),width = 0.2) +
        geom_line(aes(y = p.paw),col='red')+
        ylim(0.1,0.3) +
        annotate('text',x = 2.5, y = 0.25, label = l.whc,parse = T,size = 4,adj=0) + 
        xlab('organic carbon content (g/kg)') + ylab('PAW (-)') + theme_bw() +
        labs(title = "F. Relationship between PAW and SOC",
             subtitle = "derived from 15 ptfs for mineral soils") + ptl
  
  p7 <- ggplot(data = dt1,aes(x = A_C_OF,y=pmn.mean)) + geom_point() + geom_line()+
        geom_errorbar(aes(ymin=pmn.mean - pmn.se, ymax = pmn.mean + pmn.se),width = 0.2) +
        geom_line(aes(y = p.pmn),col='red')+
        ylim(0,350) +
        annotate('text',x = 2.5, y = 250, label = l.pmn,parse = T,size = 4,adj=0) + 
        xlab('organic carbon content (g/kg)') + ylab('PMN (mg/kg)') + theme_bw() +
        labs(title = "G. Relationship between PMN and SOC",
             subtitle = "derived from 19 ptfs for mineral soils") + ptl
  
  p8 <- ggplot(data = dt1,aes(x = A_C_OF,y=hwc.mean)) + geom_point() + geom_line()+
        geom_errorbar(aes(ymin=hwc.mean - hwc.se, ymax = hwc.mean + hwc.se),width = 0.2) +
        geom_line(aes(y = p.hwc),col='red')+
        ylim(0,3000) +
        annotate('text',x = 2.5, y = 2500, label = l.hwc,parse = T,size = 4,adj=0) + 
        xlab('organic carbon content (g/kg)') + ylab('HWC (g/kg)') + theme_bw() +
        labs(title = "H. Relationship between HWC and SOC",
             subtitle = "derived from 11 ptfs for mineral soils") + ptl
  
  p9 <- ggplot(data = dt1,aes(x = A_C_OF,y=phbc.mean)) + geom_point() + geom_line()+
        geom_errorbar(aes(ymin=phbc.mean - phbc.se, ymax = phbc.mean + phbc.se),width = 0.2) +
        geom_line(aes(y = p.phbc),col='red')+
        ylim(0,150) +
        annotate('text',x = 2.5, y = 120, label = l.phbc,parse = T,size = 4,adj=0) + 
        xlab('organic carbon content (g/kg)') + ylab('pHBC (mmol H+ / kg / pH)') + theme_bw() +
        labs(title = "I. Relationship between pHBC and SOC",
             subtitle = "derived from 8 ptfs for mineral soils") + ptl
  
  p10 <- ggplot(data = dt1,aes(x = A_C_OF,y=metal.mean)) + geom_point() + geom_line()+
        geom_errorbar(aes(ymin=metal.mean - metal.se, ymax = metal.mean + metal.se),width = 0.2) +
        geom_line(aes(y = p.metal),col='red')+
        ylim(0,150) +
        annotate('text',x = 2.5, y = 120, label = l.metal,parse = T,size = 4,adj=0) + 
        xlab('organic carbon content (g/kg)') + ylab('relative change in Kf') + theme_bw() +
        labs(title = "J. Relationship between Freundlich Kf and SOC",
             subtitle = "derived from 4 ptfs for mineral soils") + ptl
  
  p11 <- ggplot(data = dt1,aes(x = A_C_OF,y=cdec.mean)) + geom_point() + geom_line()+
        geom_errorbar(aes(ymin=cdec.mean - cdec.se, ymax = cdec.mean + cdec.se),width = 0.2) +
        geom_line(aes(y = p.cdec),col='red')+
        ylim(0,25) +
        annotate('text',x = 2.5, y = 20, label = l.cdec,parse = T,size = 4,adj=0) + 
        xlab('organic carbon content (g/kg)') + ylab('C decomposition') + theme_bw() +
        labs(title = "K. Relationship between C decomposition and SOC",
             subtitle = "derived from 1 ptfs for mineral soils") + ptl
  
  p12 <- ggplot(data = dt1,aes(x = A_C_OF,y=bd.mean)) + geom_point() + geom_line()+
         geom_errorbar(aes(ymin=bd.mean - bd.se, ymax = bd.mean + bd.se),width = 0.2) +
         geom_line(aes(y = p.bd),col='red')+
         ylim(0,3000) +
         annotate('text',x = 25, y = 2500, label = l.bd,parse = T,size = 4,adj=0) + 
         xlab('organic carbon content (g/kg)') + ylab('bulk density (kg/m3)') + theme_bw() +
         labs(title = "L. Relationship between bulk density and SOC",
              subtitle = "derived from 181 ptfs for mineral soils") + ptl
  
  p13 <- ggplot(data = dt1,aes(x = A_C_OF,y=ods)) + geom_point() + geom_line()+
          geom_errorbar(aes(ymin=ods - 0.05, ymax = ods + 0.05),width = 0.2) +
          ylim(0,1.2) +
          annotate('text',x = 25, y = 0.5, label = l.ods,parse = T,size = 4,adj=0) + 
          xlab('organic carbon content (g/kg)') + ylab('disease resistance score') + theme_bw() +
          labs(title = "M. Relationship between disease resistance and SOC",
               subtitle = "derived from OBIC for mineral soils") + ptl
        
  require(patchwork)
  
  pfin <- (p1 | p2 | p3 | p4) / (p5 | p6 | p7 | p8) / (p9 | p10 | p11 | p12) 
  ggsave(filename = "D:/ESA/04 articles/2023/som_critical_levels/230427_ptfs.png",
         plot = pfin, width = 65, height = 40, units = c("cm"), dpi = 1200)


# --- step 7. apply this approach on integrator dataset ----

  # load an earlier prepared dataset
  db.int <- readRDS('D:/ESA/04 articles/2023/som_critical_levels/dbint.rds')
  
  if(FALSE){
  
    # load in INTEGRATOR data and select only relevant data
    db.int <- fread('D:/ESA/04 articles/2023/som_critical_levels/db_final_europe.csv')
    db.int <- db.int[,.(ncu,crop_name,area_ncu,clay,ph,cn,soc_ref)]
    db.int <- db.int[,.(area = sum(area_ncu),A_CLAY_MI = clay[1],ph = ph[1],cn = cn[1],A_C_OF = soc_ref[1]*10),by=ncu]
    db.int[,A_SAND_MI := (100-A_CLAY_MI)*0.5]
    
    # calculate optimum soc
    db.int[, c('copt','copt_se') := optimcarbon_fix(A_C_OF = A_C_OF,
                                                   A_CLAY_MI = A_CLAY_MI,
                                                   A_SAND_MI = A_SAND_MI,
                                                   A_PH_CC = ph,
                                                   type='copt')]
    
    # # apply the function multi-session (since function is not yet optimized)
    # require(future.apply); require(progressr)
    # plan(multisession)
    # 
    # # Run the schedule for farm evaluation
    # runcoptmodel <- function(xs) {
    #   p <- progressor(along = xs, enable = TRUE)
    #   dt <- future_lapply(xs, optimcarbon, dtr = db.int)
    # }
    # out <- runcoptmodel(1:nrow(db.int))
    # 
    # # combine all outputs into one vector and add to original data.table
    # out <- unlist(out)
    # db.int[,copt := out]
    # saveRDS(db.int,'D:/ESA/04 articles/2023/som_critical_levels/dbint.rds')
    
  }
  

# --- step 8. apply on the EU scale ----
  
  # set theme
  theme_set(theme_bw())
  
  # get the raster to plot
  r1 <- terra::rast('D:/ESA/02 phd projects/01 maddy young/01 data/gncu2010_ext.asc')
  terra::crs(r1) <- 'epsg:3035'
  
  # convert to data.frame
  r1.p <- as.data.frame(r1,xy=TRUE)
  r1.p <- as.data.table(r1.p)
  
  # merge with estimations
  r.ncu <- merge(r1.p, db.int, by.x = 'gncu2010_ext', by.y = 'ncu')
  
  # set columns in right order for conversion to raster
  setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','copt'))
  
  # distance to target
  r.ncu[, copt_dif := copt - A_C_OF]
  
  # convert to spatial raster
  r.fin <- terra::rast(r.ncu,type='xyz')
  #r.fin <- terra::project(r.fin,'epsg:4326',method = 'near')

  # load the shapefile with EU countries and transform to CRS 3035
  eu.sh <- st_read('D:/ESA/04 articles/2023/som_critical_levels/eu_clip.gpkg')
  eu.sh <- st_transform(eu.sh,3035)

  # define here a function to plot the EU map
  visualize <- function(raster, layer, name, breaks, labels, ftitle){
    # select the correct layer
    raster.int <- raster[layer]
    # aggregate raster
    raster.int <- aggregate(raster.int, fact = 10, fun = 'mean')
    # define crs
    plotcrs <- coord_sf(crs = 3035, lims_method = "box")
    #raster to xy
    df <- as.data.frame(raster.int, xy = TRUE)
    #colnames
    colnames(df) <- c("x", "y", "variable")
    # colors
    cols <- c("< 0" = "#8c510a", "0-5" = "#c7eae5", "5-10" = "#35978f", ">10" = "#003c30")
    #plot
    ggplot() +
      geom_tile(data = df, aes(x = x, y = y,
                               fill = cut(variable, breaks,labels = labels))) +
      geom_sf(data = eu.sh,color = "black",fill = NA,show.legend=FALSE) +
      coord_sf(crs = 3035) +
      #scale_fill_viridis_d(direction=-1) + 
      scale_fill_manual(values = cols) +
      theme_bw() +
      #scale_x_continuous(breaks = xlabs, labels = c('10°W', '0','10°E','20°E', '30°E')) +
      #scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N')) +
      xlab("") + ylab("")+
      labs(fill = name) +
      theme(text = element_text(size = 16),
            legend.text=element_text(size=12),
            #legend.position = 'right',
            legend.position = c(0.2,0.8),
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      ggtitle(ftitle)
  }

  # make and save the plot
  pm1 <- visualize(raster = r.fin,
                 layer = 'copt_dif',
                 name = "Distance to Copt\n(g/kg)",
                 breaks = c(-5000,0,5,10,50),
                 labels = c('< 0','0-5','5-10','>10'),
                 ftitle = 'Distance to critical soil C content (g/kg)')
  ggsave(filename = "D:/ESA/04 articles/2023/som_critical_levels/230508_eu_copt.png",
         plot = pm1, width = 25, height = 25, units = c("cm"), dpi = 1200)

# --- step 9. apply on global scale ------
  
  # require packages
  require(sf);require(terra)
  
  # get the rasters with soil properties 
  rlist <- list.files('D:/DATA/01 soil/',pattern='.tif$',full.names = T)
  rlist <- rlist[grepl('0_5',rlist)|grepl('soc_',rlist)]
  r1 <- sds(rlist)
  names(r1) <- gsub('_mean_0_5|isric_','',names(r1))
  r.soil <- rast(r1)  
  names(r.soil) <- names(r1)
  
  # load crop area harvested as raster
  if(FALSE){
    
    # get the file names of the tiffs
    rfiles <- list.files('D:/DATA/04 crop/spam2010', pattern = '_H.tif$',full.names = TRUE)
    
    # read in all files and convert to spatrasters
    r.crop <- sds(rfiles)
    r.crop <- rast(r.crop)
    
    # aggregate to 0.5 x 0.5 degree
    r.crop <- terra::aggregate(r.crop,fact = 0.5/0.083333,fun = "sum", na.rm=T)
    
    # adjust names
    names(r.crop) <- stringr::str_extract(names(r.crop),'[A-Z]{4}')
    
    # sum all areas for all crops
    r.crop.all = app(r.crop,fun = sum,na.rm=T)
    names(r.crop.all) <- 'croparea'
    
    # reproject to isric map
    r.crop <- resample(r.crop.all,r.soil,method='bilinear')
    
    # write raster
    terra::writeRaster(r.crop,'D:/ESA/04 articles/2023/som_critical_levels/global_crops.tif', overwrite = TRUE)
    
    
  } else {
    
    # read the raster with cropping data
    r.crop <- rast('D:/ESA/04 articles/2023/som_critical_levels/global_crops.tif')
    
  }
  
  r.all <- c(r.soil,r.crop)
  r.df <- as.data.frame(r.all,xy = TRUE, na.rm = FALSE)
  r.df <- as.data.table(r.df)  
  r.df[,id := .I]
  
  r.dfa <- r.df[!is.na(croparea) & croparea>0]
  rm(r.all)
  
  # update SOC for first 30 cm and filter values > 0
  r.dfa[, soc := (5 * soc + 10 * soc_mean_5_15 + 15 * soc_mean_15_30)/30]
  r.dfa[,c('soc_mean_15_30', 'soc_mean_5_15') := NULL]
  #r.dfa <- r.dfa[soc>0]
  
  # update units to common ones
  r.dfa[,bdod := bdod * 1000/ 100]
  r.dfa[,clay := clay * 0.1]
  r.dfa[,sand := sand * 0.1]
  r.dfa[,silt := silt * 0.1]
  r.dfa[,ntot := ntot * 1000 / 100]
  r.dfa[,phw := phw * 0.1]
  r.dfa[,soc := soc * 0.1]
  
  cols <- colnames(r.dfa)[!grepl('^x|^y|^id|crop',colnames(r.dfa))]
  r.dfa[, c(cols) := lapply(.SD,function(x) fifelse(x <= 0|is.na(x), 0.01,x)),.SDcols = cols]
  r.dfa[, c(cols) := lapply(.SD,function(x) fifelse(x > quantile(x,0.99), 0.01,x)),.SDcols = cols]
  
  r.dfa[,a_ph_cc := 6.01 + 1.384*(2.285 - 4.819/(1 + exp(-3.935 + 0.608 *phw) + 0.092 * log(0.2)))]
  
  # calculate optimum soc
  r.dfa[, c('copt','copt_se') := optimcarbon_fix(A_C_OF = soc,
                                                 A_CLAY_MI = clay,
                                                 A_SAND_MI = sand,
                                                 A_PH_CC = a_ph_cc,
                                                 type='copt')]
  
  # calculate the weighed average progress in Soil Health after change 1 unit
  r.dfa[, c('cddt','cddt_se') := predcarbon(A_C_OF = soc,
                                           A_CLAY_MI = clay,
                                           A_SAND_MI = sand,
                                           A_PH_CC = a_ph_cc,
                                           type='ddtt',type2='relative')]
  
  # calculate the weighed average progress in Soil Health after change 10 unit
  r.dfa[, c('cddt10','cddt10_se') := predcarbon(A_C_OF = soc,
                                            A_CLAY_MI = clay,
                                            A_SAND_MI = sand,
                                            A_PH_CC = a_ph_cc,
                                            type='ddtt',type2='relative',
                                            dsoc = 10)]
  
  #r.dfa.fin <- rbind(r.dfa,r.df[is.na(croparea) | croparea<=0],fill = T)
  #setorder(r.dfa.fin,'id')
  saveRDS(r.dfa,file='D:/ESA/04 articles/2023/som_critical_levels/dbglobal.rds')
  #saveRDS(r.dfa,file='D:/ESA/04 articles/2023/som_critical_levels/dbglobal_relchange.rds')
  
# --- step 10. plot global maps ---------
  
  library(rnaturalearth)
  library(rnaturalearthdata)
  
  r.dfa.fin <- readRDS('D:/ESA/04 articles/2023/som_critical_levels/dbglobal.rds')
  
  # get base world map
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world <- world[!grepl('Antarctica|Seven', world$continent),]
  
  r.plot <- copy(r.dfa.fin)
  r.plot <- r.plot[!is.na(copt)]
  r.plot[,cdiff := copt- soc]
  
  r.plot <- copy(r.dfa)
  r.plot <- r.plot[!is.na(cddt)]
  
  # plot a basic world map plot (using dbglobal.rds)
  pbreaks <- c(0,14,15,16,18,20)
  plabs <-  c('<13','13-15','15-16','16-18','18-20')
  p1 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
        geom_tile(data = r.plot,
                  aes(x=x,y=y,fill= cut(copt, pbreaks,labels = plabs))) +
        scale_fill_viridis_d(direction=-1,na.translate = F)+ 
        theme_bw() +
        labs(fill = expression(SOC['opt'] (g/kg)))+
        theme(text = element_text(size = 12),
              legend.text=element_text(size=6),
              legend.title = element_text(size=8),
              legend.position = c(0.15,0.26),
              legend.key.size = unit(0.5,'cm'),
              legend.background = element_rect(fill = "white",color='white'),
              panel.border = element_blank(),
              plot.title = element_text(hjust = 0.5,size=14))+ 
        xlab("") + ylab("") +
        ggtitle("Critical SOC in croplands") +
        coord_sf(crs = 4326)
  ggsave(plot = p1, filename = 'D:/ESA/04 articles/2023/som_critical_levels/230508_socopt_global.png',
         width = 20, height = 10, units = c("cm"), dpi = 1200)  
  
  # plot difference critical and current (using dbglobal.rds)
  pbreaks <- c(-1000,-10,0,10,1000)
  plabs <-  c('< -10','-10 - 0','0 - 10','>10')
  r.plot[,ccdiff := cut(cdiff,breaks = pbreaks,labels = plabs)]
  
  p2 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_tile(data = r.plot,aes(x=x,y=y,fill= ccdiff)) +
    scale_fill_viridis_d(direction=-1)+ 
    theme_bw() +
    labs(fill = 'Difference in SOC\n critical minus current') +
    theme(text = element_text(size = 12),
          legend.text=element_text(size=6),
          legend.title = element_text(size=8),
          legend.position = c(0.15,0.26),
          legend.key.size = unit(0.5,'cm'),
          legend.background = element_rect(fill = "white",color='white'),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5,size=14),
          plot.subtitle = element_text(hjust=0.5,size=10,face="italic"))+ 
    xlab("") + ylab("") +
    ggtitle("Difference between critical and current SOC (g/kg)") +
    coord_sf(crs = 4326)
  ggsave(plot = p2, filename = 'D:/ESA/04 articles/2023/som_critical_levels/230506_soc_critdiff.png',
         width = 20, height = 10, units = c("cm"), dpi = 1200)
  
  
  # plot relative change (using dbglobal.rds)
  r.plot[,cddt := cddt * 100]
  pbreaks <- c(0,0.92,1.0,1.3,1.8,1000)
  plabs <-  c('<0.92','0.92-1.00','1.00-1.30','1.30-1.80','>1.80')
  r.plot[,ccddt := cut(cddt,breaks = pbreaks,labels = plabs)]
  
  p2 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
        geom_tile(data = r.plot,aes(x=x,y=y,fill= ccddt)) +
        scale_fill_viridis_d(direction=-1)+ 
        theme_bw() +
        labs(fill = 'Change in Soil Health (%)\n per extra unit C (g/kg)') +
        theme(text = element_text(size = 12),
              legend.text=element_text(size=6),
              legend.title = element_text(size=8),
              legend.position = c(0.15,0.26),
              legend.key.size = unit(0.5,'cm'),
              legend.background = element_rect(fill = "white",color='white'),
              panel.border = element_blank(),
              plot.title = element_text(hjust = 0.5,size=14),
              plot.subtitle = element_text(hjust=0.5,size=10,face="italic"))+ 
        xlab("") + ylab("") +
        ggtitle("Change in Soil Health (%)",subtitle = 'per unit change in SOC (g/kg)') +
        coord_sf(crs = 4326)
  ggsave(plot = p2, filename = 'D:/ESA/04 articles/2023/som_critical_levels/230506_socchange_global2.png',
         width = 20, height = 10, units = c("cm"), dpi = 1200)
  
  # plot relative change with 10 g C/kg (using dbglobal_relchange.rds)
  r.plot[,cddt := (cddt -1)* 100]
  pbreaks <- c(0,0.92,1.0,1.3,1.8,1000)
  plabs <-  c('<0.92','0.92-1.00','1.00-1.30','1.30-1.80','>1.80')
  r.plot[,ccddt := cut(cddt,breaks = pbreaks,labels = plabs)]
  
  r.plot[,cddt10 := (cddt10 -1)* 100]
  pbreaks <- c(0,5,10,15,30,1000)
  plabs <-  c('<5','5-10','10-15','15-30','>30')
  r.plot[,ccddt10 := cut(cddt10,breaks = pbreaks,labels = plabs)]
  
  p3 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
        geom_tile(data = r.plot,aes(x=x,y=y,fill= ccddt10)) +
        scale_fill_viridis_d(direction=-1)+ 
        theme_bw() +
        labs(fill = 'Change in Soil Health (%)\n per 1% increase of SOC') +
        theme(text = element_text(size = 12),
              legend.text=element_text(size=6),
              legend.title = element_text(size=8),
              legend.position = c(0.15,0.26),
              legend.key.size = unit(0.5,'cm'),
              legend.background = element_rect(fill = "white",color='white'),
              panel.border = element_blank(),
              plot.title = element_text(hjust = 0.5,size=14),
              plot.subtitle = element_text(hjust=0.5,size=10,face="italic"))+ 
        xlab("") + ylab("") +
        ggtitle("Change in Soil Health (%)",subtitle = 'per 1% change in SOC') +
        coord_sf(crs = 4326)
  ggsave(plot = p3, filename = 'D:/ESA/04 articles/2023/som_critical_levels/230506_socchange_global3_10.png',
         width = 20, height = 10, units = c("cm"), dpi = 1200)

  # soc current (g/kg)
  pbreaks <- c(0,5,10,15,30,1000)
  plabs <-  c('<5','5-10','10-15','15-30','>30')
  r.plot[,csoc := cut(soc,breaks = pbreaks,labels = plabs)]
  p4 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
        geom_tile(data = r.plot,aes(x=x,y=y,fill= csoc)) +
        scale_fill_viridis_d(direction=-1)+ 
        theme_bw() +
        labs(fill = 'Current SOC (g / kg)') +
        theme(text = element_text(size = 12),
              legend.text=element_text(size=6),
              legend.title = element_text(size=8),
              legend.position = c(0.15,0.26),
              legend.key.size = unit(0.5,'cm'),
              legend.background = element_rect(fill = "white",color='white'),
              panel.border = element_blank(),
              plot.title = element_text(hjust = 0.5,size=14),
              plot.subtitle = element_text(hjust=0.5,size=10,face="italic"))+ 
        xlab("") + ylab("") +
        ggtitle("Current SOC in topsoil (0-30cm)") +
        coord_sf(crs = 4326)
  ggsave(plot = p4, filename = 'D:/ESA/04 articles/2023/som_critical_levels/230508_currentsoc.png',
         width = 20, height = 10, units = c("cm"), dpi = 1200)
  
  d.bp <- r.plot[,.(id,current = soc,critical = copt)]
  d.bp <- melt(d.bp,id.vars='id',value.name='soc')  
  p4 <- ggplot(d.bp,aes(x = soc)) + 
        geom_density(data = d.bp[variable=='original'],fill='red',alpha=0.2) + 
        geom_density(data = d.bp[variable=='critical'],fill='blue',alpha=1)
  
# ---- step 10 quantifying cobenefits ----
  
  # relative change in SHI by 10 g C kg-1 extra C
  r.dfa.fin <- readRDS('D:/ESA/04 articles/2023/som_critical_levels/dbglobal.rds')
  r.plot <- r.dfa.fin[!is.na(cddt)]
  r.plot.d4 <- predcarbon(A_C_OF = r.plot$soc,
                          A_CLAY_MI = r.plot$clay,
                          A_SAND_MI = r.plot$sand,
                          A_PH_CC = r.plot$a_ph_cc,
                          type='all',dsoc = 10)
  r.plot2 <- dcast(r.plot.d4,id~shi,value.var='dist_to_max')
  r.plot[,id := .I]
  r.plot2 <- merge(r.plot[,.(x,y,id,croparea,A_C_OF = soc)],r.plot2,by='id')
  
  # estimate area weighted average
  cols <- colnames(r.plot2)[grepl('^i',colnames(r.plot2))]
  cols <- cols[!cols =='id']

  r.plot3a <- r.plot2[,lapply(.SD,function(x) weighted.mean(x,w = croparea)),.SDcols = cols]  
  r.plot3b <- r.plot2[,lapply(.SD,function(x) sqrt(Hmisc::wtd.var(x,weights = croparea,normwt = F))),.SDcols = cols] 
  r.plot3 <- data.table(shi = cols,dsoc = round(unlist(r.plot3a)*100,1),dsoc_sd = round(unlist(r.plot3b)*100,3))
r.plot3  
  