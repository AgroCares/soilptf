# derive ptf for optimum SOC levels
# Ros, 27 april 2023

# require packages
require(data.table); require(soilptf)
require(terra);require(sf)
require(ggplot2);require(patchwork)

# --- step 1. develop meta-ptfs ----
# make a virtual dataset for a sandy soil varying in SOC levels, and derive optimum meta-ptfs per soil function
  
  # helper fun to extract coefficients from lm model
  hpf <- function(tpr,m1,r,cfin){
    
  vals <- coefficients(m1)
  pr <- paste(tpr," == ",round(vals[1],r[1]))
  for(i in 2:length(vals)){ pr <- paste(pr,
                                        fifelse(vals[i]<0,'-','+'),
                                        round(abs(vals[i]),r[i]),
                                        cfin[i-1])}
  return(pr)}

  # for mineral soils only: 20% OS = 10% OC = 100 g/kg
  dt1 <- data.table(A_C_OF = seq(0.5,100,1), A_CLAY_MI = 7.5, A_SAND_MI = 60, A_PH_CC = 5.4)
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
  l.metal <- paste('Kf == ', '-4.01 + 10.2824 * C^0.5')
  
  # carbon decomposition, minip, and assume 10% uncertainty
  dt1.cdec <- ptf_cdec_all(dt1)
  dt1.cdec <- dt1.cdec[,list(cdec.mean = mean(cdec,na.rm=T),
                             cdec.se = mean(cdec,na.rm=T)*0.10),by='id']
  dt1 <- merge(dt1,dt1.cdec,by='id')
  m.cdec <- lm(cdec.mean~A_C_OF-1,data=dt1)
  p.cdec <- predict(m.cdec,newdata = data.frame(A_C_OF = dt1$A_C_OF))
  l.cdec <- paste('C-decomposition == ', '0.22 * C')
  
  dt1[, cyield := sptf_yield1(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI)]
  m.cyield <- lm(cyield~A_C_OF + I(A_C_OF^2),data=dt1)

# optimum crop yield (see Young et al., 2021)
# here is the target: dt1[, cyield1 := 0.5 + (1.75 - 0.5) * (A_CLAY_MI - 4)/(38 - 4)]
# model of Oldfield, 118 kg N/ha default

# --- step 2. define optimum SOC ----
# here an example is given for the dataset prepared above, and its converted to a simple function to apply this procedure on new datasets.
 # note that this function is not yet optimized for speed
  
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
  optimcarbon_fix <- function(xs, dtr){
    
    # make internal data.table  
    dt.oc <- copy(dtr)[xs]
    
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
    #dt.oc[, opaw := pmax(0.18,0.21 + 1:100 * (0.18 - 0.21)/50)]
    dt.oc[, opaw := pmax(0.13,0.16 + 1:100 * (0.13 - 0.16)/50)]
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
                     copt_oa = max(copt),
                     copt_median = median(copt),
                     copt_sd = sd(copt)),by=id]
    
    out <- dt5[,copt_av]
    
    # return out
    return(out)
  }
  
# --- step 4. plot figures of meta-ptfs ----
  
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
  
  require(patchwork)
  
  pfin <- (p1 | p2 | p3 | p4) / (p5 | p6 | p7 | p8) / (p9 | p10 | p11 | p12) 
  ggsave(filename = "D:/ESA/04 articles/2023/som_critical_levels/230427_ptfs.png",
         plot = pfin, width = 65, height = 40, units = c("cm"), dpi = 1200)


# --- step 5. apply this approach on integrator dataset ----

  # load an earlier prepared dataset
  db.int <- readRDS('D:/ESA/04 articles/2023/som_critical_levels/dbint.rds')
  
  if(FALSE){
  
    # load in INTEGRATOR data and select only relevant data
    db.int <- fread('D:/ESA/04 articles/2023/som_critical_levels/db_final_europe.csv')
    db.int <- db.int[,.(ncu,crop_name,area_ncu,clay,ph,cn,soc_ref)]
    db.int <- db.int[,.(area = sum(area_ncu),A_CLAY_MI = clay[1],ph = ph[1],cn = cn[1],A_C_OF = soc_ref[1]*10),by=ncu]
    
    # apply the function multi-session (since function is not yet optimized)
    require(future.apply); require(progressr)
    plan(multisession)
    
    # Run the schedule for farm evaluation
    runcoptmodel <- function(xs) {
      p <- progressor(along = xs, enable = TRUE)
      dt <- future_lapply(xs, optimcarbon, dtr = db.int)
    }
    out <- runcoptmodel(1:nrow(db.int))
    
    # combine all outputs into one vector and add to original data.table
    out <- unlist(out)
    db.int[,copt := out]
    saveRDS(db.int,'D:/ESA/04 articles/2023/som_critical_levels/dbint.rds')
    
  }
  

# --- step 6. plot the EU results ----
  
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
            legend.position = 'right',
            legend.background = element_rect(fill = "white",color='white'),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      ggtitle(ftitle)
  }

  # make and save the plot
  pm1 <- visualize(raster = r.fin,
                 layer = 'copt_dif',
                 name = "Distance to Copt\n(g/kg)",
                 breaks = c(-1000,0,5,10,50),
                 labels = c('< 0','0-5','5-10','>10'),
                 ftitle = 'Distance to optimum soil C content (g/kg)')
  ggsave(filename = "D:/ESA/04 articles/2023/som_critical_levels/230427_eu_copt.png",
         plot = pm1, width = 25, height = 25, units = c("cm"), dpi = 1200)

