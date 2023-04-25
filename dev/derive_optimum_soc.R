# derive ptf for optimum SOC levels

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
l.bd <- paste('CEC == ', '2125 -1.056 * C - 257 * log(C)')

# the cation exchange capacity
dt1.cec <- ptf_cec_all(dt1)
dt1.cec <- dt1.cec[,list(cec.mean = mean(cec,na.rm=T),
                         cec.se = sd(cec,na.rm=T)/sqrt(sum(!is.na(cec)))),by='id']
dt1 <- merge(dt1,dt1.cec,by='id')
m.cec <- lm(cec.mean~A_C_OF + I(A_C_OF^2),data=dt1)
p.cec <- predict(m.cec,newdata = data.frame(A_C_OF = dt1$A_C_OF))
l.cec <- paste('CEC == ', '64.9 + 2.92 * C - 0.0019 * C^2')

# water stable aggregates
dt1.wsa <- ptf_wsa_all(dt1)
dt1.wsa <- dt1.wsa[,list(wsa.mean = mean(wsa,na.rm=T),
                         wsa.se = sd(wsa,na.rm=T)/sqrt(sum(!is.na(wsa)))),by='id']
dt1 <- merge(dt1,dt1.wsa,by='id')
m.wsa <- lm(wsa.mean~A_C_OF + I(A_C_OF^2),data=dt1)
p.wsa <- predict(m.wsa,newdata = data.frame(A_C_OF = dt1$A_C_OF))
l.wsa <- paste('WSA == ', '64.2 + 1.04 * C - 0.0019 * C^2')

# see hamel for interpretation optimum
dt1.mwd <- ptf_mwd_all(dt1)
dt1.mwd <- dt1.mwd[,list(mwd.mean = mean(mwd,na.rm=T),
                         mwd.se = sd(mwd,na.rm = T)/sqrt(sum(!is.na(mwd)))),by='id']
dt1 <- merge(dt1,dt1.mwd,by='id')
m.mwd <- lm(mwd.mean~A_C_OF,data=dt1)
p.mwd <- predict(m.mwd,newdata = data.frame(A_C_OF = dt1$A_C_OF))
l.mwd <- paste('MWD == ', '0.816 + 0.02634 * C')

# soil shearing
dt1.sss <- ptf_sss_all(dt1)
dt1.sss <- dt1.sss[,list(sss.mean = mean(sss,na.rm=T),
                         sss.se = sd(sss,na.rm = T)/sqrt(sum(!is.na(sss)))),by='id']
dt1 <- merge(dt1,dt1.sss,by='id')
m.sss <- lm(sss.mean~A_C_OF + I(A_C_OF^2),data=dt1)
p.sss <- predict(m.sss,newdata = data.frame(A_C_OF = dt1$A_C_OF))
l.sss <- paste('SSS == ', '66.62 + 0.526 * C + 0.0024 * C^2')

# derive water holding capacity
dt1.whc <- ptf_whc_all(dt1)
dt1.whc <- dt1.whc[,list(whc.mean = mean(whc,na.rm=T),
                         whc.se = sd(whc,na.rm = T)/sqrt(sum(!is.na(whc)))),by='id']
dt1 <- merge(dt1,dt1.whc,by='id')
m.whc <- lm(whc.mean~I(A_C_OF^0.5),data=dt1)
p.whc <- predict(m.whc,newdata = data.frame(A_C_OF = dt1$A_C_OF))
l.whc <- paste('WHC == ', '0.4228 + 0.01582 * C^0.5')

# derive water plant available water
dt1.paw <- ptf_paw_all(dt1)
dt1.paw <- dt1.paw[,list(paw.mean = mean(paw,na.rm=T),
                         paw.se = sd(paw,na.rm = T)/sqrt(sum(!is.na(paw)))),by='id']
dt1 <- merge(dt1,dt1.paw,by='id')
m.paw <- lm(paw.mean~ I(A_C_OF^0.5),data=dt1)
p.paw <- predict(m.paw,newdata = data.frame(A_C_OF = dt1$A_C_OF))
l.paw <- paste('PAW == ', '0.135 + 0.01193 * C^0.5')

# potentially mineralizable N
dt1.pmn <- ptf_pmn_all(dt1)
dt1.pmn <- dt1.pmn[,list(pmn.mean = mean(pmn,na.rm=T),
                         pmn.se = sd(pmn,na.rm=T)/sqrt(sum(!is.na(pmn)))),by='id']
dt1 <- merge(dt1,dt1.pmn,by='id')
m.pmn <- lm(pmn.mean~ A_C_OF+I(A_C_OF^2),data=dt1)
p.pmn <- predict(m.pmn,newdata = data.frame(A_C_OF = dt1$A_C_OF))
l.pmn <- paste('PMN == ', '9.119 + 1.284 * C + 0.0102 * C^2')

# hot water carbon
dt1.hwc <- ptf_hwc_all(dt1)
dt1.hwc <- dt1.hwc[,list(hwc.mean = mean(hwc,na.rm=T),
                         hwc.se = sd(hwc,na.rm = T)/sqrt(sum(!is.na(hwc)))),by='id']
dt1 <- merge(dt1,dt1.hwc,by='id')
m.hwc <- lm(hwc.mean~ A_C_OF+I(A_C_OF^2),data=dt1)
p.hwc <- predict(m.hwc,newdata = data.frame(A_C_OF = dt1$A_C_OF))
l.hwc <- paste('HWC == ', '129.9 + 29.88 * C -0.036918 * C^2')

# pH buffer capacity
dt1.phbc <- ptf_phbc_all(dt1)
dt1.phbc <- dt1.phbc[,list(phbc.mean = mean(phbc,na.rm=T),
                           phbc.se = sd(phbc,na.rm=T)/sqrt(sum(!is.na(phbc)))),by='id']
dt1 <- merge(dt1,dt1.phbc,by='id')
m.phbc <- lm(phbc.mean~ A_C_OF,data=dt1)
p.phbc <- predict(m.phbc,newdata = data.frame(A_C_OF = dt1$A_C_OF))
l.phbc <- paste('pHBC == ', '1.669 + 1.286 * C')

# metal freundlich coefficient
dt1.metal <- ptf_metals_all(dt1)
dt1.metal <- dt1.metal[,list(metal.mean = mean(metal,na.rm=T),
                             metal.se = sd(metal,na.rm=T)/sqrt(sum(!is.na(metal)))),by='id']
dt1 <- merge(dt1,dt1.metal,by='id')
m.metal <- lm(metal.mean~ I(A_C_OF^0.5),data=dt1)
p.metal <- predict(m.metal,newdata = data.frame(A_C_OF = dt1$A_C_OF))
l.metal <- paste('K-freundlich == ', '-4.01 + 10.2824 * C^0.5')

# carbon decomposition, minip, and assume 10% uncertainty
dt1.cdec <- ptf_cdec_all(dt1)
dt1.cdec <- dt1.cdec[,list(cdec.mean = mean(cdec,na.rm=T),
                           cdec.se = mean(cdec,na.rm=T)*0.10),by='id']
dt1 <- merge(dt1,dt1.cdec,by='id')
m.cdec <- lm(cdec.mean~A_C_OF,data=dt1)
p.cdec <- predict(m.cdec,newdata = data.frame(A_C_OF = dt1$A_C_OF))
l.cdec <- paste('C-decomposition == ', '0.22 * C')

dt1[, cyield := sptf_yield1(A_C_OF = A_C_OF,A_CLAY_MI = A_CLAY_MI)]
m.cyield <- lm(cyield~A_C_OF + I(A_C_OF^2),data=dt1)

# optimum crop yield (see Young et al., 2021)
# here is the target: dt1[, cyield1 := 0.5 + (1.75 - 0.5) * (A_CLAY_MI - 4)/(38 - 4)]
# model of Oldfield, 118 kg N/ha default

# fit a model
fmod <- function(model,var){
  
  nd <- data.table(A_C_OF = seq(0,100,0.01))
  nd[, p1 := predict(model,newdata = nd)]
  nd[,diff := abs(p1-var)]
  nd <- nd[diff == min(diff)[1]]
  return(nd[,A_C_OF])
}

# optima for soil functions
dt2 <- dt1[1,.(id,A_C_OF,A_CLAY_MI)]

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
  
  # more is better, optimum score at 0.3 g / g (Moebius-Clune, 2017)
  dt2[, owhc := 0.3]
  dt2[, cwhc := fmod(m.whc,0.3),by=id]
  
  # more is better, optimum score at 0.3 g / g (Moebius-Clune, 2017)
  dt2[, opaw := 0.3]
  dt2[, cpaw := fmod(m.paw,0.3),by=id]
  
  # more is better, optimum score at 30 mg N /kg (Moebius-Clune, 2017)
  dt2[, opmn := 30]
  dt2[, cpmn := fmod(m.pmn,30),by=id]
  
  # more is better, optimum score at 900 mg C /kg (Moebius-Clune, 2017)
  dt2[, ohwc := 900]
  dt2[, chws := fmod(m.hwc,900),by=id]
  
  # optimum pH buffer capacity
  dt2[, ophbc := NA_real_]

  # optimum C decomposition is not higher than what can be added by annual grassland
  # kg EOS * 0.5 = kg C / ha
  dt2[, ocdec := 3975 * 0.5 *1000 * 10/ ((2125 - 1.056 * A_C_OF - 257 * log(A_C_OF))*100*100*0.3)]
  dt2[, ccdec := fmod(m.cdec,ocdec),by=id]
  
  # optimum metal sorption, by default the highest one
  dt2[, ometals := 100]
  dt2[, cmetals := fmod(m.metal,100),by=id]
  
  # optimum crop yield based on Oldfield et al. (2009)
  dt2[, oyield := 3]
  dt2[, cyield := fmod(m.cyield,3),by=id]
  
  # melt
  dt3 <- melt(dt2,
              id.vars = c('id','A_C_OF','A_CLAY_MI'),
              measure=patterns(target="^o", copt = "^c"))
  vars <- c('dens','cec','wsa','mwd','sss','whc','paw','pmn','hwc','ophbc','cdec','metals','yield')
  dt3[,parm := vars[variable]]
  
  
  
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
      xlab('organic carbon content (g/kg)') + ylab('Kf') + theme_bw() +
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
