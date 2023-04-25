# derive ptf for optimum SOC levels

# for mineral soils only: 20% OS = 10% OC = 100 g/kg
dt1 <- data.table(A_C_OF = seq(0.5,100,1), A_CLAY_MI = 7.5, A_SAND_MI = 60, A_PH_CC = 5.4)
dt1[,id := .I]
set.seed(124)
dt1[,A_CN_FR := rnorm(.N,12,2)]
dt1[,A_N_RT := A_C_OF * 1000 / A_CN_FR]

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
dt1.hwc <- dt1.hwc[,list(hwc = mean(hwc,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.hwc,by='id')



# pH buffer capacity
dt1.phbc <- ptf_phbc_all(dt1)
dt1.phbc <- dt1.phbc[,list(phbc = mean(phbc,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.phbc,by='id')

# carbon decomposition, minip
dt1.cdec <- ptf_cdec_all(dt1)
dt1.cdec <- dt1.cdec[,list(cdec = mean(cdec,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.cdec,by='id')

# metal freundlich coefficient
dt1.metal <- ptf_metals_all(dt1)
dt1.metal <- dt1.metal[,list(metal = mean(metal,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.metal,by='id')

# optimum crop yield (see Young et al., 2021)
# here is the target: dt1[, cyield1 := 0.5 + (1.75 - 0.5) * (A_CLAY_MI - 4)/(38 - 4)]
# model of Oldfield, 118 kg N/ha default


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

