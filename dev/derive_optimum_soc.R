# derive ptf for optimum SOC levels

# for mineral soils only: 20% OS = 10% OC = 100 g/kg
dt1 <- data.table(A_C_OF = seq(0.5,100,1), A_CLAY_MI = 7.5, A_SAND_MI = 60)
dt1[,id := .I]

dt1.cec <- ptf_cec_all(dt1)
dt1.cec <- dt1.cec[,list(cec = mean(cec,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.cec,by='id')

dt1.wsa <- ptf_wsa_all(dt1)
dt1.wsa <- dt1.wsa[,list(wsa = mean(wsa,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.wsa,by='id')

# see hamel for interpretation optimum
dt1.mwd <- ptf_mwd_all(dt1)
dt1.mwd <- dt1.mwd[,list(mwd = mean(mwd,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.mwd,by='id')

# potentially mineralizable N
dt1.pmn <- ptf_pmn_all(dt1)
dt1.pmn <- dt1.pmn[,list(pmn = mean(pmn,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.pmn,by='id')

# hot water carbon
dt1.hwc <- ptf_hwc_all(dt1)
dt1.hwc <- dt1.hwc[,list(hwc = mean(hwc,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.hwc,by='id')

# derive water holding capacity = plant available water
dt1.whc <- ptf_whc_all(dt1)
dt1.whc <- dt1.whc[,list(paw = mean(whc,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.whc,by='id')

# pH buffer capacity
dt1.phbc <- ptf_phbc_all(dt1)
dt1.phbc <- dt1.phbc[,list(phbc = mean(phbc,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.phbc,by='id')

# carbon decomposition, minip
dt1.cdec <- ptf_cdec_all(dt1)
dt1.phbc <- dt1.phbc[,list(phbc = mean(phbc,na.rm=T)),by='id']
dt1 <- merge(dt1,dt1.phbc,by='id')

plot(dt1$phbc~dt1$A_C_OF)
