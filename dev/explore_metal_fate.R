# relations to assess critical soil metal concentrations

require(data.table)

# initial concentrations (mg/kg)
dt <- CJ(metal = c('a_cd_rt','a_pb_rt','a_cu_rt','a_zn_rt'),
         soiltype = c('sand','clay','peat'))
dt[metal == 'a_cd_rt', mconc := c(0.32,0.44,0.19)]
dt[metal == 'a_pb_rt', mconc := c(31,52,16)]
dt[metal == 'a_cu_rt', mconc := c(16,21,6.8)]
dt[metal == 'a_zn_rt', mconc := c(72,69,19)]

# add some default soil properties
dt[, a_ph_kcl := 4.8]
dt[, a_ph_cc := a_ph_kcl * 0.82 + 1.79]
dt[, a_som_loi := 4.3]
dt[, a_clay_mi := 3]

# dcast data
dt <- dcast(dt,soiltype + a_ph_kcl + a_som_loi + a_clay_mi ~metal, value.var = 'mconc')
dt[,crop := 'maize']

d0 <- copy(dt)

# estimate concentraiton in plant (mg/kg) from total concentration in soil (mg/kg)
cplant <- function(crop,soiltype, a_ph_kcl,a_som_loi,a_clay_mi,a_zn_rt = NA_real_,a_cu_rt= NA_real_,a_pb_rt= NA_real_,a_cd_rt= NA_real_){
  
  d0 <- data.table(a_ph_kcl = a_ph_kcl,
                   a_som_loi = a_som_loi,
                   a_clay_mi = a_clay_mi,
                   a_zn_rt = a_zn_rt,
                   a_cu_rt = a_cu_rt,
                   a_pb_rt = a_pb_rt,
                   a_cd_rt = a_cd_rt,
                   value = NA_real_
                   )
  
  # values for cadmium
  d1.cd <- data.table(crop = c('grass','maize','sugarbeet','wheat','lettuce'),
                      a0 = c(1.45,0.90,1.33,0.22,2.55),
                      a1 = c(-0.38,-0.21,-0.22,-0.12,-0.33),
                      a2 = c(0,0,0,-0.33,-0.39),
                      a3 = c(0,-0.32,-0.13,-0.04,-0.19), 
                      n = c(1.22,1.08,0.62,0.62,0.85),
                      metal = 'cd')
  
  d0 <- merge(d0, d1, by= 'crop')
  
  # estimate plant Cd concentration (mg/kg)
  d0[,plant_cd := 10^(a0 + a1 * a_ph_kcl + a2 * log10(a_som_loi) + a3 * log10(a_clay_mi) + n * log10(a_cd_rt))]
  
  # food quality criteria (mg /kg dw)
  fqc <- data.table(crop = c('grass','maize','sugarbeet','wheat','lettuce'),
                    cd_crt = c(1.1,1.1,1.1,0.24,4))
  
}

# estimate concentraiton for ecological health from total concentration in soil (mg/kg)
shi_metals <- function(A_PH_CC,A_SOM_LOI,A_CLAY_MI,
                       a_zn_rt = NA_real_,a_cu_rt= NA_real_,a_pb_rt= NA_real_,a_cd_rt= NA_real_,
                       type = 'soc_crit'){
  
  # collect data internal db.
  d0 <- data.table(id = 1:length(A_SOM_LOI),
                   A_PH_KCL = (A_PH_CC - 0.5262)/0.9288,
                   a_som_loi = A_SOM_LOI,
                   a_clay_mi = A_CLAY_MI,
                   a_zn_rt = a_zn_rt,
                   a_cu_rt = a_cu_rt,
                   a_pb_rt = a_pb_rt,
                   a_cd_rt = a_cd_rt,
                   value = NA_real_)
  
  # add pH values
  d0[,a_ph_cc := A_PH_KCL * 0.82 + 1.79]
  d0[,a_ph_kcl := A_PH_KCL]
  
  # add metals
  d1 <- CJ(metal = c('a_cd_rt','a_pb_rt','a_cu_rt','a_zn_rt'),
           id = 1:length(A_SOM_LOI))
  d1 <- merge(d1,d0,by='id')
  
  # values for estimating critical reactive metal concentration (b0 to b2, Table 8, de Vries et al., 2008)
  # values for estimating critical total metal concentration from reactive metal concentration (c0 to c3, Table 9, de Vries et al., 2008)
  # background concentrations (d0 to d3, table 10)
  # freundlich coefficients (e0 to e4, table 11)
  # topsoil concentration Aqua Regio (mg/kg) of agricultural soils (f0 to f3, table 12)
  # subsoil concentration Aqua Regio (mg/kg) of agricultural soils (g0 to g5, table 13)
  d1.mp <- data.table(metal = c('a_cd_rt','a_pb_rt','a_cu_rt','a_zn_rt'),
                      b0 = c(-2.27,0.58,0.26,-0.74),
                      b1 = c(0.33,0.11,0.02,0.14),
                      b2 = c(1.00,0.66,0.68,1.07),
                      c0 = c(0.028,0.323,0.318,0.614),
                      c1 = c(0.877,0.810,0.761,0.753),
                      c2 = c(0.009,0.035,0.044,-0.107),
                      c3 = c(0.081,0.136,0.191,0.275),
                      d0 = c(-1.919,0.443,-0.142,0.33),
                      d1 = c(0.418,0.469,0.481,0.402),
                      d2 = c(0.186,0.267,0.594,0.425),
                      d3 = c(0.059,0,0,0.076),
                      f0 = c(-1.152,0.407,0.799,0.757),
                      f1 = c(0.380,0.420,0.456,0.327),
                      f2 = c(0.138,0.228,0.251,0.366),
                      f3 = c(0.033,0.026,0,0.056),
                      g0 = c(-0.496,0.364,1.141,0.343),
                      g1 = c(-0.354,-0.351,-0.56,-0.111),
                      g2 = c(0.382,0.102,0.191,0.146),
                      g3 = c(0.174,0.372,0.312,0.32),
                      g4 = c(0.261,0.65,0.369,0.466),
                      g5 = c(0.011,0.009,0.014,0.071),
                      e0 = c(-0.91,0.78,0.41,-1.04),
                      e0_int = c(-4.85,-2.96,-3.55,-4.41),
                      e1 = c(0.58,0.83,0.48,0.39),
                      e2 = c(0.28,0.02,0.18,0.35),
                      e3 = c(0.27,0.25,0.16,0.45),
                      e4 = c(-0.05,-0.23,-0.27,-0.14),
                      n = c(0.54,0.68,0.47,0.74))
  
  # critical limits for groundwater and surface water
  d1.target <- data.table(metal = c('a_cd_rt','a_pb_rt','a_cu_rt','a_zn_rt'),
                          tv_nl = c(0.4,15,15,65),
                          eu_rar = c(0.19,9.5,7.8,8.8),
                          mtr_nl = c(0.4,11,1.5,9.4))
  
  # merge
  d0 <- merge(d1, d1.mp, by= 'metal')
  d0 <- merge(d0, d1.target,by ='metal')

  # estimate critial reactive metal concentration (HNO3) in reactive form (mg kg-1), pH is pH soil solution
  d0[,m_crit_reactive := 10^(b0 + b1 * a_ph_cc + b2 * log10(a_som_loi))]
  
  # estimate critical total metal concentration (mg/kg)
  d0[,m_crit_total := 10^(c0 + c1 * log10(m_crit_reactive) + c2 * log10(a_som_loi) + c3 * log10(a_clay_mi))]
  
  # estimate background concentrations (mg/kg)
  d0[,m_background := 10^(d0 + d1 * log10(a_som_loi) + d2 * log10(a_clay_mi) + d3 * log10(a_ph_kcl))]
  
  # estimate metal concentration of topsoil from agricultural sites
  d0[,m_topsoil := 10^(f0 + f1 * log10(a_som_loi) + f2 * log10(a_clay_mi) + f3 * a_ph_kcl)]
  
  # what is critical SOM so that current metal concentration stays below critical one 
  # assuming optimum pH for normal crops (SHI, pH_water of 6 = 5.45 pH-KCL = 6.3 ph_ss as used here) )
  # calculated via ((7-1.3235) / 0.8581)*0.82+1.79 = 7.2
  
  d0[,a_som_crit := 10^((log10(m_topsoil) - c3 * log10(a_clay_mi) - c0 - c1 * (b0 + b1 * (6.25)))/(c1 * b2 + c2))]
  
  # SOC critical
  d0[, a_soc_crit := a_som_crit * 10 *0.5]
  
  if(FALSE){
    
    # estimate DOC (Romkens, 2004) given SOM, pH and solid-solution rate
    d0[, ssr := 1.45]
    d0[, a_c_cc := 10^(2.66 + 0.7 * log10(a_som_loi) - 0.15 * a_ph_cc + 1.52 * log10(ssr))]
    
    # estimate kf
    d0[, kf := 10^(e0 + e1 * log10(a_som_loi) + e2 * log10(a_clay_mi) + e3 * a_ph_cc + e4 * log10(a_c_cc))]
    
    # estimate reactive metal content
    d0[, m_topsoil_reactive := 10^((log(m_topsoil) - c0 - c2 * log10(a_som_loi) - c3 * log10(a_clay_mi))/c1)]
    
    # estimate kf_critical
    d0[,kf_crit := m_topsoil_reactive / (eu_rar*0.001)^n]
    
    # derive critical a_som_loi
    d0[, som_crit_leaching := 10^((log10(kf_crit) - e0 - e2 * log10(a_clay_mi) - e3 * a_ph_cc - e4 * log10(a_c_cc))/e1)]
  }
  
  if(type == 'soc_crit') {d1 <- dcast(d0, id~metal,value.var='a_soc_crit')}
  if(type == 'me_crit') {
    d1 <- dcast(d0, id~metal,value.var='m_crit_reactive')
    setnames(d1,c('id', 'ccd','ccu','cpb','czn'))
    }
  
  # return
  return(d1)
  
}


