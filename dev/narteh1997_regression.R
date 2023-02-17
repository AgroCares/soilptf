# This script makes regression model to predict PMN for Narteh and Sahrawat (1997)
# (I used the calculated regression coefficients for sptf_pmn1, because the reported coefficient values in the paper seems incorrect.)

# Narteh and Sahrawat (1997) Potentially Mineralizable Nitrogen in West African Lowland Rice Soils. Geoderma, 76, 145-154

library(data.table); library(ggplot2)

# onedrive directory name
fdnm = "SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI"
iloc_onedrive <- paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")), "/", fdnm, "/")
projectdr <- paste0(iloc_onedrive, 
                    'NMISite - Documents (1)/Projecten/O 1800 - O 1900/1819.N.20 PPS Beter Bodembeheer/04 rapportage/06 note pedotransfer/')

# Load data of Table 2
tb_narteh <- as.data.table(read_xlsx(paste0(projectdr, "sptf_PMN.xlsx"),
                                   sheet = "narteh_1997", skip = 3))



# calculate C:N ratio
tb_narteh[, CN := OrgC / (totalN/1000)]

# linear regression
summary(lm(Nmin ~ OrgC + Fe_DTPA + pH + Clay + CN + totalN, data = tb_narteh))

# linear regression 2
summary(lm(Nmin ~ Fe_DTPA + pH + Clay, data = tb_narteh))
