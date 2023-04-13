# this script analyzes behaviours of different PTFs, using a test dataset

library(data.table)
library(readxl)
library(ggplot2); library(viridis); library(ggpubr)

# onedrive directory name
fdnm = "SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI"
iloc_onedrive <- paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")), "/", fdnm, "/")
projectdr <- paste0(iloc_onedrive, 
                    'NMISite - Documents (1)/Projecten/O 1800 - O 1900/1819.N.20 PPS Beter Bodembeheer/04 rapportage/06 note pedotransfer/')


source('R/sptf_tables.R') 
source('R/sptf_textureclass.R') 
source('R/sptf_predict.R') 
source('R/bulkdensity.R') 
source('R/whc.R')
source('R/pmn.R')

##  Make a test set -------

testdtfnm <- paste0(projectdr, 'testdata/testdata.rds')  

if(!file.exists(testdtfnm)){
  ## Make a test set by andomly chosing 1000 parcels from all parcels of the Netherlands
  # load OBIC input data of all parcels in the Netherlands (used for OBI paper)
  iloc_save <- paste0(iloc_onedrive, "Sven Verweij - NMI-DATA/project/OBIC/brp2019/")
  dtall <- readRDS(file=paste0(iloc_save,'OBIC_input_brp2019NL.rds')) # not-aggregated
  dtall2019 <- dtall[YEAR == 2019,]
  # randomly select 1000 records of year 2019
  seeds(123)
  dt <- dtall2019[sample(1:nrow(dtall2019), 1000), ]
  # save test set
  saveRDS(dt, file = testdtfnm)
} else {
  dt <- readRDS(testdtfnm)
}


setnames(dt, "ID", "id")
# add standard depth
dt[, B_DEPTH := 0.3]

# # temp test
# # make internal table
# dt <- data.table(id = 1: 40,
#                  A_SOM_LOI = seq(0.1,20,0.5),
#                  A_CLAY_MI = 7.5,
#                  A_SAND_MI = 60,
#                  B_DEPTH = 0.3,
#                  B_LOC_COUNTRY = NA,
#                  B_LU_PTFCLASS = NA
# )


# BD ----------------------------
## Estimate BD with all PTFs ----

# fill-in necessary input values for some PTF's
dt[, B_SLOPE_DEGREE := 0]
dt[, B_SLOPE_ASPECT := 0]
dt[, B_ALTITUDE := 0]
dt[, A_PH_WA := A_PH_CC]
dt[, A_CACO3_MI := A_CACO3_IF]

dt3 <- ptf_bd_all(dt)  # --> obtained values includes strange values (-21675.11  64435.66)! To be checked
# # dt2 <- merge(dt2,ptf.mods,by = 'ptf_id')

# add attributes of PTFs
ptf.mods <- as.data.table(soilptf::sptf_bulkdensity)

# flag not relevant PTF's for dutch agricultural soils
ptf.mods[, outside_eu := 0]
ptf.mods[!continent_code %in% c("EU", ""), outside_eu := 1]
ptf.mods[, not_agriculture := 0]
ptf.mods[landuse %in% c("forest", "nature"), not_agriculture := 1]
ptf.mods[, flag := "relevant"]
ptf.mods[outside_eu == 1 | not_agriculture == 1, flag := "not relevant"]

dt3 <- merge(dt3, ptf.mods[, .(ptf_id, country_code, continent_code, soiltype, landuse, flag)],
             by = "ptf_id", all.x = T)

# Check strange values
nneg <- dt3[value < 0, .N, by = ptf_id]
n2500 <- dt3[value > 2500, .N, by = ptf_id]
nna <- dt3[is.na(value), .N, by = ptf_id]
# 10 PTF's need not-standard input values. That are: 
# 25: A_H2O_T105, 70: B_ALTITUDE,B_SLOPE_DEGREE,B_SLOPE_ASPECT, 
# 71: B_SLOPE_DEGREE, 78: A_PH_WA, 79: A_PH_WA, 80: A_CACO3_MI
# 88: A_CACO3_MI,A_PH_WA, 113: A_PH_WA, 137: A_PH_WA, 154: A_PH_WA



## Scatter plot SOM vs BD -------

## Fitterd line only
ggplot(dt3[flag == "relevant"], aes(x = A_SOM_LOI, y = value, 
                #col = as.factor(flag))) +
                col = as.factor(ptf_id))) +
  geom_smooth(method = "loess", se = FALSE) + 
  ylim(c(0,3000)) + ylab("Bulk density (kg / m3)") + xlab("SOM (%)") +
  theme_minimal() +
  theme(legend.position = "none")
#ggsave(file = paste0(projectdr, "figs/bd_vs_som_perptf_fittedlines.jpg"), bg = "white", width = 5, height = 5)
  

# All points with fitted line 
# prediction interval
dt4 <- dt3[!is.na(value) & !is.na(A_SOM_LOI) & flag == "relevant",]
#model1 <- lm(value ~ A_SOM_LOI, data=dt4) #-> this should be nonlinear model
model1 <- lm(value ~ poly(A_SOM_LOI, 2), data=dt4)
temp_var <- predict(model1, interval="prediction")
dt4 <- cbind(dt4, temp_var)

ggplot(dt4,
       aes(x = A_SOM_LOI, y = value)) +
  # add not-relevant points
  geom_point(data = dt3[!is.na(value) & !is.na(A_SOM_LOI) & flag != "relevant"],
             aes(x = A_SOM_LOI, y = value), size = 0.5, col = "gray") +
  geom_point(size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "blue", alpha = .4) + 
  #geom_smooth(method = "loess", se=TRUE,  level=0.95) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)+
  ylim(c(-1000,5000)) + ylab("Bulk density (kg / m3)") + xlab("SOM (%)") +
  theme_minimal() 
#ggsave(file = paste0(projectdr, "figs/bd_vs_som_all.jpg"), bg = "white", width = 5, height = 5)


## 5-95% of each interval
# make intervals of SOM
dt3[, SOM_int := cut(A_SOM_LOI, breaks = seq(min(A_SOM_LOI), max(A_SOM_LOI), length.out = 100),
                     labels = seq(min(A_SOM_LOI), max(A_SOM_LOI), length.out = 100)[-1])]
# median and percentiles of each SOM interval
sum_int <- dt3[flag == "relevant", .(med = median(value, na.rm = T),
                   q5 = quantile(value, probs = 0.05, na.rm = T),
                   q95 = quantile(value, probs = 0.95, na.rm = T),
                   N = .N), by = SOM_int]
sum_int[, SOM_int := as.numeric(as.character(SOM_int))]

ggplot(sum_int) +
  # 5-95th percentile
  geom_segment(aes(x = SOM_int, xend = SOM_int, y = q5, yend = q95)) +
  # median
  geom_line(aes(x = SOM_int, y = med), col = "red") +
  # # fitted line
  # geom_smooth(data = dt4, aes(x = A_SOM_LOI, y = value),
  #             method = "lm", formula = y ~ poly(x, 2), se = FALSE)+
  xlim(c(0, max(dt3$A_SOM_LOI))) + ylim(c(0,2000)) + 
  xlab("SOM (%)") + ylab("Bulk density (kg / m3)") +
  theme_minimal() 
#ggsave(file = paste0(projectdr, "figs/bd_vs_som_interval.jpg"), bg = "white", width = 5, height = 5)


## variation in BD by differnt PTF --------

# quantify variation
iqr <- dt3[value > 0, 
              .(q1 = quantile(value, probs = 0.25, na.rm = T),
                q3 = quantile(value, probs = 0.75, na.rm = T),
                q2 = quantile(value, probs = 0.5, na.rm = T)), by = id]
# interquartile range (IQR)
iqr[, iqr := q3 - q1]
# quartile coefficient of dispersion (QCD) 
iqr[, qcd := iqr / (q3 + q1)]
iqr <- merge(iqr, dt[, .(id, B_SOILTYPE_AGR, B_LU_BRP, A_SOM_LOI, A_CLAY_MI)], 
             by = "id", all.x = T)

# histogram QCD per soil type
ggplot(iqr) + 
  geom_histogram(aes(x = qcd)) + facet_grid(B_SOILTYPE_AGR ~ ., scale = "free_y") +
  theme_minimal() 

# som vs clay vs QCD-WCH
gp1 <- ggplot(iqr) + 
  geom_point(aes(x = A_SOM_LOI, y = A_CLAY_MI, col = qcd)) +
  labs(col  = "QCD BD") + xlab("SOM (%)") + ylab("Clay (%)") + 
  scale_color_viridis() +
  theme_minimal() 

# som vs clay vs median-WHC
gp2 <- ggplot(iqr) + 
  geom_point(aes(x = A_SOM_LOI, y = A_CLAY_MI, col = q2)) +
  labs(col  = "median BD") + xlab("SOM (%)") + ylab("Clay (%)") + 
  scale_color_viridis() +
  theme_minimal() 

ggpubr::ggarrange(gp2, gp1)
#ggsave(file = paste0(projectdr, "figs/bd_vs_som_vs_clay_median_qcd.jpeg"),bg = "white", width = 8, height = 3)



## check types of Bulk density PTF's -----
ptf.mods <- as.data.table(soilptf::sptf_bulkdensity)

# count PTF's per continent
ptf.mods[continent_code == "", continent_code := "variable"]
ptf.mods[continent_code == "AF||AS||SA", continent_code := "variable"]
table(ptf.mods$continent_code)

# count PTF's per landuse
ptf.mods[is.na(landuse), landuse := "variable"]
table(ptf.mods$landuse)

# count input parameters
ptf.mods[grepl("Ia_silt_mi", soilproperties), soilproperties := gsub("I", "|", soilproperties)]
#ptf.mods[, soilproperties := gsub("\\a_sand|mi", "a_sand_mi", soilproperties)]
cols <- paste0("in", 1:7)
ptf.mods2 <- ptf.mods[, (cols) := tstrsplit(soilproperties, '||', fixed=TRUE)][]
ptf.mods2 <- melt(ptf.mods2, id.vars = c("ptf_id"), measure.vars = cols)
ptf.mods2 <- ptf.mods2[!is.na(value)]
# nr input parameters per PTF
input_nr <- ptf.mods2[, .N, by = ptf_id]
table(input_nr$N)
# nr PTF per input parameter
tb <- ptf.mods2[, .N, by = value]
tb <- tb[order(N, decreasing = T)]

# # estimate BD (median and SD) 
# cols <- c("id", "bd.mean", "bd.sd")
# dt2 <- copy(dt)
# dt2[, (cols) := ptf_bd(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SAND_MI = A_SAND_MI, A_SILT_MI = A_SILT_MI)]
# estimate missing BD, using PTFs



# WHC -------------------------------------

## Estimate water holding capacity  ---------

# Calculate average of predicted BD (kg/m3)
dt[, D_BDS_kg_m3 := ptf_bd2(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI, 
                      A_SAND_MI = A_SAND_MI, A_SILT_MI = A_SILT_MI, 
                      B_DEPTH = B_DEPTH, 
                      B_LU_PTFCLASS = 'agriculture',  # should actually include 'cropland' and 'grassland' too
                      B_LOC_COUNTRY = 'NL')$bd.mean]
# convert kg/m3 to g/cm3
dt[, D_BDS := D_BDS_kg_m3 / 1000]

# calculate WHC
library(euptf2)
dt_whc <- ptf_whc_all(dt) 

# check number of NA's -> ptf2 (for calcareous soil), 6 (Belgium soil: not accurate for clay), 13
dt_whc[is.na(value)|value < 0, .N, by = 'ptf_id']


## Scatter plot SOM vs WHC -------

# prepare facet labels
ptf_label <- paste0("PTF ", unique(dt_whc$ptf_id))
names(ptf_label) <- unique(dt_whc$ptf_id)

ggplot(dt_whc,
       aes(y = value,
           x = A_SOM_LOI,
           #x = A_CLAY_MI,
           #x = D_BDS,
           col = A_CLAY_MI)) + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 1) +
  geom_point() +
  scale_color_viridis() +
  #geom_smooth(method = "loess", se = FALSE) + 
  ylab("Water Holding Capacity (cm3 / cm3)") + xlab("SOM (%)") +
  facet_wrap(.~ ptf_id, ncol = 5, labeller = labeller(ptf_id=ptf_label)) + 
  labs(col = "Clay (%)") +
  theme_minimal() +
  theme(legend.position = "top")
#ggsave(file = paste0(projectdr, "figs/whc_vs_som_perptf_facet.jpeg"),bg = "white", width = 7, height = 7)

## variation in WHC by differnt PTF --------

# quantify variation
iqr <- dt_whc[value > 0, 
              .(q1 = quantile(value, probs = 0.25, na.rm = T),
                q3 = quantile(value, probs = 0.75, na.rm = T),
                q2 = quantile(value, probs = 0.5, na.rm = T)), by = id]
# interquartile range (IQR)
iqr[, iqr := q3 - q1]
# quartile coefficient of dispersion (QCD) 
iqr[, qcd := iqr / (q3 + q1)]
iqr <- merge(iqr, dt[, .(id, B_SOILTYPE_AGR, B_LU_BRP, A_SOM_LOI, D_BDS, A_CLAY_MI)], 
             by = "id", all.x = T)

# histogram QCD per soil type
ggplot(iqr) + 
  geom_histogram(aes(x = qcd)) + facet_grid(B_SOILTYPE_AGR ~ ., scale = "free_y") +
  theme_minimal() 

# som vs clay vs QCD-WCH
gp1 <- ggplot(iqr) + 
  geom_point(aes(x = A_SOM_LOI, y = A_CLAY_MI, col = qcd)) +
  labs(col  = "QCD WHC") + xlab("SOM (%)") + ylab("Clay (%)") + 
  scale_color_viridis() +
  theme_minimal() 

# som vs clay vs median-WHC
gp2 <- ggplot(iqr) + 
  geom_point(aes(x = A_SOM_LOI, y = A_CLAY_MI, col = q2)) +
  labs(col  = "median WHC") + xlab("SOM (%)") + ylab("Clay (%)") + 
  scale_color_viridis() +
  theme_minimal() 

ggpubr::ggarrange(gp2, gp1)
#ggsave(file = paste0(projectdr, "figs/whc_vs_som_vs_clay_median_qcd.jpeg"),bg = "white", width = 8, height = 3)


## 5-95% of each interval
# make intervals of SOM
dt_whc[, SOM_int := cut(A_SOM_LOI, breaks = seq(min(A_SOM_LOI), max(A_SOM_LOI), length.out = 100),
                     labels = seq(min(A_SOM_LOI), max(A_SOM_LOI), length.out = 100)[-1])]
# median and percentiles of each SOM interval
sum_int <- dt_whc[, .(med = median(value, na.rm = T),
                                     q5 = quantile(value, probs = 0.05, na.rm = T),
                                     q95 = quantile(value, probs = 0.95, na.rm = T),
                                     N = .N), by = SOM_int]
sum_int[, SOM_int := as.numeric(as.character(SOM_int))]

ggplot(sum_int) +
  # 5-95th percentile
  geom_segment(aes(x = SOM_int, xend = SOM_int, y = q5, yend = q95)) +
  # median
  geom_line(aes(x = SOM_int, y = med), col = "red") +
  # fitted line
  geom_smooth(data = dt_whc, aes(x = A_SOM_LOI, y = value),
              method = "lm", formula = y ~ poly(x, 2), se = FALSE)+
  #xlim(c(0, max(dt3$A_SOM_LOI))) + ylim(c(0,2000)) + 
  xlab("SOM (%)") + ylab("Water Holding Capacity (cm3 / cm3)") +
  theme_minimal() 
#ggsave(file = paste0(projectdr, "figs/whc_vs_som_interval.jpg"), bg = "white", width = 5, height = 5)



## check types of WFC PTF's -----
ptf.whc <- as.data.table(read_xlsx(paste0(projectdr, "sptf_WHC.xlsx"),
                                   sheet = "data"))
# exclude ptf x-3 as it needs Rosetta
ptf.whc <- ptf.whc[!ptf_id %in% c("whcx_3")]

# count input parameters
cols <- paste0("in", 1:6)
ptf.whc2 <- ptf.whc[, (cols) := tstrsplit(soilproperties, '||', fixed=TRUE)][]
ptf.whc2 <- melt(ptf.whc2, id.vars = c("ptf_id"), measure.vars = cols)
ptf.whc2 <- ptf.whc2[!is.na(value)]
# nr input parameters per PTF
input_nr <- ptf.whc2[, .N, by = ptf_id]
table(input_nr$N)
# nr PTF per input parameter
tb <- ptf.whc2[, .N, by = value]
tb <- tb[order(N, decreasing = T)]
# class of PTF
table(ptf.whc$class)


# PMN ----------------------------------

## Estimate PMN -------------
dt_pmn <- ptf_pmn_all(dt) 

# check number of NA's
dt_pmn[is.na(value), .N, by = 'ptf_id']


## check type of PMN PTF's ---------
ptf.pmn <- as.data.table(read_xlsx(paste0(projectdr, "sptf_PMN.xlsx"),
                                   sheet = "data"))
# count input parameters
cols <- paste0("in", 1:7)
ptf.pmn2 <- ptf.pmn[, (cols) := tstrsplit(soilproperties, '||', fixed=TRUE)][]
ptf.pmn2 <- melt(ptf.pmn2, id.vars = c("ptf_id"), measure.vars = cols)
ptf.pmn2 <- ptf.pmn2[!is.na(value)]
# nr input parameters per PTF
input_nr <- ptf.pmn2[, .N, by = ptf_id]
table(input_nr$N)
# nr PTF per input parameter
tb <- ptf.pmn2[, .N, by = value]
tb <- tb[order(N, decreasing = T)]


## scatter plot PMN vs total N--------
# prepare facet labels
ptf_label <- paste0("PTF ", unique(dt_pmn$ptf_id))
names(ptf_label) <- unique(dt_pmn$ptf_id)

ylim2u <- 1000
#ylim2u <- 200
ggplot() +
  #col = A_CLAY_MI)) +
  geom_point(data = dt_pmn,
             aes(y = value,
                 x = A_N_RT
                 ,col = A_PH_CC
                 )) +
  #col = A_CLAY_MI)) +
  scale_color_viridis() +
  #geom_smooth(method = "loess", se = FALSE) + 
  ylab("PMN  (mg / kg / 7 days)") + xlab("N total (mg / kg)") +
  # add range of total N in original dataset
  geom_segment(data = ptf.pmn,
            aes(x = lowerrange_Ntotal_mgkg, xend = upperrange_Ntotal_mgkg,
                y = ylim2u*0.9, yend = ylim2u*0.9),
            arrow = arrow(length = unit(0.03, "inches"), angle = 90, ends = "both")) +
facet_wrap(.~ ptf_id, ncol = 4, labeller = labeller(ptf_id=ptf_label)) +   
  labs(col = "pH") +
  ylim(c(0,ylim2u)) +
  #xlim(c(0,5000)) + # this is the range of most Dutch soils (81%)
  theme_minimal() +
  theme(legend.position = "bottom")
#ggsave(file = paste0(projectdr, "figs/pmn_vs_ntot.jpeg"),bg = "white", width = 8, height = 6)
#ggsave(file = paste0(projectdr, "figs/pmn_vs_ntot_zoomup.jpeg"),bg = "white", width = 8, height = 6)


## 5-95% of each interval
# make intervals of Ntot
dt_pmn[, PMN_int := cut(A_N_RT, breaks = seq(min(A_N_RT), max(A_N_RT), length.out = 100),
                        labels = seq(min(A_N_RT), max(A_N_RT), length.out = 100)[-1])]
# median and percentiles of each SOM interval
sum_int <- dt_pmn[, .(med = median(value, na.rm = T),
                      q5 = quantile(value, probs = 0.05, na.rm = T),
                      q95 = quantile(value, probs = 0.95, na.rm = T),
                      N = .N), by = PMN_int]
sum_int[, PMN_int := as.numeric(as.character(PMN_int))]

ggplot(sum_int) +
  # 5-95th percentile
  geom_segment(aes(x = PMN_int, xend = PMN_int, y = q5, yend = q95)) +
  # median
  geom_line(aes(x = PMN_int, y = med), col = "red") +
  # # fitted line
  # geom_smooth(data = dt_pmn, aes(x = PMN_int, y = value),
  #             method = "lm", formula = y ~ poly(x, 2), se = FALSE)+
  xlab("Total N (mg/kg)") + ylab("PMN  (mg / kg / 7 days)") +
  #xlim(c(0,5000)) +
  #ylim(c(0, 1000)) +
  theme_minimal() 
#ggsave(file = paste0(projectdr, "figs/pmn_vs_Ntot_interval.jpg"), bg = "white", width = 5, height = 5)
