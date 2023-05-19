# The following types of data were removed: bulk densities less than 0.30 Mg m3 or greater than 2.30 Mg m3 and soils in
# which the sum of clay, sand, and silt contents was greater than or less than 100%

require(data.table)
require(soilptf)

# make prediction
# for mineral soils only: 20% OS = 10% OC = 100 g/kg
dt1 <- data.table(A_C_OF = seq(0.5,100,1), A_CLAY_MI = 7.5, A_SAND_MI = 60)
dt1[,id := .I]
dt2 <- ptf_bd(A_C_OF = dt1$A_C_OF,
              A_CLAY_MI = dt1$A_CLAY_MI,
              A_SAND_MI = dt1$A_SAND_MI,nmax = 200)
dt <- merge(dt1,dt2,by='id') 

dt[,logC := log(A_C_OF)]
dt[,logBD := log(bd.mean)]

m1 = lm(bd.mean~logC+A_C_OF,data=dt)
p1 <- c(predict(m1, newdata = data.table(logC =dt$logC, A_C_OF = dt$A_C_OF)))
lbl1 <- paste('Density == ', '1617 - 77.4 * logC - 3.49 * C')

m1 = lm(bd.mean~logC,data=dt)
p1 <- predict(m1,newdata = data.table(logC = dt$logC))

m1 = lm(logBD~A_C_OF,data=dt)
p1 <- exp(predict(m1,newdata = data.table(A_C_OF = dt$A_C_OF)))


m2 = lm(bd.mean~A_C_OF+I(A_C_OF^2),data=dt)
p2 <- predict(m2,newdata = data.table(A_C_OF = dt$A_C_OF))
lbl <- paste('Density == ', '1520 - 9.86 * C + 0.041 * C^2')



require(ggplot2)

pp1 <- ggplot(data = dt,aes(x = A_C_OF,y=bd.mean)) + geom_point() + geom_line()+
  geom_errorbar(aes(ymin=bd.mean - bd.sd, ymax = bd.mean + bd.sd),width = 0.2) +
  geom_line(aes(y = p1),col='red')+
  #geom_line(aes(y = p2),col='red')+
  ylim(0,2000)+
  annotate('text',x = 2.5, y = 500, label = lbl1,parse = T,size = 4,adj=0) + 
  xlab('organic carbon content (g/kg)') + ylab('bulk density (kg/m3)') + theme_bw() +
labs(title = "A. Relationship between bulk density and SOC",
     subtitle = "derived from 180 ptfs for mineral soils") +
  theme(plot.subtitle=element_text(size=10, face="italic", color="black"),
        axis.text = element_text(size = 12,colour ='black'),
        axis.title = element_text(size = 12,colour ='black'),
        title = element_text(size=12,colour ='black'))

# make prediction
dt1 <- CJ(A_C_OF = seq(0.5,100,0.5), A_CLAY_MI = seq(1,75,10), A_SAND_MI = seq(1,100,10))
dt1 <- dt1[A_SAND_MI + A_CLAY_MI <= 100]
dt1[,id := .I]

dt2 <- ptf_bd(A_C_OF = dt1$A_C_OF,
              A_CLAY_MI = dt1$A_CLAY_MI,
              A_SAND_MI = dt1$A_SAND_MI,nmax = 200)
dt <- merge(dt1,dt2,by='id') 

summary(lm(bd.mean~A_C_OF + I(A_C_OF^2),data=dt))


# MAKING FIGURE
# bd <- function(oc) {100/(oc/800 + (100 - oc)/1600)}
# bd = function(oc){1520.31 - 9.8561 * oc + 0.0406177 * oc^2}

require(data.table)

# retrieve model for predictions
bdm <- ptf_bd_lm()

# estimate C increase over time, oc in g/kg
paf <- function(oc,year) {for(i in 1:year) {oc <- oc * 1.004} ; return(oc)}

# set database for mineral soils up to 20% SOM
db <- data.table(soc1 = 1:100)
db.pred <- predict(bdm,newdata=data.frame(A_C_OF = db$soc1),se.fit = T, level=0.95) # interval = 'confidence',
db[,dens1 := db.pred$fit]
db[,dens1_se := db.pred$se.fit]
db[,cf_unit := 100 * 100 * 0.30 * 0.001 * 0.001] # from g/kg to ton C/ha
db[, cstock1 :=  cf_unit * dens1 * soc1 ]
db[, cstock1_se := cf_unit * dens1_se * soc1]
db[,soc2 := paf(soc1,year = 100)]
db.pred2 <- predict(bdm,newdata=data.frame(A_C_OF = db$soc2),se.fit = T, level=0.95)
db[,dens2 := db.pred2$fit]
db[,dens2_se := db.pred2$se.fit]
db[, cstock2 := cf_unit * dens2 * soc1] # set soc similar as t0 to see change due to density only 
db[, cstock2_se := cf_unit * dens2_se * soc1]
db[,dcstock := (cstock1 - cstock2)]
db[,dcstock_se := (cstock1_se^2 + cstock2_se^2)^0.5]
db[,slurry_ton := (dcstock * 1000/ 100) / (50 * 0.5) ]
db[,dcstock3 := (cf_unit * dens2 * soc2 - cstock1)]
db[,dcstock4 := (cf_unit * dens1 * soc2 - cstock1)]

dbmodcoef <- coefficients(bdm)
db[,ccredit_orig := soc2 * dens2 - soc1 * dens1]
db[,ccredit_new := (soc2 - soc1) * (dbmodcoef[2] + dbmodcoef[3] * (soc1 + soc2)/2)]

require(ggplot2)
pp2 <- ggplot(data = db,aes(x = soc1,y=dcstock)) + 
  geom_point(col='black',cex = 1.5) + theme_bw() + xlim(0,50) + ylim(0,20)+
  geom_errorbar(aes(ymin=dcstock - dcstock_se, ymax = dcstock + dcstock_se),width = 0.2)+
  xlab('initial SOC (g/kg)') + ylab('C stock to be compensated (tonC/ha)') + 
  labs(title = "B. Extra C required for equal C stock",
       subtitle = "required to achieve annual 4-promille change over 100yr") +
  theme(plot.subtitle=element_text(size=10, face="italic", color="black"),
        axis.text = element_text(size = 12,colour ='black'),
        axis.title = element_text(size = 12,colour ='black'),
        title = element_text(size=12,colour ='black'))

plot(dcstock~soc,data=db,main='extra C required for equal C stock',ylab='tonC/ha',xlab= 'initial SOC(%)',
     ylim=c(0,20))
plot(cstock~soc,data = db,xlab="SOC(%)",ylab='C stock (ton C ha)',type='l')
lines(cstock2~soc,data=db,col='blue')

db2 <- melt(db,id.vars = 'soc1',
            measure = c('dcstock3','dcstock4'))
db2[grepl('k3$',variable),variable := 'with density correction']
db2[grepl('k4$',variable),variable := 'without density correction']

pp3 <- ggplot(data = db2,aes(x = soc1,y=value,color = variable)) + 
        geom_point(cex = 2.5) + theme_bw() + xlim(0,50)+ ylim(0,100)+
        xlab('initial SOC (g/kg)') + ylab('C stock (tonC/ha)') + 
        scale_color_manual(name='',
                           values = c("without density correction" = "black",
                                      "with density correction" = "gray75"
                                      )) +
        labs(title = "C. Predicted C stock change in relation to initial SOC level",
             subtitle = "given an annual 4-promille change over 100yr") +
        theme(plot.subtitle=element_text(size=12, face="italic", color="black"),
              legend.position = c(0.1, 0.9),
              legend.text = element_text(size=12),
              legend.title = element_text(size=8,face='bold'),
              legend.direction="vertical",
              legend.justification = "left", 
              legend.margin=margin(t=3, r=0, b=3, l=0, unit="pt"),
              legend.background = element_rect(colour='white'),
              panel.grid.minor = element_blank(),
              axis.text = element_text(size = 10,colour ='black'),
              axis.title = element_text(size = 12,colour ='black'),
              title = element_text(size=12,colour ='black'))

require(patchwork)
pp4 <- pp1 + pp2 + pp3
pp4
ggsave(plot = pp4,filename = 'D:/ESA/04 articles/density_article/figure1_v2.jpg',width = 20,height = 7.4)


# make plot using SD from ptfs
db <- dt[,.(id,A_C_OF,bd.mean,bd.sd)]


# error on dcstock
db[,tmp1 := (sqrt(cstock1_se^2 + cstock2_se^2) / (cstock1 - cstock2))^2]
db[,tmp2 := (cstock_se^2 / cstock)^2]
db[,dcstock_se := sqrt(tmp1 + tmp2) * dcstock ]

# ---------------------------------------

# make prediction
# for mineral soils only: 20% OS = 10% OC = 100 g/kg
dt1 <- CJ(A_C_OF = seq(0.5,100,1), A_CLAY_MI = c(1,5,10,15,25), A_SAND_MI = 60)
dt1[,id := .I]
dt2 <- ptf_bd(A_C_OF = dt1$A_C_OF,
              A_CLAY_MI = dt1$A_CLAY_MI,
              A_SAND_MI = dt1$A_SAND_MI,nmax = 200)
dt <- merge(dt1,dt2,by='id') 

dt[,logC := log(A_C_OF)]
dt[,logBD := log(bd.mean)]
dt[,rdens := 1/(bd.mean*0.001)]
dt[,oc := 0.1 * A_C_OF]
m3 <- lm(rdens~oc + A_CLAY_MI,data=dt)

p1 <- c(predict(m3, newdata = data.table(A_C_OF =dt$A_C_OF, A_CLAY_MI = dt$A_CLAY_MI)))
lbl1 <- paste('Density == ', '1000/(0.6714 + 0.00423 * C - 0.0005 * clay)')
