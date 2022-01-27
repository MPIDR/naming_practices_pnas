
# Load libraries
library(RColorBrewer); library(rgdal); library(rworldmap); library(car)

#### -------------------------------------------------------------------------------- ####
# FIGURE 1
#### -------------------------------------------------------------------------------- ####

# Reading the data
t_all<-readRDS("Data_Figure_1.RDS")[[1]]
t_pro<-readRDS("Data_Figure_1.RDS")[[2]]

# Labels and colors
xlab<-'Year of publication'
ylab1<-'Proportion'; ylab2<-'Localization rate'
c06<-scales::alpha(brewer.pal(7, 'Dark2'), .75)

# Figure layout
par(mfrow=c(1,2), mar=c(4,4,1,.5), oma=c(4,0,0,0))

# Panel A
barplot(t_pro, col=c06, border='transparent', ylim=c(0,1), xlab=xlab, ylab=ylab1, space=0)

# Panel B
plot(0, 0, xlim=c(1996, 2020), ylim=c(0,1), xlab=xlab, ylab=ylab2)
for(i in 1:length(levels(t_all$r_abregf))){ 
   lines(t_all[t_all$r_abregf==levels(t_all$r_abregf)[i], c('r_yearpu', 'r_duoutc.mean')],
         col=c06[i], lwd=2)
}

# Legend
legend(1960, -.25, fill=c06, legend=levels(t_all$r_abregf), bty='n', xpd=NA,
       title=expression(bold("Region of study")), border='transparent', ncol=3,
       title.adj=0)


#### -------------------------------------------------------------------------------- ####
# FIGURE 2
#### -------------------------------------------------------------------------------- ####

# Reading the data, merging with world map, and using the Robinson projection
dgc<-readRDS('Data_Figure_2.RDS')
m1<-joinCountryData2Map(dgc, joinCode="ISO3", nameJoinColumn="iso3c") 
robinson = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
m1<-spTransform(m1, CRS=robinson)

# Re-coding countries
m1@data[is.na(m1@data$total_ob), c('GEOUNIT','NAME','total_lc','total_oc')]
m1@data$total_lc[m1@data$GEOUNIT=='Kosovo']<-
  m1@data$total_lc[m1@data$GEOUNIT=='Republic of Serbia']
m1@data$total_oc[m1@data$GEOUNIT=='Kosovo']<-
  m1@data$total_oc[m1@data$GEOUNIT=='Republic of Serbia']

m1@data$total_lc[m1@data$GEOUNIT=='Northern Cyprus' | m1@data$GEOUNIT=='Aland']<-
  m1@data$total_lc[m1@data$GEOUNIT=='Finland']
m1@data$total_oc[m1@data$GEOUNIT=='Northern Cyprus' | m1@data$GEOUNIT=='Aland']<-
  m1@data$total_oc[m1@data$GEOUNIT=='Finland']

# Map limits and color palette
x_coord<-c(bbox(m1)[1,1]*.75, bbox(m1)[1,2]*.9)
y_coord<-c(bbox(m1)[2,1]*.25, bbox(m1)[2,2]*.4)
c05<-brewer.pal(9, 'YlOrRd')[c(2,3,5,7,8)]

# Panel A
par(mar=c(0,0,0,0), mfrow=c(2,1))
mapCountryData(m1, nameColumnToPlot="total_oc", catMethod='categorical',
               addLegend=T, missingCountryCol="white", ylim=y_coord, xlim=x_coord,
               borderCol='gray40', numCats=5, oceanCol='white', lwd=0.01,
               mapTitle="", colourPalette=scales::alpha(c05,.95))
# Panel B
mapCountryData(m1, nameColumnToPlot="total_lc", catMethod='categorical',
               addLegend=T, missingCountryCol="white", ylim=y_coord, xlim=x_coord,
               borderCol='gray40', numCats=5, oceanCol='white', lwd=0.01,
               mapTitle="", colourPalette=scales::alpha(c05,.95))

#### -------------------------------------------------------------------------------- ####
# FIGURE 3
#### -------------------------------------------------------------------------------- ####

# Reading the data (summary Poisson model)
sp0<-readRDS("Data_Figure_3.RDS")

# Transforming summary model into a table for plotting
s<-data.frame((sp0[[1]]$coeff))[-1,]
int<-data.frame((sp0[[1]]$coeff))[1,1]
gap<-2.8
colnames(s)<-substr(colnames(s), 1, 3)
s$p_catego<-substr(rownames(s), 9, 50)
s$p_variab<-substr(rownames(s), 1,8)
s$p_varloc<-recode(c(1, diff(as.numeric(as.factor(s$p_variab)))), "0=0; else=gap")
s$p_upperv<-s$Est + qnorm(.975) * s$Std
s$p_lowerv<-s$Est - qnorm(.975) * s$Std
s$p_yaxisp<-cumsum(rep(1, nrow(s)) + s$p_varloc); s$p_yaxisp<-max(s$p_yaxisp)-s$p_yaxisp
s$p_yaxisp<-max(s$p_yaxisp)-s$p_yaxisp
ran<-range(c(s$p_upperv, s$p_lowerv))

# Background
par(mar=c(3,10,0,0))
plot(s$Est, nrow(s):1, yaxt='n', ylab='', xlab='', type='n', 
     ylim=c(0,max(s$p_yaxisp+gap)), xlim=range(c(s$p_upperv, s$p_lowerv)))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray92")
abline(v=seq(ran[1], ran[2], diff(ran)/10), lty=2, col='white')
abline(v=0, lty=2, col='gray70')
abline(h=s$p_yaxisp, col='white', lty=3)
legend('topright', legend = paste0('Intercept: ', round(int, 3)), bty='n', cex=0.8)
mtext("Regression coefficient", side=1, line=2)

# Confidence intervals and point estimates
rect(s$p_lowerv, s$p_yaxisp-.3, s$p_upperv, s$p_yaxisp+.3, 
     col=scales::alpha('red',.4), border='gray50')
points(s$Est, s$p_yaxisp, pch='|', cex=.6, lwd=2, col='gray50')

# Labels
vlabs<-c("Number of countries \n (Ref: One)",
         "Title's length \n (Ref: Medium)",
         "Year of publication \n (Ref: 2005-2010)",
         "Number of authors \n (Ref: One)",
         "First author's location \n (Ref: Europe and North America)",
         "Region of study \n (Ref: Europe and North america)")
axis(2, s$p_yaxisp, s$p_catego, las=2, font=3, cex.axis=0.6)
axis(2, s$p_yaxisp[s$p_varloc==gap]+c(3,5,5,4,6,6)-.5, vlabs, las=2, tick=F, 
     cex.axis=0.7, font=2)


#### -------------------------------------------------------------------------------- ####
# FIGURE 4
#### -------------------------------------------------------------------------------- ####

# Reading the data
sp0<-readRDS("Data_Figure_4.RDS")

# Color, labels, and aesthetics
c06<-brewer.pal(7, 'Dark2')
col<-list(c06[c(1,1,2,2,3,3,4,4,5,5,6,6)], c06[c(3,1,1,2,1,1,1,1,4,1,2,3,4,5,6)])
alp<-list(rep(c(0.4, 0.8), 6), c(rep(0.8, 9), c(rep(0.4, 6))))
list_labels<-list(c('Europe and Northern America \n (Excluding the UK and the US)',
                    'United Kingdom',
                    'Central and Southern Asia \n (Excluding India)', 'India', 
                    'Eastern and South-Eastern Asia \n  (Excluding China)', 'China',
                    'Latin America and The Caribbean \n (Excluding Brazil)', 'Brazil',
                    'Northern Africa and Western Asia \n (Excluding Israel)', 'Israel',
                    'Sub-Saharan Africa \n (Excluding South Africa)', 'South Africa'),
                  c('China', 'United Kingdom', 'Australia', 'India','Canada', 'Germany',
                    'France','Russia','Brazil',
                    'Europe and North America (Excl. top ten \n Europe, Australia, Canada, and the US)',
                    'Central and Southern Asia \n (Excluding India)',
                    'Eastern and South-Eastern Asia \n  (Excluding China)',
                    'Latin America and The Caribbean \n (Excluding Brazil)',
                    'Northern Africa and Western Asia',
                    'Sub-Saharan Africa'))

vlabs<-c("Subject area \n (Ref: Health Sciences)",
         "Number of thematic areas \n (Ref: One)",
         "Title's length \n (Ref: Very short)",
         "Year of publication \n (Ref: 1995-1999)",
         "Number of authors \n (Ref: One)",
         "First author's location \n (Ref: Europe and North America)",
         "Region or country of study \n (Ref: United States of America)")[7]

par(mar=c(3,12,0.5,0), mfrow=c(1,2)); gap<-1
for(f in 1:2){
  s<-data.frame((sp0[[f]]$coeff))[-1,]; int<-data.frame((sp0[[f]]$coeff))[1,1]
  colnames(s)<-substr(colnames(s), 1, 3)
  s$p_catego<-substr(rownames(s), 9, 50)
  s$p_variab<-substr(rownames(s), 1,8)
  s$p_varloc<-recode(c(1, diff(as.numeric(as.factor(s$p_variab)))), "0=0; else=gap")
  s$p_upperv<-s$Est + 1.96 * s$Std
  s$p_lowerv<-s$Est - 1.96 * s$Std
  s$p_yaxisp<-cumsum(rep(1, nrow(s)) + s$p_varloc)
  s$p_yaxisp<-max(s$p_yaxisp)-s$p_yaxisp
  s<-s[grep('r_abloc', rownames(s)),]
  ran<-range(c(s$p_upperv, s$p_lowerv))
  
  # Background
  plot(s$Est, nrow(s):1, yaxt='n', ylab='', xlab='', type='n', 
       ylim=c(0,max(s$p_yaxisp+gap+1)), xlim=range(c(s$p_upperv, s$p_lowerv)))
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray92")
  abline(v=seq(ran[1], ran[2], diff(ran)/10), lty=2, col='white')
  #abline(v=1, lty=2, col='gray50')
  abline(h=s$p_yaxisp, col='white', lty=3)
  legend('topright', legend = paste0('Intercept: ', ceiling(int*100)/100), bty='n', 
         cex=0.8)
  mtext("Regression coefficient", side=1, line=2)
  
  # Confidence intervals and point estimates
  rect(s$p_lowerv, s$p_yaxisp-.25, s$p_upperv, s$p_yaxisp+.25,
       col=scales::alpha(col[[f]], alp[[f]]), border='gray50')
  points(s$Est, s$p_yaxisp, pch='|', cex=1, col='gray50')
  
  # Legends
  llab<-list_labels[[f]]
  axis(2, s$p_yaxisp, llab, las=2, font=3, cex.axis=0.7)
  axis(2, s$p_yaxisp[s$p_varloc==gap]+2,  
       "Region or country of study \n (Ref: United States of America)", 
       las=2, tick=F, cex.axis=0.85, font=2)
}



