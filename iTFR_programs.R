###########################################################
# downloaded HFD and HMD datasets on 6 May 17
# 1. from HFD - exposRR.txt (expos by sing yr age, country, year)
# 2. from HFD - tfrRR.txt (period TFR by country, year)
# 3. from HFD - totbirthsRR.txt (total births by country, year)
# 4. from HMD - HMD_population.zip (zipped file containing
#               a directory called Population5, which in turn
#               contains files called xxxPopulation5.txt that
#               have Jan 1 pop estimates by 5-yr age group
#               for country xxx)
###########################################################

rm(list=ls())
#graphics.off()
#if (.Platform$OS.type == 'windows') windows(record=TRUE)
#setwd("U:/0 govserv/ADP/Manuscripts/1. Schmertmann iTFR")
library(HMDHFDplus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(openxlsx) # Microsoft Excel Files



primateeval = read.xlsx("U:/0 govserv/ADP/Manuscripts/1. Schmertmann iTFR/Copy of Bronikowski.LifeTables.SciData2016.xlsx", sheet=2)



# a function to calculate five-year averages of TFR
# given vector X of length L, returns
#   NA NA NA NA,  mean( X[1:5]), mean(X[2:6]), ..., mean(X[(L-4):L])
mean_lag5 = function(X) {
  if (length(X) > 4) {
    Z = sapply( 5:length(X),  function(i) mean(X[i - 0:4]))
    Z = c( rep(NA,4), Z)
  } else {
    Z = rep(NA, length(X))
  }  
  return(Z)
}

# set up data from Human FERTILITY database files
# 
# remove GBR_NP and DEUTNP observations, because those
# countries are subdivided and we don't want to double count

discard = c('GBR_NP', 'DEUTE', 'DEUTW')

HFD_expos = readHFD('exposRR.txt') %>%
  filter(Age %in% 15:49, !(Code %in% discard)) %>%
  mutate(AgeGroup = 5 * floor(Age/5)) %>%
  group_by(Code,Year,AgeGroup) %>%
  summarize(W = sum(Exposure)) %>%
  spread(key=AgeGroup, value=W, sep='_') %>%
  select(Code,Year,W=starts_with('AgeGroup')) %>%
  ungroup()

HFD_tfr = readHFD('tfrRR.txt') %>%
  filter(!(Code %in% discard)) %>%
  select(-TFR40)


HFD_births = readHFD('totbirthsRR.txt') %>%
  filter(!(Code %in% discard)) %>%
  select(Births=Total, Code=Code, Year=Year)

HMD_population = data.frame()
lt_both = data.frame()

# set up data from Human MORTALITY database files
for (this.country in unique(HFD_expos$Code)) {
  filename = paste0(this.country,'.Population5.txt')
  unzip(zipfile='HMD_population.zip', 
        files=paste0('Population5/',filename),
        junkpaths=TRUE)
  
  tmp = read.table(filename, skip=2, stringsAsFactors = FALSE,
                   header=TRUE)
  
  # Some countries have YYYY+ and YYYY- versions of Year, for yrs in
  # which territory changed (AK and HI in 1959 US, NF in 1949 Canada, etc)
  # In all cases we discard the "-" versions, and remove the "+"
  
  minus_ix = grep('-', tmp$Year)
  if (length(minus_ix) > 0)  tmp = tmp[-minus_ix, ]
  
  plus_ix = grep('+', tmp$Year)
  if (length(plus_ix) > 0)  tmp$Year[plus_ix] = substr(tmp$Year,1,4)
  
  
  
  tmp = tmp %>%
    mutate(Code = this.country,
           Year = as.integer(Year))
  
  tmp$Age = c(0,1,seq(5,110,5))   # replace with numeric
  
  tmp = tmp %>%
    group_by(Code, Year) %>%
    summarize( C = sum(Total[Age %in% 0:1]),
               W15 = Female[Age == 15],
               W20 = Female[Age == 20],
               W25 = Female[Age == 25],
               W30 = Female[Age == 30],
               W35 = Female[Age == 35],
               W40 = Female[Age == 40],
               W45 = Female[Age == 45],
               W   = sum(Female[Age %in% seq(15,45,5)]),
               p2534 = (W25+W30)/W,
               iTFR = 7*C/W) %>%
    ungroup()
  
  HMD_population = rbind(HMD_population, tmp)
  
  file.remove(filename)
  
} # for this.country

# Merge
for (this.country in unique(HFD_expos$Code)) {
  filename = paste0(this.country,'.bltper_1x1.txt')
  unzip(zipfile='lt_both.zip', 
        files=paste0('bltper_1x1/',filename),
        junkpaths=TRUE)
  
  tmp = read.table(filename, skip=2, stringsAsFactors = FALSE,
                   header=TRUE)
  
  # Some countries have YYYY+ and YYYY- versions of Year, for yrs in
  # which territory changed (AK and HI in 1959 US, NF in 1949 Canada, etc)
  # In all cases we discard the "-" versions, and remove the "+"
  
  minus_ix = grep('-', tmp$Year)
  if (length(minus_ix) > 0)  tmp = tmp[-minus_ix, ]
  
  plus_ix = grep('+', tmp$Year)
  if (length(plus_ix) > 0)  tmp$Year[plus_ix] = substr(tmp$Year,1,4)
  
  
  
  tmp = tmp %>%
    mutate(Code = this.country,
           Year = as.integer(Year))
  
  #tmp$Age = c(0,1,seq(5,110,5))   # replace with numeric
  
  tmp = tmp %>%
    filter(Age==0)
  #  group_by(Code, Year) %>%
  #  summarize(q1 = qx[Age == 0]   %>%
  #  ungroup()
  
  
  lt_both = rbind(lt_both, tmp)
  
  file.remove(filename)
  
} # for this.country

earliest.year = 1891

tmp1 = left_join(HFD_tfr, 
                select(HMD_population, Code, Year, C, contains('W'), iTFR, p2534), 
                by=c('Code','Year'))

tmp = left_join(tmp1, 
                select(lt_both, Code, Year, qx), 
                by=c('Code','Year'))

## calculate the lagged TFR values for each country
tmp$lagTFR = NA

for (this.country in unique(tmp$Code)) {
  ix = which(tmp$Code == this.country)
  tmp$lagTFR[ix] = mean_lag5( tmp$TFR[ix])  
} # for this.country

tmp = tmp %>%
  filter(Year >= earliest.year, !is.na(iTFR)) %>%   
  mutate( CWR    = C/W,
          mult   = lagTFR/CWR,
          p15    = W15/W,
          p20    = W20/W,
          p25    = W25/W,
          p30    = W30/W,
          p35    = W35/W,
          p40    = W40/W,
          p45    = W45/W,
          decade = as.factor( 10 * floor(Year/10)) )

sample_ids <- sample(1:nrow(tmp), 600, replace=FALSE)
sample_in <- tmp[sample_ids,]
sample_out <- tmp[-sample_ids,]

reg = lm( mult ~ p2534, data=sample_in) 
reg2 = lm( mult ~ -1+p15+p20+p25+p30+p35+p40+p45, data=sample_in) 

beta = coef(reg2)

percentilerank<-function(x){
  rx<-rle(sort(x))
  smaller<-cumsum(c(0, rx$lengths))[seq(length(rx$lengths))]
  larger<-rev(cumsum(c(0, rev(rx$lengths))))[-1]
  rxpr<-smaller/(smaller+larger)
  rxpr[match(x, rx$values)]
}

sample_out = sample_out %>%
  mutate( xTFR = (coef(reg)[1] + coef(reg)[2] * p2534) * CWR,
          zTFR = (beta[1]*p15+beta[2]*p20+beta[3]*p25+beta[4]*p30+beta[5]*p35+beta[6]*p40+beta[7]*p45) * CWR
          )

# brief summary
tmp %>% 
  group_by(Code) %>%
  summarize(nschedules=n(), 
            firstyr = min(Year),
            lastyr  = max(Year),
            minTFR=round(min(lagTFR,na.rm=TRUE),2),
            maxTFR=round(max(lagTFR,na.rm=TRUE),2)) %>%
  data.frame()

total <- full_join(tmp,sample_out, c("Code", "Year"))

total2 = select(total, Code, Year, TFR=TFR.x, iTFR=iTFR.x, lagTFR=lagTFR.x, xTFR=xTFR.y, C=C.y, W=W.x, W15=W15.x, W20=W20.x, W30=W30.x, W35=W35.x, W40=W40.x, W45=W45.x, p2534=p2534.y )

write.table(total2, "U:/0 govserv/ADP/Manuscripts/1. Submitted Localized Estiamtes fo the TFR/xitfr.txt", sep = "\t", row.names = TRUE) # Tab Delimited

my_groba = grobTree(textGrob("a", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobb = grobTree(textGrob("b", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobc = grobTree(textGrob("c", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobd = grobTree(textGrob("d", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobe = grobTree(textGrob("e", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobf = grobTree(textGrob("f", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobg = grobTree(textGrob("g", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobh = grobTree(textGrob("h", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobi = grobTree(textGrob("i", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobj = grobTree(textGrob("j", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobk = grobTree(textGrob("k", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))


f1a<- ggplot(data=tmp, aes(x=lagTFR, y=iTFR)) +
  geom_text(x=1, y=3.5, label="iTFR", col='red', size=6)+
  annotation_custom(my_groba)+
  geom_point(size=1, alpha=.50,color='gray') +
  geom_abline(intercept=0,slope=1) +
  theme_bw() +
  theme(legend.position="none")+
  theme(text=element_text(face='bold')) +
  xlim(0.9,4.1) + ylim(0.9,4.1) +
  labs(#caption='Source: Human Fertility Database 1950+', 
       #title='Actual TFR vs. iTFR',
       x='TFR (Data)', y='p(TFR)') +
  geom_line(aes(x=lagTFR, y= 0.90 * lagTFR), lty=2, lwd=1, col='black') +
  geom_line(aes(x=lagTFR, y= 1.10 * lagTFR), lty=2, lwd=1, col='black') 
  #geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
  #geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)

f1c<-ggplot(data=tmp, aes(x=lagTFR, y=xTFR)) +
  geom_text(x=1, y=3.5, label="xTFR", col='blue', size=6)+
  annotation_custom(my_grobb)+
  geom_point(size=1, alpha=.50,color='gray') +
  geom_abline(intercept=0,slope=1) +
  theme_bw() +
  theme(text=element_text(face='bold')) +
  xlim(0.9,4.1) + ylim(0.9,4.1) +
  labs(#caption='Source: Human Fertility Database 1950+', 
       #title='Actual TFR vs. Estimate from Age-Sex Pyramid',
       x='TFR (Data)', y='p(TFR)') +
  geom_line(aes(x=lagTFR, y= 0.90 * lagTFR), lty=2, lwd=1, col='black') +
  geom_line(aes(x=lagTFR, y= 1.10 * lagTFR), lty=2, lwd=1, col='black') 
 # geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
  #geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)

f1c<-ggplot(data=tmp, aes(x=C, y=ipct)) +
  annotation_custom(my_grobc)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  theme(text=element_text(face='bold')) +
  ylim(-30,30) +
  scale_x_log10() +
  labs(#caption='Source: Human Fertility Database 1950+', 
       #title='Algebraic Percent Error',
       x='Population, n', y='M(TFR)') 
  #geom_line(aes(x=lagTFR, y= 0.90 * lagTFR), lty=2, lwd=1, col='black') +
  #geom_line(aes(x=lagTFR, y= 1.10 * lagTFR), lty=2, lwd=1, col='black')
  #geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
  #geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)

f1d<-ggplot(data=tmp, aes(x=C, y=xpct)) +
  annotation_custom(my_grobd)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  scale_x_log10() +
  theme(text=element_text(face='bold')) +
  ylim(-30,30) +
  labs(#caption='Source: Human Fertility Database 1950+', 
       #title='Algebraic Percent Error',
       x='Population, n', y='M(TFR)')

#geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
#geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)

f1e<- ggplot(data=tmp, aes(x=Year, y=ipct)) +
  annotation_custom(my_grobe)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  theme(text=element_text(face='bold')) +
  xlim(1891,2015) + ylim(-30,30) +
  labs(#caption='Source: Human Fertility Database 1950+', 
       #title='Algebraic Percent Error',
       x='Time', y='M(TFR)') 
#geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
#geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)

f1f<- ggplot(data=tmp, aes(x=Year, y=xpct)) +
  annotation_custom(my_grobf)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  theme(text=element_text(face='bold')) +
  xlim(1891,2015) + ylim(-30,30) +
  labs(#caption='Source: Human Fertility Database 1950+', 
       #title='Algebraic Percent Error',
       x='Time', y='M(TFR)') 
#geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
#geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)
  
factsi = tmp %>% 
  #group_by(ierr) %>% 
  summarize(
    p10a=quantile(ierr, 0.1, na.rm=T), 
    p90a=quantile(ierr, 0.9, na.rm=T),
    p10p=quantile(ipct, 0.1, na.rm=T),
    p90p=quantile(ipct, 0.9, na.rm=T)
  )
factsx = tmp %>% 
  #group_by(ierr) %>% 
  summarize(
    p10a=quantile(xerr, 0.1, na.rm=T), 
    p90a=quantile(xerr, 0.9, na.rm=T),
    p10p=quantile(xpct, 0.1, na.rm=T), 
    p90p=quantile(xpct, 0.9, na.rm=T)
  )

f1g<-ggplot() +
  annotation_custom(my_grobg)+
  geom_density(data=tmp, aes(x=ierr), adjust=1.5,fill='red',col='red', lwd=1, alpha=.20) +
  geom_density(data=tmp, aes(x=xerr),adjust=1.5, fill='blue',col='blue', lwd=1, alpha=.20) +
  geom_segment(aes(x=0,xend=0,y=0,yend=5.5), lwd=1.5) +
  theme_bw() +
  xlim(-0.5,0.5) + 
  geom_vline(data=factsi, aes(xintercept=p90a), lty=2, lwd=1, color="red")+
  geom_vline(data=factsi, aes(xintercept=p10a), lty=2, lwd=1, color="red")+
  geom_vline(data=factsx, aes(xintercept=p90a), lty=2, lwd=1, color="blue")+
  geom_vline(data=factsx, aes(xintercept=p10a), lty=2, lwd=1, color="blue")+
  #geom_vline(xintercept = 0.2, lty=2, lwd=1)+
  #geom_vline(xintercept = -0.2, lty=2, lwd=1)+
  theme(text=element_text(face='bold')) +
  labs(x='Abs(error)',
       y='Density'
      # title='Estimation Errors: iTFR (red) versus Age-Structure-Adjusted iTFR (blue)',
       #caption='Human Fertility Database 1950+'
       )

f1h<- ggplot() +
  annotation_custom(my_grobh)+
  geom_density(data=tmp, aes(x=ipct), adjust=1.5,fill='red',col='red', lwd=1, alpha=.20) +
  geom_density(data=tmp, aes(x=xpct),adjust=1.5, fill='blue',col='blue', lwd=1, alpha=.20) +
  geom_segment(aes(x=0,xend=0,y=0,yend=0.08), lwd=1.5) +
  theme_bw() +
  #xlim(-.5,+0.50) + 
  geom_vline(data=factsi, aes(xintercept=p90p), lty=2, lwd=1, color="red")+
  geom_vline(data=factsi, aes(xintercept=p10p), lty=2, lwd=1, color="red")+
  geom_vline(data=factsx, aes(xintercept=p90p), lty=2, lwd=1, color="blue")+
  geom_vline(data=factsx, aes(xintercept=p10p), lty=2, lwd=1, color="blue")+
  #geom_vline(xintercept = 10, lty=2, lwd=1)+
  #geom_vline(xintercept = -10, lty=2, lwd=1)+
  theme(text=element_text(face='bold')) +
  labs(x='Percent(error)',
       y='Density'
       # title='Estimation Errors: iTFR (red) versus Age-Structure-Adjusted iTFR (blue)',
       #caption='Human Fertility Database 1950+'
  )



  top<- grid.arrange(f1a,f1b,f1c,f1d, f1e, f1f, f1g, f1h,  ncol=2)
  grid.arrange(top, primatedata, ncol=1)