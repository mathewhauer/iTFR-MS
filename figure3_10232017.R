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
if (.Platform$OS.type == 'windows') windows(record=TRUE)
setwd("U:/0 govserv/ADP/Manuscripts/1. Schmertmann iTFR")
library(HMDHFDplus)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(openxlsx) # Microsoft Excel Files
library(tmap)
library(tmaptools)
library(tigris)
library(censusapi)
library(tidycensus)
library(RColorBrewer)

###   My Census API key   ###
census_api_key("0206e3f2924a424be8722887fd0a49cea6308a7e")

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
  dplyr::select(Code,Year,W=starts_with('AgeGroup')) %>%
  ungroup()

HFD_tfr = readHFD('tfrRR.txt') %>%
  filter(!(Code %in% discard)) %>%
  dplyr::select(-TFR40)


HFD_births = readHFD('totbirthsRR.txt') %>%
  filter(!(Code %in% discard)) %>%
  dplyr::select(Births=Total, Code=Code, Year=Year)

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
                 dplyr::select(HMD_population, Code, Year, C, contains('W'), iTFR, p2534), 
                 by=c('Code','Year'))

tmp = left_join(tmp1, 
                dplyr::select(lt_both, Code, Year, qx), 
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


###   Importing the Natality files from CDC Wonder for US Counties, 2007-2010   ###
tfr_county0710 <- read_tsv("U:/0 govserv/ADP/Manuscripts/1. Schmertmann iTFR/figures/figure3/Natality, 2007-2010.txt") %>%
  dplyr::select(County, CountyCode, AgeofMother9Code, Year, Births, FemalePopulation) %>% # selecting specific values
  mutate(YearCode = as.numeric(Year)) # Year is sometimes weird, this converts it to numeric

###   Importing the Natality file from CDC Wonder for US Counties, 2006-2010   ###
tfr_county06 <- read_tsv("U:/0 govserv/ADP/Manuscripts/1. Schmertmann iTFR/figures/figure3/Natality, 2003-2006.txt")%>%
  dplyr::select(County, CountyCode, AgeofMother9Code, Year, Births, FemalePopulation) %>%
  mutate(YearCode = as.numeric(Year)*1)

###   Merging the two Natality files together   ###
tfr_county <- full_join(tfr_county06, tfr_county0710) %>%
  filter(!is.na(County))  %>% # dropping all of the extra stuff at the bottom of the CDC's output
  group_by(County, CountyCode, Year) %>%
  mutate(fertrat = (Births / as.numeric(FemalePopulation))*5) %>% # Calculating ASFRs
  summarize(TFR = sum(na.omit(fertrat))) # Calculating the TFR

###   Calculating the lagged TFR values
tfr_county$lagTFR = NA

for (this.county in unique(tfr_county$CountyCode)) {
  ix = which(tfr_county$CountyCode == this.county)
  tfr_county$lagTFR[ix] = mean_lag5( tfr_county$TFR[ix])  
}

###   Importing the Census 2010 information for US Counties
census_county <- read_csv("U:/0 govserv/ADP/Manuscripts/1. Schmertmann iTFR/figures/figure3/ift_county_data_c2010.csv") %>%
  mutate(W = (W1517 +  W1819 + W20 + W21 + W2224 + W2529 + W3034 + W3539 + W4044 + W4549), # estimating total number of Women
         CWR = (m0004 + W0004) / W, # estimating Child-woman ratio
         p2534 = (W2529 + W3034) / W, # estimating the proportion of women aged 25-34
         xTFR = (coef(reg)[1] + coef(reg)[2] * p2534) * CWR, # applying the regression coefficients from the HMD/HFD to US counties
         CountyCode = sprintf("%05d", KEY),
         Year = 2010) %>%
  dplyr::select(CountyCode, xTFR, Year) %>%
  left_join(., tfr_county)

###   Estimating the 50th percentile error rate from the xTFR method
countyeval <- census_county %>%
  mutate(num = if_else(lagTFR >0,1,0)) %>%
  summarize(tot = sum(num, na.rm = T) ,
            qlagTFR50 = quantile(abs(xTFR / lagTFR -1), 0.5, na.rm=T),
            qlagTFR90 = quantile(abs(xTFR / lagTFR -1), 0.9, na.rm=T),
            qlagTFR_abs50 = quantile(abs(xTFR - lagTFR), 0.5, na.rm=T),
            qlagTFR_abs90 = quantile(abs(xTFR - lagTFR), 0.9, na.rm=T))

### Selecting the lagged TFR from the 5-year time series for merging with the shapefile.
selectcnties <- census_county %>%
  filter(!is.na(lagTFR))


counties <- counties(cb = TRUE)
countiestfr <- append_data(counties, census_county, key.shp = "GEOID", key.data = "CountyCode", ignore.duplicates = TRUE) %>%
  subset(!(STATEFP %in% c("02", "60", "64","66", "68", "69", "70", "74", "15","72", "78")))
countiestfr2 <- append_data(counties, selectcnties, key.shp = "GEOID", key.data = "CountyCode", ignore.duplicates = TRUE) %>%
  subset(!(STATEFP %in% c("02", "60", "64","66", "68", "69", "70", "74", "15","72", "78"))) %>%
  subset(!is.na(xTFR))

states <- states(cb=TRUE)

pal <- c("#4475B5", "#849EBA", "#BFCCBD", "#ffffbf", "#F9B984", "#ED7550", "#D62F26")

a<- tm_shape(countiestfr, projection = "laea_NA") +
  tm_polygons("xTFR",
              palette = pal , 
              breaks= c(-Inf, 1.0, 1.5, 2.1, 2.5, 3.0, 3.5, Inf),
              auto.palette.mapping = FALSE, 
              id = "GEOID", 
              #showNA = TRUE, 
              border.col = "gray50", 
              border.alpha = 0.4,
              alpha = 0.4,
              legend.show = FALSE
              #style ="jenks",
              #n = 6
  ) +
 tm_shape(countiestfr2) +
 tm_polygons("xTFR",
              palette = pal , 
              breaks= c(-Inf, 1.0, 1.5, 2.1, 2.5, 3.0, 3.5, Inf),
              auto.palette.mapping = FALSE, 
              id = "GEOID", 
              #showNA = TRUE, 
              colorNA = NULL,
              border.col = "black", 
              lwd = 1.5
              #border.alpha = 0.5
              #legend.show = FALSE
              #style ="jenks",
              #n = 6
  ) +
 tm_shape(states) +
  tm_borders(lwd=2, col="black", alpha = 0.5)

results <- read.csv('U:/0 govserv/ADP/Manuscripts/1. Submitted Localized Estiamtes fo the TFR/hmdall_06172017.csv')

my_groba = grobTree(textGrob("a", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobb = grobTree(textGrob("b", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
my_grobc = grobTree(textGrob("c", x=0.1, y=0.95, hjust=0, gp=gpar(col="black", size=6)))
f3b <- ggplot() +
  annotation_custom(my_grobb)+
  geom_line(data=subset(results, Code %in% c("NLD", "SWE", "FRATNP")), aes(x=Year, y=post_mean, color=factor(Code, labels = c("France", "Netherlands", "Sweden")))) +
  geom_ribbon(data=subset(results, Code %in% c("NLD", "SWE", "FRATNP")), aes(x=Year, ymin=Q10, ymax=Q90,fill=factor(Code, labels = c("France", "Netherlands", "Sweden"))), alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data=subset(results, Code =="NLD" & Year <=1950), aes(x=Year, ymin=Q10, ymax=Q90), fill="green", alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data=subset(results, Code =="SWE" & Year <=1891), aes(x=Year, ymin=Q10, ymax=Q90), fill="blue", alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data=subset(results, Code =="FRATNP" & Year <=1946), aes(x=Year, ymin=Q10, ymax=Q90), fill="red", alpha=0.2, show.legend = FALSE) +
  theme_bw() +
  theme(text=element_text(face='bold')) +
  theme(legend.position=c(0.87,0.8)) +
  labs(x='Year', y='BayesTFR', color = "Country") +
  geom_vline(xintercept=1950,lty=1, lwd=1, color="Green") +
  geom_vline(xintercept=1891,lty=1, lwd=1, color="Blue") +
  geom_vline(xintercept=1946,lty=1, lwd=1, color="Red") +
  scale_x_continuous(breaks = c(1750,1800,1850, 1900, 1950, 2000))

income <-read.xlsx("U:/0 govserv/ADP/Manuscripts/1. Schmertmann iTFR/test_income_itfr_06222017.xlsx")

incomelabels <- c("0-24", "25-49", "50-74", "75-99", "100-124", "125-149", "150-199", "200-249", "250-499", "500-999", "1000+")

f3c <- ggplot() +
  annotation_custom(my_grobc) +
  geom_line(data=income, aes(x=Income.Bracket, y=iTFR)) +
  ylim(0,+2.50) +
  theme_bw() +
  #theme(aspect.ratio=1) +
  labs(x="Income (in 000s)")+
  theme(text=element_text(face='bold'), axis.text.x = element_text(angle=45, hjust=1)) +
  scale_x_discrete(limits=incomelabels)

b<- grid.arrange(f3b, f3c, ncol=2)
grid.arrange(a, b, ncol=1)

grid.newpage()
pushViewport(viewport(layout=grid.layout(3,2)))
print(f3b, vp = viewport(layout.pos.col = 1, layout.pos.row = c(3)))
print(f3c, vp = viewport(layout.pos.col =2, layout.pos.row =c(3)))

print(a, vp = viewport(layout.pos.row =c(1,2)))