library(ggplot2)
library(grid)
library(gridExtra)
packages(HMDHFDplus)
library(HMDHFDplus)
library(scales)
library(dplyr)

my_groba = grobTree(textGrob("a", x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))
my_grobb = grobTree(textGrob("b", x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))
my_grobc = grobTree(textGrob("c", x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))
my_grobd = grobTree(textGrob("d",x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))
my_grobe = grobTree(textGrob("e", x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))
my_grobf = grobTree(textGrob("f", x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))
my_grobg = grobTree(textGrob("g", x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))
my_grobh = grobTree(textGrob("h", x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))
my_grobi = grobTree(textGrob("i", x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))
my_grobj = grobTree(textGrob("j", x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))
my_grobk = grobTree(textGrob("k", x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))
my_grobl = grobTree(textGrob("l", x=0.9, y=0.1, hjust=0, gp=gpar(col="black", size=6)))

z = read.csv("U:/0 govserv/ADP/Manuscripts/1. Submitted Localized Estiamtes fo the TFR/bayesitfr_hmdhfd3.csv") %>%
  select(Code, Year, iTFR, lagTFR, bayesTFR=post_mean)

bayesTFR = read.table("U:/0 govserv/ADP/Manuscripts/1. Submitted Localized Estiamtes fo the TFR/bayesitfr_hmdhfd2.txt", sep="\t", header=TRUE)
xitfr = read.table("U:/0 govserv/ADP/Manuscripts/1. Submitted Localized Estiamtes fo the TFR/xitfr.txt", sep = "\t", header = TRUE)


join = inner_join(xitfr, z, c("Code", "Year"))

join = join %>%
  select(Code, Year, iTFR=iTFR.x, xTFR, C,lagTFR=lagTFR.x, bayesTFR)

bayesTFR = bayesTFR %>%
  mutate(           bayeserr = bayesTFR-lagTFR,
          bayespct = (bayeserr/lagTFR),
          absbpct = abs(bayespct),
          absberr = abs(bayeserr)
          )

countyeval = countyeval %>%
  mutate( xTFR = (coef(reg)[1] + coef(reg)[2] * p2534) * CWR,
          xerr = xTFR-TFR,
          xpct = (xerr/TFR),
          ierr = iTFR-TFR,
          ipct = (ierr/TFR),
          absi=abs(xerr))



factsi = subset(tmp) %>% 
  #group_by(ierr) %>% 
  summarize(
    p10a=quantile((iTFR-lagTFR), 0.1, na.rm=T), 
    p90a=quantile((iTFR-lagTFR), 0.9, na.rm=T),
    p10p=quantile((iTFR-lagTFR)/lagTFR, 0.1, na.rm=T),
    p90p=quantile((iTFR-lagTFR)/lagTFR, 0.9, na.rm=T),
    abs90p=quantile(abs((iTFR-lagTFR)/lagTFR),0.9, na.rm=T),
    abs50p=quantile(abs((iTFR-lagTFR)/lagTFR),0.5, na.rm=T),
    meanbayesalgeerr = mean(abs((iTFR-lagTFR)), na.rm=T),
    p90meanalge = quantile(abs((iTFR-lagTFR)), 0.90, na.rm=T)
  )
factsb = subset(bayesTFR) %>% 
  #group_by(ierr) %>% 
  summarize(
    p10a=quantile(bayeserr, 0.1, na.rm=T), 
    p90a=quantile(bayeserr, 0.9, na.rm=T),
    p10p=quantile(bayespct, 0.1, na.rm=T), 
    p90p=quantile(bayespct, 0.9, na.rm=T),
    abs90p=quantile(absbpct, 0.9, na.rm=T),
    abs50p=quantile(abs(absbpct),0.5, na.rm=T),
    meanbayesalgeerr = mean(absberr),
    p90meanalge = quantile(abs(bayeserr), 0.90, na.rm=T)
  )

factsx = sample_out %>% 
  #group_by(ierr) %>% 
  summarize(
    p10a=quantile(xerr, 0.1, na.rm=T), 
    p90a=quantile(xerr, 0.9, na.rm=T),
    p10p=quantile(xpct, 0.1, na.rm=T), 
    p90p=quantile(xpct, 0.9, na.rm=T),
    abs90=quantile(abs(xpct), 0.9, na.rm=T),
    abs50p=quantile(abs(xpct),0.5, na.rm=T),
    meanalgeerr = mean(abs(xerr), na.rm=T),
    p90meanalge = quantile(abs(xerr), 0.90, na.rm=T)
  )

factscountyi = countyeval %>% 
  #group_by(ierr) %>% 
  summarize(
    p10a=quantile(ierr, 0.1, na.rm=T), 
    p90a=quantile(ierr, 0.9, na.rm=T),
    p10p=quantile(ipct, 0.1, na.rm=T),
    p90p=quantile(ipct, 0.9, na.rm=T)
  )
factscountyx = countyeval %>% 
  #group_by(ierr) %>% 
  summarize(
    p10a=quantile(xerr, 0.1, na.rm=T), 
    p90a=quantile(xerr, 0.9, na.rm=T),
    p10p=quantile(xpct, 0.1, na.rm=T), 
    p90p=quantile(xpct, 0.9, na.rm=T)
  )

f1a <- ggplot(data=join, aes(x=lagTFR, y=iTFR)) +
  geom_text(x=1.5, y=3.5, label="iTFR", col='red', size=6)+
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

ggplotly(f1a)

f1c <- ggplot(data=join, aes(x=lagTFR, y=bayesTFR, text = paste(Code))) +
  geom_text(x=2, y=3.5, label="BayesTFR", col='green', size=6)+
  annotation_custom(my_grobc)+
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

f1b <- ggplot(data=join, aes(x=lagTFR, y=xTFR, text = paste(Code, Year))) +
  geom_text(x=1.5, y=3.5, label="xTFR", col='blue', size=6)+
  annotation_custom(my_grobb)+
  geom_point(size=1, alpha=.50,color='gray') +
  geom_abline(intercept=0,slope=1) +
  geom_abline(intercept=0, slope = 0.9, lty=2, lwd=1) +
  geom_abline(intercept=0, slope = 1.1, lty=2, lwd=1) +
  theme_bw() +
  theme(text=element_text(face='bold')) +
  xlim(0.9,4.1) + ylim(0.9,4.1) +
  labs(#caption='Source: Human Fertility Database 1950+', 
    #title='Actual TFR vs. Estimate from Age-Sex Pyramid',
    x='TFR (Data)', y='p(TFR)') 
  #geom_line(aes(x=lagTFR, y= 0.90 * lagTFR), lty=2, lwd=1, col='black') +
  #geom_line(aes(x=lagTFR, y= 1.10 * lagTFR), lty=2, lwd=1, col='black') 
# geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
#geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)

ggplotly(f1b)

f1d <- ggplot(data=join, aes(x=C, y=(iTFR-lagTFR)/lagTFR*100)) +
  annotation_custom(my_grobd)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
    theme(text=element_text(face='bold')) +
  ylim(-30,30) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(#caption='Source: Human Fertility Database 1950+', 
    #title='Algebraic Percent Error',
    x='Population, n', y='M(TFR)') 
#geom_line(aes(x=lagTFR, y= 0.90 * lagTFR), lty=2, lwd=1, col='black') +
#geom_line(aes(x=lagTFR, y= 1.10 * lagTFR), lty=2, lwd=1, col='black')
#geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
#geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)

f1f <- ggplot(data=join, aes(x=C, y=(bayesTFR-lagTFR)/lagTFR*100)) +
  annotation_custom(my_grobf)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme(text=element_text(face='bold')) +
  ylim(-30,30) +
  labs(#caption='Source: Human Fertility Database 1950+', 
    #title='Algebraic Percent Error',
    x='Population, n', y='M(TFR)')
#geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
#geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)

f1e<-ggplot(data=join, aes(x=C, y=(xTFR-lagTFR)/lagTFR*100)) +
  annotation_custom(my_grobe)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme(text=element_text(face='bold')) +
  ylim(-30,30) +
  labs(#caption='Source: Human Fertility Database 1950+', 
    #title='Algebraic Percent Error',
    x='Population, n', y='M(TFR)')

#geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
#geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)


f1g<- ggplot(data=join, aes(x=Year, y=(iTFR-lagTFR)/lagTFR*100)) +
  annotation_custom(my_grobg)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  theme(text=element_text(face='bold')) +
  xlim(1891,2015) + ylim(-30,30) +
  labs(#caption='Source: Human Fertility Database 1950+', 
    #title='Algebraic Percent Error',
    x='Time, t', y='M(TFR)') 
#geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
#geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)

f1i<- ggplot(data=join, aes(x=Year, y=(bayesTFR-lagTFR)/lagTFR*100)) +
  annotation_custom(my_grobi)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  theme(text=element_text(face='bold')) +
  xlim(1891,2015) + ylim(-30,30) +
  labs(#caption='Source: Human Fertility Database 1950+', 
    #title='Algebraic Percent Error',
    x='Time, t', y='M(TFR)') 
#geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
#geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)

f1h<- ggplot(data=join, aes(x=Year, y=(xTFR-lagTFR)/lagTFR*100)) +
  annotation_custom(my_grobh)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  theme(text=element_text(face='bold')) +
  xlim(1891,2015) + ylim(-30,30) +
  labs(#caption='Source: Human Fertility Database 1950+', 
    #title='Algebraic Percent Error',
    x='Time, t', y='M(TFR)') 
#geom_text(x=4.0, y=3.35, label='-10%', col='black', size=6) +
#geom_text(x=3.2, y=3.95, label='+10%', col='black', size=6)


factsx = sample_out %>% 
  #group_by(ierr) %>% 
  summarize(
    p10a=quantile(xerr, 0.1, na.rm=T), 
    p90a=quantile(xerr, 0.9, na.rm=T),
    p10p=quantile(xpct, 0.1, na.rm=T), 
    p90p=quantile(xpct, 0.9, na.rm=T),
    abs90=quantile(abs(xpct), 0.9, na.rm=T),
    meanalgeerr = mean(abs(xerr), na.rm=T)
  )

f1j<- ggplot() +
  annotation_custom(my_grobj)+
  geom_density(data=join, aes(x=iTFR-lagTFR), adjust=1.5,fill='red',col='red', lwd=1, alpha=.20) +
  geom_density(data=join, aes(x=bayesTFR-lagTFR),adjust=1.5, fill='green',col='green', lwd=1, alpha=.20) +
  geom_density(data=join, aes(x=xTFR-lagTFR), adjust=1.5, fill="blue", col="blue", lwd=1, alpha=.20)+
  geom_vline(xintercept=0) +
  theme_bw() +
  xlim(-0.5,0.5) + 
  geom_vline(data=join, aes(xintercept=quantile((iTFR-lagTFR), 0.9, na.rm=T)), lty=2, lwd=0.5, color="red") +
  geom_vline(data=join, aes(xintercept=quantile((iTFR-lagTFR), 0.1, na.rm=T)), lty=2, lwd=0.5, color="red") +
  geom_vline(data=join, aes(xintercept=quantile((bayesTFR-lagTFR), 0.9, na.rm=T)), lty=2, lwd=0.5, color="green")+
  geom_vline(data=join, aes(xintercept=quantile((bayesTFR-lagTFR), 0.1, na.rm=T)), lty=2, lwd=0.5, color="green")+
  geom_vline(data=join, aes(xintercept=quantile((xTFR-lagTFR), 0.9, na.rm=T)), lty=2, lwd=0.5, color="blue")+
  geom_vline(data=join, aes(xintercept=quantile((xTFR-lagTFR), 0.1, na.rm=T)), lty=2, lwd=0.5, color="blue")+
  #geom_vline(xintercept = 0.2, lty=2, lwd=1)+
  #geom_vline(xintercept = -0.2, lty=2, lwd=1)+
  theme(text=element_text(face='bold')) +
  labs(x='Algebraic(error)',
       y='Density'
       # title='Estimation Errors: iTFR (red) versus Age-Structure-Adjusted iTFR (blue)',
       #caption='Human Fertility Database 1950+'
  )


f1k<- ggplot() +
  annotation_custom(my_grobk)+
  geom_density(data=join, aes(x=abs((iTFR-lagTFR))), adjust=1.5,fill='red',col='red', lwd=1, alpha=.20) +
  geom_density(data=join, aes(x=abs((bayesTFR-lagTFR))),adjust=1.5, fill='green',col='green', lwd=1, alpha=.20) +
  geom_density(data=join, aes(x=abs((xTFR-lagTFR))), adjust=1.5, fill="blue", col="blue", lwd=1, alpha=.20)+
  
  geom_vline(xintercept=0) +
  theme_bw() +
  xlim(0,+0.50) + 
  geom_vline(data=join, aes(xintercept=quantile(abs((iTFR-lagTFR)), 0.5, na.rm=T)), lty=1, lwd=0.5, color="red") +
  geom_vline(data=join, aes(xintercept=quantile(abs((xTFR-lagTFR)), 0.5, na.rm=T)), lty=1, lwd=0.5, color="blue") +
  geom_vline(data=join, aes(xintercept=quantile(abs((bayesTFR-lagTFR)), 0.5, na.rm=T)), lty=1, lwd=0.5, color="green")+
  geom_vline(data=join, aes(xintercept=quantile(abs((iTFR-lagTFR)), 0.9, na.rm=T)), lty=2, lwd=0.5, color="red")+
  geom_vline(data=join, aes(xintercept=quantile(abs((xTFR-lagTFR)), 0.9, na.rm=T)), lty=2, lwd=0.5, color="blue")+
  geom_vline(data=join, aes(xintercept=quantile(abs((bayesTFR-lagTFR)), 0.9, na.rm=T)), lty=2, lwd=0.5, color="green")+
  #geom_vline(xintercept = 10, lty=2, lwd=1)+
  #geom_vline(xintercept = -10, lty=2, lwd=1)+
  theme(text=element_text(face='bold')) +
  labs(x='Absolute Algebraic(error)',
       y='Density'
       # title='Estimation Errors: iTFR (red) versus Age-Structure-Adjusted iTFR (blue)',
       #caption='Human Fertility Database 1950+'
  )

f1l<- ggplot() +
  annotation_custom(my_grobl)+
  geom_density(data=join, aes(x=abs((iTFR-lagTFR)/lagTFR)*100), adjust=1.5,fill='red',col='red', lwd=1, alpha=.20) +
  geom_density(data=join, aes(x=abs((bayesTFR-lagTFR)/lagTFR)*100),adjust=1.5, fill='green',col='green', lwd=1, alpha=.20) +
  geom_density(data=join, aes(x=abs((xTFR-lagTFR)/lagTFR)*100), adjust=1.5, fill="blue", col="blue", lwd=1, alpha=.20)+
  
  geom_vline(xintercept=0) +
  theme_bw() +
  #xlim(-.5,+0.50) + 
  geom_vline(data=join, aes(xintercept=quantile(abs((iTFR-lagTFR)/lagTFR)*100, 0.5, na.rm=T)), lty=1, lwd=0.5, color="red") +
  geom_vline(data=join, aes(xintercept=quantile(abs((xTFR-lagTFR)/lagTFR)*100, 0.5, na.rm=T)), lty=1, lwd=0.5, color="blue") +
  geom_vline(data=join, aes(xintercept=quantile(abs((bayesTFR-lagTFR)/lagTFR)*100, 0.5, na.rm=T)), lty=1, lwd=0.5, color="green")+
  geom_vline(data=join, aes(xintercept=quantile(abs((iTFR-lagTFR)/lagTFR)*100, 0.9, na.rm=T)), lty=2, lwd=0.5, color="red")+
  geom_vline(data=join, aes(xintercept=quantile(abs((xTFR-lagTFR)/lagTFR)*100, 0.9, na.rm=T)), lty=2, lwd=0.5, color="blue")+
  geom_vline(data=join, aes(xintercept=quantile(abs((bayesTFR-lagTFR)/lagTFR)*100, 0.9, na.rm=T)), lty=2, lwd=0.5, color="green")+
   #geom_vline(xintercept = 10, lty=2, lwd=1)+
  #geom_vline(xintercept = -10, lty=2, lwd=1)+
  theme(text=element_text(face='bold')) +
  labs(x='Absolute Percent(error)',
       y='Density'
       # title='Estimation Errors: iTFR (red) versus Age-Structure-Adjusted iTFR (blue)',
       #caption='Human Fertility Database 1950+'
  )



grid.arrange(f1a,f1b,f1c,f1d, f1e, f1f, f1g, f1h, f1i, f1j, f1k, f1l, ncol=3, bottom="source: HFD-HMD 1891-2015")
