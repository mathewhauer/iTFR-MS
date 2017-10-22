rm(list=ls())
library(grid)
library(gridExtra)
library(tidyverse)
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

z = read.csv("C:/Users/Matt/Documents/Matt - Work Stuff/r code/myrepo/iTFR-MS/bayesitfr_hmdhfd3.csv") %>%
  select(Code, Year, iTFR, lagTFR, bayesTFR=post_mean)

bayesTFR = read_csv("C:/Users/Matt/Documents/Matt - Work Stuff/r code/myrepo/iTFR-MS/bayesitfr_hmdhfd3.csv")
xitfr = read_csv("C:/Users/Matt/Documents/Matt - Work Stuff/r code/myrepo/iTFR-MS/xTFR_06082017.csv")


join = inner_join(xitfr, z, c("Code", "Year"))

join = join %>%
  select(Code, Year, iTFR=iTFR.x, xTFR, C,lagTFR=lagTFR.x, bayesTFR)

f1a <- ggplot(data=join, aes(x=lagTFR, y=iTFR)) +
  geom_text(x=1.5, y=3.5, label="iTFR", col='red', size=6)+
  annotation_custom(my_groba)+
  geom_point(size=1, alpha=.50,color='gray') +
  geom_abline(intercept=0,slope=1) +
  theme_bw() +
  theme(legend.position="none")+
  theme(text=element_text(face='bold')) +
  xlim(0.9,4.1) + ylim(0.9,4.1) +
  labs(x='TFR (Data)', y='p(TFR)') +
  geom_line(aes(x=lagTFR, y= 0.90 * lagTFR), lty=2, lwd=1, col='black') +
  geom_line(aes(x=lagTFR, y= 1.10 * lagTFR), lty=2, lwd=1, col='black') 
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
  labs(x='TFR (Data)', y='p(TFR)') 
f1c <- ggplot(data=join, aes(x=lagTFR, y=bayesTFR, text = paste(Code))) +
  geom_text(x=2, y=3.5, label="BayesTFR", col='green', size=6)+
  annotation_custom(my_grobc)+
  geom_point(size=1, alpha=.50,color='gray') +
  geom_abline(intercept=0,slope=1) +
  theme_bw() +
  theme(text=element_text(face='bold')) +
  xlim(0.9,4.1) + ylim(0.9,4.1) +
  labs(x='TFR (Data)', y='p(TFR)') +
  geom_line(aes(x=lagTFR, y= 0.90 * lagTFR), lty=2, lwd=1, col='black') +
  geom_line(aes(x=lagTFR, y= 1.10 * lagTFR), lty=2, lwd=1, col='black')  


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
  labs( x='Population, n', y='M(TFR)') 
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
  labs(x='Population, n', y='M(TFR)')
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
  labs( x='Population, n', y='M(TFR)')


f1g<- ggplot(data=join, aes(x=Year, y=(iTFR-lagTFR)/lagTFR*100)) +
  annotation_custom(my_grobg)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  theme(text=element_text(face='bold')) +
  xlim(1891,2015) + ylim(-30,30) +
  labs(x='Time, t', y='M(TFR)') 
f1h<- ggplot(data=join, aes(x=Year, y=(xTFR-lagTFR)/lagTFR*100)) +
  annotation_custom(my_grobh)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  theme(text=element_text(face='bold')) +
  xlim(1891,2015) + ylim(-30,30) +
  labs(x='Time, t', y='M(TFR)') 
f1i<- ggplot(data=join, aes(x=Year, y=(bayesTFR-lagTFR)/lagTFR*100)) +
  annotation_custom(my_grobi)+
  geom_point(size=1, alpha=.50,color='gray')+
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, lty=2, lwd=1)+
  geom_hline(yintercept = -10, lty=2, lwd=1)+
  theme_bw() +
  theme(text=element_text(face='bold')) +
  xlim(1891,2015) + ylim(-30,30) +
  labs(x='Time, t', y='M(TFR)') 


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
  theme(text=element_text(face='bold')) +
  labs(x='Algebraic(error)',
       y='Density')
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
  theme(text=element_text(face='bold')) +
  labs(x='Absolute Algebraic(error)',
       y='Density')
f1l<- ggplot() +
  annotation_custom(my_grobl)+
  geom_density(data=join, aes(x=abs((iTFR-lagTFR)/lagTFR)*100), adjust=1.5,fill='red',col='red', lwd=1, alpha=.20) +
  geom_density(data=join, aes(x=abs((bayesTFR-lagTFR)/lagTFR)*100),adjust=1.5, fill='green',col='green', lwd=1, alpha=.20) +
  geom_density(data=join, aes(x=abs((xTFR-lagTFR)/lagTFR)*100), adjust=1.5, fill="blue", col="blue", lwd=1, alpha=.20)+
  geom_vline(xintercept=0) +
  theme_bw() +
  geom_vline(data=join, aes(xintercept=quantile(abs((iTFR-lagTFR)/lagTFR)*100, 0.5, na.rm=T)), lty=1, lwd=0.5, color="red") +
  geom_vline(data=join, aes(xintercept=quantile(abs((xTFR-lagTFR)/lagTFR)*100, 0.5, na.rm=T)), lty=1, lwd=0.5, color="blue") +
  geom_vline(data=join, aes(xintercept=quantile(abs((bayesTFR-lagTFR)/lagTFR)*100, 0.5, na.rm=T)), lty=1, lwd=0.5, color="green")+
  geom_vline(data=join, aes(xintercept=quantile(abs((iTFR-lagTFR)/lagTFR)*100, 0.9, na.rm=T)), lty=2, lwd=0.5, color="red")+
  geom_vline(data=join, aes(xintercept=quantile(abs((xTFR-lagTFR)/lagTFR)*100, 0.9, na.rm=T)), lty=2, lwd=0.5, color="blue")+
  geom_vline(data=join, aes(xintercept=quantile(abs((bayesTFR-lagTFR)/lagTFR)*100, 0.9, na.rm=T)), lty=2, lwd=0.5, color="green")+
  theme(text=element_text(face='bold')) +
  labs(x='Absolute Percent(error)',
       y='Density')



grid.arrange(f1a,f1b,f1c,f1d, f1e, f1f, f1g, f1h, f1i, f1j, f1k, f1l, ncol=3)