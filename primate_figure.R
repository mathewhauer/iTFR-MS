
rm(list=ls())
library(tidyverse)
library(ggrepel)

PRIM_DATA <- read_csv("C:/Users/Matt/Documents/Matt - Work Stuff/r code/myrepo/iTFR-MS/Bronikowski.LifeTables.SciData2016.csv")

d <- PRIM_DATA %>%
  mutate(child = N.x.female + N.x.male,
         nFx = Offspring / Breeding_females,
         nFx = ifelse(is.na(nFx),0, nFx))

children <- d %>%
  filter(Age == 0) %>%
  select(Species, child)

women <- d %>%
  group_by(Species) %>%
  summarize(Women = sum(N.x.female[fertint > 0]),
            fertwidth = sum(fertint))

TFR_table <- d %>%
  group_by(Species) %>%
  summarize(TFR_table = sum(nFx))

cw <- inner_join(children, women)

combined <- inner_join(cw, TFR_table) %>%
  mutate(iTFR = (child/Women) * fertwidth)

ggplot(combined, aes(x = TFR_table, y = iTFR)) +
  geom_point(shape = 19, color = 'gray', size = 3) +
  geom_text_repel(aes(label = Species), hjust = 0, vjust = 0) +
  theme_bw() +
  geom_abline(slope = 1) +
  xlim(5, 11) +
  ylim(5, 11) +
  theme(text = element_text(face = 'bold')) +
  geom_line(aes(x = TFR_table, y = 0.9 * TFR_table), lty = 2, lwd = 1, col = 'black') +
  geom_line(aes(x = TFR_table, y = 1.1 * TFR_table), lty = 2, lwd = 1, col = 'black') +
  labs(x = 'TFR(Data',
       y = 'p(TFR)')
