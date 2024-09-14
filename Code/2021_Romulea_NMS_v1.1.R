## updating the Roxanne code from 2018 to improve plots and change some of the species

###Load Packages and data.


suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(broom))
suppressMessages(library(lme4))
suppressMessages(library(car))
suppressMessages(library(tidyr))
suppressMessages(library(effects))
suppressMessages(library(mgcv))
suppressMessages(library(parallel))
suppressMessages(library(ggeffects))
suppressMessages(library(lubridate))
suppressMessages(library(vegan))
suppressMessages(library(knitr))
suppressMessages(library(magrittr))
library(readr)
library(sjPlot)


FullNMDS <- read_csv("Data/NMDS2.csv")  ## new dataset with 2016 all niman grazed recoded per DV
## data is part of total cover

# need to deal with total





# remove sum column
FullNMDS <- FullNMDS %>% select(-Total)
## use *.1 to put at end of data frame
## Romulea is an EPF.
#View(FullNMDS)


FullNMDS.1 <- FullNMDS %>%
 mutate(NAF.1 = rowSums(select(., starts_with("NAF")))) %>%
 mutate(NPG.1 = rowSums(select(., starts_with("NPG")))) %>%
 mutate(Unknown.1 = rowSums(select(., starts_with("Unknown")))) %>%
 mutate(EAF.1 = rowSums(select(., starts_with("EAF")))) %>%
 mutate(EAG.1 = rowSums(select(., starts_with("EAG")))) %>%
 mutate(NS.1 = rowSums(select(., starts_with("NS")))) %>%
  mutate(NPRuS.1 = rowSums(select(., starts_with("NPRuS")))) %>% # reeds and sedges
  mutate(NARuS.1 = rowSums(select(., starts_with("NARuS")))) %>% #  reeds and sedges
  mutate(NPF.1 = rowSums(select(., starts_with("NPF")))) %>%
  mutate(EPG.1 = rowSums(select(., starts_with("EPG")))) %>%
  mutate(EPF.1 = rowSums(select(., starts_with("EPF")))) %>%  ## were EPF_7, but separated Rolumea so now only 6
 mutate(Romulea.1 = rowSums(select(., starts_with("Romulea")))) # subtract Romulea.1 from EPF.1 
  
  
## Keep columns 1:14 and 122:133
FullNMDS.2 <- FullNMDS.1 %>%
  select(-c(15:121))

## convert to integer
FullNMDS.2$NPF.1 <- as.integer(FullNMDS.2$NPF.1)

## remove *.1
FullNMDS.2 <- FullNMDS.2 %>%
  rename(NAF = NAF.1) %>%
  rename(NPG = NPG.1) %>%
  rename(Unknown = Unknown.1) %>%
  rename(EAF = EAF.1) %>%
  rename(EAG = EAG.1) %>%
  rename(NS = NS.1) %>%
  rename(NPRuS = NPRuS.1) %>%
  rename(NARuS = NARuS.1) %>%
  rename(NPF = NPF.1) %>%
  rename(EPG = EPG.1) %>%
  rename(EPF = EPF.1) %>%
  rename(Romulea = Romulea.1)

####
## NEED TO MAKE A UNIQUE ID FOR GROUPS, PROBABLY YEAR|GRAZED|HERBICIDE


## make seeding only count for 2018, check with Roxanne if could have impacted 2017
FullNMDS.2$Seeded <- ifelse(FullNMDS.2$Year != 2016, FullNMDS.2$Seeded, "U")

## make fencing only count for 2017-2018
FullNMDS.2$Grazed <- ifelse(FullNMDS.2$Year != 2016, FullNMDS.2$Grazed, "N")

## make herbicide only applied in 2017-2018
FullNMDS.2$Herbicide <- ifelse(FullNMDS.2$Year != 2016, FullNMDS.2$Herbicide, "N")

FullNMDS.2 <- FullNMDS.2 %>% mutate(YearGrazHerb = paste(Year, Grazed, Herbicide, sep = "-"))
FullNMDS.2 <- FullNMDS.2 %>% mutate(YearHerb = paste(Year, Herbicide, sep = "-"))

## convert to factors (if do this before ifelses above, subsitutes numbers instead of letters !!)
FullNMDS.2$Year <- as.factor(FullNMDS.2$Year)
FullNMDS.2$Grazed <- as.factor(FullNMDS.2$Grazed)
FullNMDS.2$Seeded <- as.factor(FullNMDS.2$Seeded)
FullNMDS.2$Herbicide <- as.factor(FullNMDS.2$Herbicide)
FullNMDS.2$Gopher <- as.factor(FullNMDS.2$Gopher)
FullNMDS.2$Herbicide <- as.factor(FullNMDS.2$Herbicide)

## Break into 4 analysis units
#Cheda Erodium
#Cheda Distaff
#Genazzi Purple Starthistle (fix spelling)
#Niman Romulea

NimanRomulea <- FullNMDS.2 %>% filter(Ranch == "Niman" & FocalSpecies == "Romulea rosea")
# ChedaErodium <- FullNMDS.2 %>% filter(Ranch == "Cheda" & FocalSpecies == "Erodium")
# ChedaDistaff <- FullNMDS.2 %>% filter(Ranch == "Cheda" & FocalSpecies == "Distaff")
# GenazziPurpStar <- FullNMDS.2 %>% filter(Ranch == "Genazzi" & FocalSpecies == "Purple starthistle")

## sum rows checks ok

## add Rainfaill
## rain from DV for cover data NMS
# 2015	32.23
# 2016	37.98
# 2017	55.93
# 2018	28.55
# 
# and if adding univariate
# 2019  48.93
# 2020	21.9
unique(NimanRomulea$Year)

NimanRomulea$rain <- ifelse(NimanRomulea$Year == 2016, 37.98, 
                            ifelse(NimanRomulea$Year == 2017, 55.93, 
                                   28.55))

NimanRomulea$rain_prior <- ifelse(NimanRomulea$Year == 2016, 32.23, 
                            ifelse(NimanRomulea$Year == 2017, 37.98, 
                                   55.93))

NimanRomulea$Oct_Dec_Rain <- ifelse(NimanRomulea$Year == 2016, 13.4, 
                                     ifelse(NimanRomulea$Year == 2017, 18.25, 
                                            6.03))
                                                  

summary(NimanRomulea)

## split into data and .env files

NimanRomulea.env <- NimanRomulea %>% select(c(1:9, 27:30))
NimanRomulea.cover <- NimanRomulea %>% select(c(10:26))


summary(NimanRomulea.cover)

### REMOVE COWPIE !!  TO DO 

# ChedaErodium.env <- ChedaErodium %>% select(c(1:9, 27))
# ChedaErodium.cover <- ChedaErodium %>% select(c(10:26))
# 
# ChedaDistaff.env <- ChedaDistaff %>% select(c(1:9, 27))
# ChedaDistaff.cover <- ChedaDistaff %>% select(c(10:26))
# 
# GenazziPurpStar.env <- GenazziPurpStar %>% select(c(1:9, 27))
# GenazziPurpStar.cover <- GenazziPurpStar %>% select(c(10:26))


## Analyses

### Niman Romulea experiment

##First, we perform an anova (`adonis`) on the full dataset.

## NIMAN ROMULEA (adonis 2 gets same results)
adonis.NimanRomulea.Year <-
  adonis(NimanRomulea.cover ~ Herbicide * Grazed * Year, 
         data = NimanRomulea,
         permutations = 1000,
         parallel = 16)
adonis.NimanRomulea.Year$aov.tab  # year -> r2 = 0.27  total r2 = 0.39

adonis.NimanRomulea.Rain <-
  adonis(NimanRomulea.cover ~ Herbicide + Grazed + rain,
         data = NimanRomulea,
         permutations = 1000,
         parallel = 16)
adonis.NimanRomulea.Rain$aov.tab  # rain -> r2 = 0.10   total r2 = 0.39

adonis.NimanRomulea.Rain_prior <-
  adonis(NimanRomulea.cover ~ Herbicide + Grazed + rain_prior,
         data = NimanRomulea,
         permutations = 1000,
         parallel = 16)
adonis.NimanRomulea.Rain_prior$aov.tab # rain_prioe -> r2 = 0.22 total r2 = 0.39

adonis.NimanRomulea.year_rainprior <-
  adonis(NimanRomulea.cover ~ Herbicide + Grazed + Year + rain_prior,
         data = NimanRomulea,
         permutations = 1000,
         parallel = 16)
adonis.NimanRomulea.year_rainprior$aov.tab #total r2 = 0.49

adonis.NimanRomulea.year_Oct_Dec_Rain <-
  adonis(NimanRomulea.cover ~ Herbicide + Grazed + Year + Oct_Dec_Rain,
         data = NimanRomulea,
         permutations = 1000,
         parallel = 16)
adonis.NimanRomulea.year_Oct_Dec_Rain$aov.tab # oct-dec_rain -> r2 = 0.05   #total r2 = 0.49



## test for dispersion per gavin
## differences in dispersion by groups can be problematic for adonis

dis <- vegdist(NimanRomulea.cover) # Bray-Curtis distances
groups <- NimanRomulea$YearGrazHerb
mod <- betadisper(dis, groups)
mod  # check average distance to median .. hopefully not too different
permutest(mod)  # gives a p value if any are different
permutest(mod, pairwise = TRUE)  # same


# NMS
ord.NimanRomulea <- metaMDS(NimanRomulea.cover, trymax = 50)
ord.NimanRomulea

#
stressplot(ord.NimanRomulea)

# fit variables to ord

NimanRomulea.env$Year <- as.numeric(NimanRomulea.env$Year)
envForFit.Year <- NimanRomulea.env[c(1,6,8)]
envForFit.Rain <- NimanRomulea.env[c(11,6,8)]
envForFit.Rain_lag <- NimanRomulea.env[c(12,6,8)]


fit.Year <- envfit(ord.NimanRomulea, envForFit.Year, perm = 999)
scores(fit.Year, "vectors")
fit.Year

fit.Rain <- envfit(ord.NimanRomulea, envForFit.Rain, perm = 999)
scores(fit.Rain, "vectors")
fit.Rain

fit.Rain_lag <- envfit(ord.NimanRomulea, envForFit.Rain_lag, perm = 999)
scores(fit.Rain_lag, "vectors")
fit.Rain_lag






#           NMDS1   NMDS2    r2 Pr(>r)    
# Year 0.97215 0.23438 0.743  0.001 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Permutation: free
# Number of permutations: 999
# 
# ***FACTORS:
#   
#   Centroids:
#              NMDS1   NMDS2
# GrazedG     0.0349  0.0779
# GrazedN    -0.0175 -0.0390
# HerbicideH  0.1728  0.0240
# HerbicideN -0.0864 -0.0120




### example for plotting NMS---------------

library(ggord)

p.nmds.Year <- ggord(ord.NimanRomulea, 
                grp_in = NimanRomulea.env$YearGrazHerb,
                pbslab = FALSE,
                arrow = NULL, #size = 3,
                alpha_el = 0.2, 
                ptslab = TRUE,  # darkness of polygon
                poly = TRUE,
                labcol = "black",
                ellipse = TRUE,
                ellipse_pro = 0.6,      # confidence intervals for ellipse
                grp_title = "Year-Grazed-Herbicide",  
                repel = TRUE,            # make text not overlap
                txt = 4,                  # size of text
                #cols = c('purple', 'red', 'purple', 'red', 'red', 'yellow', 'red', 'purple', 'red', 'yellow'),
                facet = FALSE,
                xlims = c(-1,1.2),
                ylims = c(-0.75,1)) +  ## removes unknown, and cowpie from chart
  #theme_gray(base_size = 18) +
  geom_segment(x = 0, y = 0, xend = 0.978, yend = 0.21, arrow = arrow(), size = 1, color = "blue") +  # YEAR
  annotate("text", label = "YEAR", x = 0.75, y =  0.25, colour = "blue", size = 3, fontface = 3) + 
  annotate("text", label = "GRAZED", x = 0.03, y =  0.09, colour = "blue", size = 3, fontface = 3) + 
  annotate("text", label = "UNGRAZED", x = -0.02, y = -0.04, colour = "blue", size = 3, fontface = 3) +     
  annotate("text", label = "TREATED", x = 0.16, y = 0.03, colour = "blue", size = 3, fontface = 3) + 
  annotate("text", label = "UNTREATED", x = -0.08, y = -0.02, colour = "blue", size = 3, fontface = 3) +
  theme(legend.text=element_text(size=9),
        legend.title=element_text(size=11),
        legend.position = "top") + 
  theme_classic(base_size = 18) 
p.nmds.Year
ggsave("Output/p.NMS.jpg", width = 10, height = 8, units = "in", dpi = 300)  ## save a jpeg.


NimanRomulea.env <- NimanRomulea.env %>% mutate(Rain_LagGrazHerb = paste(rain_prior, Grazed, Herbicide, sep = "-"))




p.nmds.Rain_lag <- ggord(ord.NimanRomulea, 
                     grp_in = NimanRomulea.env$Rain_LagGrazHerb,
                     pbslab = FALSE,
                     arrow = NULL, #size = 3,
                     alpha_el = 0.2, 
                     ptslab = TRUE,  # darkness of polygon
                     poly = TRUE,
                     labcol = "black",
                     ellipse = TRUE,
                     ellipse_pro = 0.6,      # confidence intervals for ellipse
                     grp_title = "Rain Lag-Grazed-Herbicide",  
                     repel = TRUE,            # make text not overlap
                     txt = 4,                  # size of text
                     #cols = c('purple', 'red', 'purple', 'red', 'red', 'yellow', 'red', 'purple', 'red', 'yellow'),
                     facet = FALSE,
                     xlims = c(-1,1.2),
                     ylims = c(-0.75,1)) +  ## removes unknown, and cowpie from chart
  #theme_gray(base_size = 18) +
  geom_segment(x = 0, y = 0, xend = .978, yend = 0.208, arrow = arrow(), size = 1, color = "blue") +  # YEAR
  annotate("text", label = "RAIN LAG", x = 1, y =  0.3, colour = "blue", size = 3, fontface = 3) +
  annotate("text", label = "GRAZED", x = 0.03, y =  0.07, colour = "blue", size = 3, fontface = 3) +
  annotate("text", label = "UNGRAZED", x = -0.02, y = -0.04, colour = "blue", size = 3, fontface = 3) +
  annotate("text", label = "TREATED", x = 0.17, y = 0.02, colour = "blue", size = 3, fontface = 3) +
  annotate("text", label = "UNTREATED", x = -0.09, y = -0.01, colour = "blue", size = 3, fontface = 3) +
  theme(legend.text=element_text(size=9),
        legend.title=element_text(size=11),
        legend.position = "top") + 
  theme_classic(base_size = 18)
p.nmds.Rain_lag
ggsave("Output/p.NMS_rain_lag.jpg", width = 10, height = 8, units = "in", dpi = 300)  ## save a jpeg.

cowplot::plot_grid(p.nmds.Year, p.nmds.Rain_lag)
ggsave("Output/p.NMS_rain_lag.jpg", width = 16, height = 8, units = "in", dpi = 300) 



######### Univariate Niman data for Romulea only-----------------

hist(NimanRomulea$Romulea)

p.Niman.boxplot <- ggplot(NimanRomulea, 
                                  aes(Year, Romulea, fill = Herbicide)) +
  geom_boxplot(alpha = 0.15, outlier.shape = NA) +
  geom_point(position=position_dodge(width=0.75), size = 3, aes(group=Herbicide)) +
  #geom_smooth() +
  #geom_jitter(position=position_dodge(width=0.75), aes(group=Treatment2)) +
  ylab("Romulea cover (%)") + xlab("Year") +
  theme_gray(base_size = 20) +
  scale_fill_manual(values = c('red', 'blue')) +
  scale_y_continuous(limits = c(0,100)) +
  theme(legend.position = c(0.1, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  facet_wrap(.~Grazed)
p.Niman.boxplot
ggsave("Output/NimanRawDataBoxplot.jpg", width = 12, height = 5, units = "in", dpi = 300)  ## save a jpeg.


cowplot::plot_grid(p.Niman.boxplot, p.HomePlusNiman.boxplot)


m6.niman.bin <- glmer(cbind(Romulea, 100) ~ Year + Herbicide + Oct_Dec_Rain + (1|Plot), 
                family = binomial, data = NimanRomulea)

summary(m6.niman.bin)


BESTMODEL <- m6.niman.bin

plot(BESTMODEL)
performance::r2(BESTMODEL)
p.BEST_MODEL.Niman <- plot_model(BESTMODEL, type = "eff", 
                                terms = c("Year", "Herbicide"), #"Field"), 
                                #terms = c("YearsSinceTreat.f", "Treatment2", "Site"), 
                                #terms = c("Oct_Dec_Rain [6, 12, 18]", "Treatment2", "YearsSinceTreat.f"),
                                #terms = c("Year.f", "Oct_Dec_Rain"), 
                                ci.lvl = 0.8, add.data = TRUE) +  # "eff" not working
  #labs(title = "Model predictions - Years since treatment") +
  #geom_point(data = DATA, mapping = aes(x=Year, y = CoverRomulea, shape = Site)) +
  ggtitle(NULL) + 
  theme_bw(base_size = 18) +
  xlab("Years since treatment") +
  ylab("Romulea Cover") +
  #ylim(0,.5) + 
  theme(legend.position = c(0.2, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  geom_vline(xintercept = 0.5, lty = 2)
p.BEST_MODEL.Niman
ggsave("Output/RomuleaCoverBestModel_Niman.jpg", width = 10, height = 8, units = "in", dpi = 300)  ## save a jpeg.



## CCA
NimanRolumeaCCA <- cca(NimanRomulea.cover ~ Herbicide + Grazed  + Oct_Dec_Rain + Year, 
                       #Herbicide + Grazed + Year + rain_prior + Oct_Dec_Rain,
                       data = NimanRomulea)

summary(NimanRolumeaCCA)
NimanRolumeaCCA
plot(NimanRolumeaCCA)

anova(NimanRolumeaCCA)
anova(NimanRolumeaCCA, by = "axis")
anova(NimanRolumeaCCA, by = "term", permutations = 999)


p.CCA <- ggord(NimanRolumeaCCA, 
                     grp_in = NimanRomulea.env$YearHerb, #YearGrazHerb
                     pbslab = FALSE,
                     arrow = 0.5, #size = 3,
                     veclsz = 1.25, # line size on vectors
                     ext = 1.3, # distance of labels from arrows
                     alpha_el = 0.3, # darkness of polygon
                     ptslab = TRUE,  
                     poly = TRUE,
                     labcol = "black",
                     ellipse = TRUE,
                     ellipse_pro = 0.75,      # confidence intervals for ellipse
                     grp_title = "Year-Herbicide",  
                     repel = TRUE,            # make text not overlap
                     txt = 5,                  # size of text
                     size = c(7),                 # point size
                     #cols = c('purple', 'red', 'purple', 'red', 'red', 'yellow', 'red', 'purple', 'red', 'yellow'),
                     facet = FALSE,
                     xlims = c(-3.5,3.5),
                     ylims = c(-3.5,4.5)) +  ## removes unknown, and cowpie from chart
  #theme_gray(base_size = 18) +
  # geom_segment(x = 0, y = 0, xend = 0.978, yend = 0.21, arrow = arrow(), size = 1, color = "blue") +  # YEAR
  # annotate("text", label = "YEAR", x = 0.75, y =  0.25, colour = "blue", size = 3, fontface = 3) + 
  # annotate("text", label = "GRAZED", x = 0.03, y =  0.09, colour = "blue", size = 3, fontface = 3) + 
  # annotate("text", label = "UNGRAZED", x = -0.02, y = -0.04, colour = "blue", size = 3, fontface = 3) +     
  # annotate("text", label = "TREATED", x = 0.16, y = 0.03, colour = "blue", size = 3, fontface = 3) + 
  # annotate("text", label = "UNTREATED", x = -0.08, y = -0.02, colour = "blue", size = 3, fontface = 3) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
 # theme(legend.text=element_text(size=9),
  #      legend.title=element_text(size=11),
   #     legend.position = c(2,3)) + 
  scale_shape_manual('Groups', values = c(21, 22, 21, 22, 21)) +
  scale_fill_manual('Groups', values = c("pink", "light green", "lightgreen", "light blue", "light blue")) +
  theme_classic(base_size = 18)
p.CCA
ggsave("Report/p.CCA.jpg", width = 12, height = 10, units = "in", dpi = 300)  


#include grazing 
# not used since grazing only 6% in adonis model
p.CCA.graz <- ggord(NimanRolumeaCCA, 
               grp_in = NimanRomulea.env$YearGrazHerb,
               pbslab = FALSE,
               arrow = 0.5, #size = 3,
               veclsz = 1.25, # line size on vectors
               ext = 1.3, # distance of labels from arrows
               alpha_el = 0.2, # darkness of polygon
               ptslab = TRUE,  
               poly = TRUE,
               labcol = "black",
               ellipse = TRUE,
               ellipse_pro = 0.75,      # confidence intervals for ellipse
               grp_title = "Year-Grazed-Herbicide",  
               repel = TRUE,            # make text not overlap
               txt = 5,                  # size of text
               size = c(7),                 # point size
               #cols = c('purple', 'red', 'purple', 'red', 'red', 'yellow', 'red', 'purple', 'red', 'yellow'),
               facet = FALSE,
               xlims = c(-3.5,3.5),
               ylims = c(-3.5,4.5)) +  ## removes unknown, and cowpie from chart
  #theme_gray(base_size = 18) +
  # geom_segment(x = 0, y = 0, xend = 0.978, yend = 0.21, arrow = arrow(), size = 1, color = "blue") +  # YEAR
  # annotate("text", label = "YEAR", x = 0.75, y =  0.25, colour = "blue", size = 3, fontface = 3) + 
  # annotate("text", label = "GRAZED", x = 0.03, y =  0.09, colour = "blue", size = 3, fontface = 3) + 
  # annotate("text", label = "UNGRAZED", x = -0.02, y = -0.04, colour = "blue", size = 3, fontface = 3) +     
  # annotate("text", label = "TREATED", x = 0.16, y = 0.03, colour = "blue", size = 3, fontface = 3) + 
  # annotate("text", label = "UNTREATED", x = -0.08, y = -0.02, colour = "blue", size = 3, fontface = 3) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  # theme(legend.text=element_text(size=9),
  #      legend.title=element_text(size=11),
  #     legend.position = c(2,3)) + 
  scale_shape_manual('Groups', values = c(21, 22, 21, 22, 21,22, 21, 22, 21)) +
  theme_classic(base_size = 18)
p.CCA.graz


#################################
