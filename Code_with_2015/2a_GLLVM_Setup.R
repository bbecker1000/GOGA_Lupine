source("Code_with_2015/1b_NMS_ModelRun.R")

library(gllvm)
library(patchwork)

#check data are ready
wide_data_groupings_2015.nms
data_plot_groupings_2015

data_plot_groupings_2015$Plot <- as.factor(data_plot_groupings_2015$Plot)

#all species ordination (~NMS)
fit_ord_2015 <- gllvm(wide_data_groupings_2015.nms, family = "negative.binomial") #30 sec
fit_ord_2015
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fit_ord, var.colors = 1)

ordiplot(fit_ord_2015, biplot = TRUE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Biplot")
ordiplot(fit_ord_2015, biplot = FALSE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Ordination plot", predict.region = TRUE)

#reset graphics
par(mfrow = c(1, 1))



# FOR YEAR

#neg binomial with covariates
fit_env_2015_year.nb <- gllvm(wide_data_groupings_2015.nms,  # [,c(23, 32, 11, 18, 28, 14)]
                         data_plot_groupings_2015, family = "negative.binomial", 
                         num.lv = 1,
                         formula = ~ Year * Treatment + yearly_rain,
                         seed = 1234)
summary(fit_env_2015_year.nb)
plot(fit_env_2015_year.nb, mfrow=c(3,2))
coefplot(fit_env_2015_year.nb, cex.ylab = 0.7, mar = c(4, 5, 2, 1), mfrow=c(3,3),
         order = FALSE )

#make nicer plots of  fit_env.nb
estimate_2015_year <- data.frame(fit_env_2015_year.nb[["params"]][["Xcoef"]])
estimate_2015_year <- tibble::rownames_to_column(estimate_2015_year, "Species")
estimate.sd_2015_year <- data.frame(fit_env_2015_year.nb[["sd"]][["Xcoef"]])
estimate.sd_2015_year <- tibble::rownames_to_column(estimate.sd_2015_year, "Species.sd")


#make df
estimate.tidy_2015_year <- estimate_2015_year %>% 
  pivot_longer(cols = c(-Species) , names_to = "Covariate", values_to = "Estimate")

estimate.sd.tidy_2015_year <- estimate.sd_2015_year %>% 
  pivot_longer(cols = c(-Species.sd) , names_to = "Covariate", values_to = "Estimate.sd")

glvvm.coef.plot_2015_year <- tibble(cbind(estimate.tidy_2015_year, estimate.sd.tidy_2015_year[,-2]))
glvvm.coef.plot_2015_year$nativity <- str_extract(glvvm.coef.plot_2015_year$Species, "^[A-Za-z]+")
glvvm.coef.plot_2015_year$nativity <- ifelse(is.na(glvvm.coef.plot_2015_year$nativity), 
                                        "Native", 
                                        glvvm.coef.plot_2015_year$nativity)


glvvm.coef.plot.burnmech <- glvvm.coef.plot_2015_year %>%
  filter(Covariate %in% c("Year2009.TreatmentBurn", 
                          "Year2011.TreatmentBurn",
                          "Year2012.TreatmentBurn",
                          "Year2013.TreatmentBurn",
                          "Year2015.TreatmentBurn",
                          "Year2009.TreatmentMechanical", 
                          "Year2011.TreatmentMechanical",
                          "Year2012.TreatmentMechanical",
                          "Year2013.TreatmentMechanical",
                          "Year2015.TreatmentMechanical"))

gllvm.burnmech <- glvvm.coef.plot.burnmech %>%
  separate(Covariate,
           into = c("Year_raw", "Treatment_raw"),
           sep = "\\.") %>% 
  mutate(Year = str_remove(Year_raw,"^Year"),
         Treatment = str_remove(Treatment_raw, "^Treatment")) %>% 
  select(-Year_raw, -Treatment_raw) %>%
  mutate(nativity = ifelse(Species.sd == "Lupine", "Native", nativity))


gllvm.rain <- glvvm.coef.plot_2015_year %>%
  filter(Covariate %in% "yearly_rain") %>%
  mutate(nativity = ifelse(Species.sd == "Lupine", "Native", nativity))


view(glvvm.coef.plot.burnmech)


# FOR STATUS

#neg binomial with covariates
fit_env_2015.nb <- gllvm(wide_data_groupings_2015.nms,  # [,c(23, 32, 11, 18, 28, 14)]
                    data_plot_groupings_2015, family = "negative.binomial", 
                    num.lv = 1,
                    formula = ~ Trt_Status * Treatment + yearly_rain,
                    seed = 1234)
summary(fit_env_2015.nb)
plot(fit_env_2015.nb, mfrow=c(3,2))
coefplot(fit_env_2015.nb, cex.ylab = 0.7, mar = c(4, 5, 2, 1), mfrow=c(3,3),
         order = FALSE )


#make nicer plots of  fit_env.nb

estimate_2015 <- data.frame(fit_env_2015.nb[["params"]][["Xcoef"]])
estimate_2015 <- tibble::rownames_to_column(estimate_2015, "Species")
estimate.sd_2015 <- data.frame(fit_env_2015.nb[["sd"]][["Xcoef"]])
estimate.sd_2015 <- tibble::rownames_to_column(estimate.sd_2015, "Species.sd")

#make df
estimate.tidy_2015 <- estimate_2015 %>% 
  pivot_longer(cols = c(-Species) , names_to = "Covariate", values_to = "Estimate")

estimate.sd.tidy_2015 <- estimate.sd_2015 %>% 
  pivot_longer(cols = c(-Species.sd) , names_to = "Covariate", values_to = "Estimate.sd")

glvvm.coef.plot_2015 <- tibble(cbind(estimate.tidy_2015, estimate.sd.tidy_2015[,-2]))
glvvm.coef.plot_2015$nativity <- str_extract(glvvm.coef.plot_2015$Species, "^[A-Za-z]+")
glvvm.coef.plot_2015$nativity <- ifelse(is.na(glvvm.coef.plot_2015$nativity), 
                                   "Native", 
                                   glvvm.coef.plot_2015$nativity)

# Create separate data frames based on covariates
glvvm.coef.plot.afterBurn_2015 <- glvvm.coef.plot_2015 %>% 
  filter(Covariate == "Trt_Statusafter.TreatmentBurn") %>%
  mutate(nativity = ifelse(Species.sd == "Lupine", "Native", nativity))

glvvm.coef.plot.aftermech_2015 <- glvvm.coef.plot_2015 %>% 
  filter(Covariate == "Trt_Statusafter.TreatmentMechanical") %>%
  mutate(nativity = ifelse(Species.sd == "Lupine", "Native", nativity))

glvvm.coef.plot.rain_2015 <- glvvm.coef.plot_2015 %>% 
  filter(Covariate == "yearly_rain") %>%
  mutate(nativity = ifelse(Species.sd == "Lupine", "Native", nativity))

