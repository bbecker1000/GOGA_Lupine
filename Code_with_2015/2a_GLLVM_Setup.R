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
plot(fit_ord_2015, var.colors = 1)

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


# Create column to help us code shape in the plot
glvvm.coef.plot_2015_yr <- glvvm.coef.plot_2015_year %>%                             
  mutate(lower = Estimate - 2 * Estimate.sd,
         upper = Estimate + 2 * Estimate.sd,
    code = if_else(
      Estimate >= -5 & Estimate <= 5 &        
        lower >= -5 & upper <= 5,         
      TRUE,                                 
      FALSE))


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #



# # FOR YEAR FACETED BY COVARIATE

# CREATE A DATAFRAME WITH THE BURN DATA

# Filter out the relevant data
glvvm.coef.plot.burn <- glvvm.coef.plot_2015_yr %>%
  filter(Covariate %in% c("Year2009.TreatmentMechanical", 
                          "Year2011.TreatmentMechanical",
                          "Year2012.TreatmentMechanical",
                          "Year2013.TreatmentMechanical",
                          "Year2015.TreatmentMechanical"))

# Break covariates into raw data for plotting
gllvm.burn <- glvvm.coef.plot.burn %>%
  separate(Covariate,
           into = c("Year_raw", "Treatment_raw"),
           sep = "\\.") %>% 
  mutate(Year = str_remove(Year_raw,"^Year"),
         Treatment = str_remove(Treatment_raw, "^Treatment")) %>% 
  select(-Year_raw, -Treatment_raw) %>%
  mutate(nativity = ifelse(Species.sd == "Lupine", "Native", nativity))


gllvm.burn$Year <- factor(gllvm.burn$Year, 
                          levels = c("2009", "2010", "2011", "2012", "2013", "2015"))




# CREATE A DATA FRAME WITH THE MECH DATA

# Filter out the relevant data
glvvm.coef.plot.mech <- glvvm.coef.plot_2015_yr %>%
  filter(Covariate %in% c("Year2009.TreatmentMechanical", 
                          "Year2011.TreatmentMechanical",
                          "Year2012.TreatmentMechanical",
                          "Year2013.TreatmentMechanical",
                          "Year2015.TreatmentMechanical"))

# Break covariates into raw data for plotting
gllvm.mech <- glvvm.coef.plot.mech %>%
  separate(Covariate,
           into = c("Year_raw", "Treatment_raw"),
           sep = "\\.") %>% 
  mutate(Year = str_remove(Year_raw,"^Year"),
         Treatment = str_remove(Treatment_raw, "^Treatment")) %>% 
  select(-Year_raw, -Treatment_raw) %>%
  mutate(nativity = ifelse(Species.sd == "Lupine", "Native", nativity))

gllvm.mech$Year <- factor(gllvm.mech$Year, 
                          levels = c("2009", "2010", "2011", "2012", "2013", "2015"))



# CREATE A DATA FRAME WITH THE RAIN DATA

gllvm.rain <- glvvm.coef.plot_2015_yr %>%
  filter(Covariate %in% "yearly_rain") %>%
  mutate(nativity = ifelse(Species.sd == "Lupine", "Native", nativity))






# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #



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

