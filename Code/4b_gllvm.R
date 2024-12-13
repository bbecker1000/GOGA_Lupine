source("Code/2b_NMS.R")

library(gllvm)

#check data are ready
wide_data_groupings.nms
data_plot_groupings

data_plot_groupings$Plot <- as.factor(data_plot_groupings$Plot)

#all species ordination (~NMS)
fit_ord <- gllvm(wide_data_groupings.nms, family = "negative.binomial") #30 sec
fit_ord
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fit_ord, var.colors = 1)

ordiplot(fit_ord, biplot = TRUE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Biplot")
ordiplot(fit_ord, biplot = FALSE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Ordination plot", predict.region = TRUE)

#reset graphics
par(mfrow = c(1, 1))

#neg binomial with covariates
fit_env.nb <- gllvm(wide_data_groupings.nms,  # [,c(23, 32, 11, 18, 28, 14)]
                    data_plot_groupings, family = "negative.binomial", 
                    num.lv = 1,
                    formula = ~ Trt_Status * Treatment + yearly_rain,
                    seed = 1234)
summary(fit_env.nb)
plot(fit_env.nb, mfrow=c(3,2))
coefplot(fit_env.nb, cex.ylab = 0.7, mar = c(4, 5, 2, 1), mfrow=c(3,3),
         order = FALSE )


#make nicer plots of  fit_env.nb

estimate <- data.frame(fit_env.nb[["params"]][["Xcoef"]])
estimate <- tibble::rownames_to_column(estimate, "Species")
estimate.sd <- data.frame(fit_env.nb[["sd"]][["Xcoef"]])
estimate.sd <- tibble::rownames_to_column(estimate.sd, "Species.sd")

#make df
estimate.tidy <- estimate %>% 
  pivot_longer(cols = c(-Species) , names_to = "Covariate", values_to = "Estimate")

estimate.sd.tidy <- estimate.sd %>% 
  pivot_longer(cols = c(-Species.sd) , names_to = "Covariate", values_to = "Estimate.sd")

glvvm.coef.plot <- tibble(cbind(estimate.tidy, estimate.sd.tidy[,-2]))
glvvm.coef.plot$nativity <- str_extract(glvvm.coef.plot$Species, "[A-Z][a-z]+$")
glvvm.coef.plot$nativity <- ifelse(is.na(glvvm.coef.plot$nativity), 
                                   "Native", 
                                   glvvm.coef.plot$nativity)
  
# Create separate data frames based on covariates
glvvm.coef.plot.afterburn <- glvvm.coef.plot %>% 
  filter(Covariate == "Trt_Statusafter.TreatmentBURN")

glvvm.coef.plot.aftermech <- glvvm.coef.plot %>% 
  filter(Covariate == "Trt_Statusafter.TreatmentMECHANICAL")

glvvm.coef.plot.rain <- glvvm.coef.plot %>% 
  filter(Covariate == "yearly_rain")

# Forest Plot for Post-treatment burn
afterburn <- ggplot(glvvm.coef.plot.afterburn, aes(reorder(Species, -Estimate), Estimate,
                                      color = nativity)) + 
  geom_pointrange(aes(ymin = Estimate-2*Estimate.sd, ymax = Estimate+2*Estimate.sd)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "Effect of Prescribed Fire on Species Groups",
       color = "Nativity Status") + 
  ylim(-10,10) +
  scale_color_manual(
    values = c(
      "Invasive" = "#F8766D",
      "Introduced" = "#619CFF",
      "Native" = "#00BA38")
  ) +
  theme_gray(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 14)
        ) 

# Forest Plot for Post-treatment mechanical
aftermech <- ggplot(glvvm.coef.plot.aftermech, aes(reorder(Species, -Estimate), Estimate, 
                                      color = nativity)) + 
  geom_pointrange(aes(ymin = Estimate-2*Estimate.sd, 
                      ymax = Estimate+2*Estimate.sd)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "Effect of Mechanical Treatment on Species Groups",
       color = "Nativity Status") + 
  ylim(-6,6) +
  scale_color_manual(
    values = c(
      "Invasive" = "#F8766D",
      "Introduced" = "#619CFF",
      "Native" = "#00BA38")
    ) +
  theme_gray(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 14)
  )

# Forest Plot for rainfall
rain.forestplot <- ggplot(glvvm.coef.plot.rain, aes(reorder(Species, -Estimate), Estimate,
                                 color = nativity)) + 
  geom_pointrange(aes(ymin = Estimate-2*Estimate.sd, ymax = Estimate+2*Estimate.sd)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "Effect of Rainfall on Species Groups",
       color = "Nativity Status") + 
  scale_color_manual(
    values = c(
      "Invasive" = "#F8766D",
      "Introduced" = "#619CFF",
      "Native" = "#00BA38")
  ) +
  ylim(-2,2) +
  theme_gray(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 14)
  )


# 
# file_path <- file.path(Sys.getenv("HOME"), "Downloads", "forestplot_rain.png")
# # 
# # # Save the plot using ggsave
# ggsave(file_path, plot = rain.forestplot,
#        width = 8, height = 6,   # Set desired width and height in inches
#        dpi = 300,               # Set the resolution (300 DPI for high quality)
#        units = "in",            # Set units to inches
#        device = "png")
# 
