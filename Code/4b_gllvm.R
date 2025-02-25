source("Code/2b_NMS.R")

library(gllvm)
library(patchwork)

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
glvvm.coef.plot$nativity <- str_extract(glvvm.coef.plot$Species, "^[A-Za-z]+")
glvvm.coef.plot$nativity <- ifelse(is.na(glvvm.coef.plot$nativity), 
                                   "Native", 
                                   glvvm.coef.plot$nativity)

# Order the Species
# glvvm.coef.plot$Species <- 
#   factor(glvvm.coef.plot$Species,
#          levels = c("Native PU",
#                     "Native PS",
#                     "Native PG",
#                     "Native PF",
#                     "Native NG",
#                     "Native NF",
#                     "Native BF",
#                     "Native AG",
#                     "Native AF",
#                     "Invasive PS",
#                     "Invasive PG",
#                     "Invasive PF",
#                     "Invasive AG",
#                     "Invasive AF",
#                     "Introduced PS",
#                     "Introduced AG",
#                     "Introduced AF",
#                     "Lupine"))
#   
# Create separate data frames based on covariates
glvvm.coef.plot.afterburn <- glvvm.coef.plot %>% 
  filter(Covariate == "Trt_Statusafter.TreatmentBURN") %>%
  mutate(nativity = ifelse(Species.sd == "Lupine", "Native", nativity))

glvvm.coef.plot.aftermech <- glvvm.coef.plot %>% 
  filter(Covariate == "Trt_Statusafter.TreatmentMECHANICAL") %>%
  mutate(nativity = ifelse(Species.sd == "Lupine", "Native", nativity))

glvvm.coef.plot.rain <- glvvm.coef.plot %>% 
  filter(Covariate == "yearly_rain") %>%
  mutate(nativity = ifelse(Species.sd == "Lupine", "Native", nativity))



# Forest Plot for Post-treatment burn
afterburn <- ggplot(glvvm.coef.plot.afterburn, aes(Species, Estimate,
                                      color = nativity)) + 
  geom_pointrange(aes(ymin = Estimate-2*Estimate.sd, ymax = Estimate+2*Estimate.sd)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "Burn Treatment",
       color = "Nativity Status") + 
  ylim(-5,5) +
  scale_color_manual(
    values = c(
      "Invasive" = "#F8766D",
      "Introduced" = "#619CFF",
      "Native" = "#00BA38")
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(color = "black", size = 15),
    # legend.title = element_text(face = "bold", size = 14),
    # legend.text = element_text(size = 14),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    plot.tag.position = c(0.34, 0.99)
        ) 

afterburn

# Forest Plot for Post-treatment mechanical
aftermech <- ggplot(glvvm.coef.plot.aftermech, aes(Species, Estimate, 
                                      color = nativity)) + 
  geom_pointrange(aes(ymin = Estimate-2*Estimate.sd, 
                      ymax = Estimate+2*Estimate.sd)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "Mechanical Treatment",
       color = "Nativity Status") + 
  ylim(-5,5) +
  scale_color_manual(
    values = c(
      "Invasive" = "#F8766D",
      "Introduced" = "#619CFF",
      "Native" = "#00BA38")
    ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(color = "black", size = 15),
    axis.text.y = element_blank(),
    legend.position = "none",
    # legend.title = element_text(face = "bold", size = 14),
    # legend.text = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA, size = .7)
  )

aftermech

# Forest Plot for rainfall
rain.forestplot <- ggplot(glvvm.coef.plot.rain, aes(Species, Estimate,
                                 color = nativity)) + 
  geom_pointrange(aes(ymin = Estimate-2*Estimate.sd, ymax = Estimate+2*Estimate.sd)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "Rainfall",
       color = "Nativity Status") + 
  scale_color_manual(
    values = c(
      "Invasive" = "#F8766D",
      "Introduced" = "#619CFF",
      "Native" = "#00BA38")
  ) +
  ylim(-5,5) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(color = "black", size = 15),
    axis.text.y = element_blank(),
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 15),
    panel.border = element_rect(color = "black", fill = NA, size = .7)
  )

rain.forestplot

gllvm_plots <- afterburn + aftermech + rain.forestplot + plot_annotation(tag_levels = "a")

gllvm_plots


file_path <- file.path(Sys.getenv("HOME"), "Downloads", "gllvm_plots.png")
#
# # Save the plot using ggsave
ggsave(file_path, plot = gllvm_plots,
       width = 12, height = 6,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")

