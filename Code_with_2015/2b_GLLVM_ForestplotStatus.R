source("Code_with_2015/2a_GLLVM_Setup.R")

# Forest Plot for Post-treatment burn

afterburn_2015 <- ggplot(glvvm.coef.plot.afterBurn_2015, aes(Species, Estimate, color = nativity, shape = Species)) + 
  geom_pointrange(aes(ymin = Estimate - 2 * Estimate.sd, ymax = Estimate + 2 * Estimate.sd)) + 
  coord_flip() + 
  scale_shape_manual(values = c(
    "Substrate" = 16,
    "Native UG" = 16,
    "Native PU" = 16, 
    "Native PS" = 16, 
    "Native PL" = 1,
    "Native PG" = 16, 
    "Native PF" = 16, 
    "Native NG" = 16, 
    "Native NF" = 16, 
    "Native BF" = 1, 
    "Native AG" = 16, 
    "Native AF" = 16, 
    "Lupine" = 16, 
    "Invasive PS" = 16, 
    "Invasive PG" = 16, 
    "Invasive PF" = 16, 
    "Invasive AG" = 16, 
    "Invasive AF" = 16, 
    "Introduced PS" = 1, 
    "Introduced AG" = 1, 
    "Introduced AF" = 16)
    ) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(title = "Burn Treatment", color = "Nativity Status") + 
  ylim(-5, 5) + 
  scale_color_manual(
    values = c(
      "Invasive" = "#F8766D",
      "Introduced" = "#619CFF",
      "Native" = "#00BA38"
    )
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

afterburn_2015



# Forest Plot for Post-treatment mechanical
aftermech_2015 <- ggplot(glvvm.coef.plot.aftermech_2015, aes(Species, Estimate, color = nativity, shape = Species)) + 
  geom_pointrange(aes(ymin = Estimate - 2 * Estimate.sd, ymax = Estimate + 2 * Estimate.sd)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(title = "Mechanical Treatment", color = "Nativity Status") + 
  ylim(-5, 5) + 
  scale_color_manual(
    values = c(
      "Invasive" = "#F8766D",
      "Introduced" = "#619CFF",
      "Native" = "#00BA38"
    )
  ) + 
  scale_shape_manual(values = c(
    "Substrate" = 16,
    "Native UG" = 16,
    "Native PU" = 16, 
    "Native PS" = 16, 
    "Native PL" = 1,
    "Native PG" = 16, 
    "Native PF" = 16, 
    "Native NG" = 16, 
    "Native NF" = 16, 
    "Native BF" = 1, 
    "Native AG" = 16, 
    "Native AF" = 16, 
    "Lupine" = 16, 
    "Invasive PS" = 16, 
    "Invasive PG" = 16, 
    "Invasive PF" = 16, 
    "Invasive AG" = 16, 
    "Invasive AF" = 16, 
    "Introduced PS" = 1, 
    "Introduced AG" = 1, 
    "Introduced AF" = 16
  )) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(color = "black", size = 15),
    axis.text.y = element_blank(),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = .7)
  )

aftermech_2015


# Forest Plot for rainfall

rain.forestplot_2015 <- ggplot(glvvm.coef.plot.rain_2015, aes(Species, Estimate, color = nativity, shape = Species)) + 
  geom_pointrange(aes(ymin = Estimate - 2 * Estimate.sd, ymax = Estimate + 2 * Estimate.sd)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(title = "Rainfall", color = "Nativity Status") + 
  scale_color_manual(
    values = c(
      "Invasive" = "#F8766D",
      "Introduced" = "#619CFF",
      "Native" = "#00BA38"
    )
  ) + 
  scale_shape_manual(values = c(
    "Substrate" = 16,
    "Native UG" = 16,
    "Native PU" = 16, 
    "Native PS" = 16, 
    "Native PL" = 16,
    "Native PG" = 16, 
    "Native PF" = 16, 
    "Native NG" = 16, 
    "Native NF" = 16, 
    "Native BF" = 16, 
    "Native AG" = 16, 
    "Native AF" = 16, 
    "Lupine" = 16, 
    "Invasive PS" = 16, 
    "Invasive PG" = 16, 
    "Invasive PF" = 16, 
    "Invasive AG" = 16, 
    "Invasive AF" = 16, 
    "Introduced PS" = 1, 
    "Introduced AG" = 16, 
    "Introduced AF" = 16
  )) + 
  ylim(-5, 5) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(color = "black", size = 15),
    axis.text.y = element_blank(),
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 15),
    panel.border = element_rect(color = "black", fill = NA, size = .7)
  ) +
  guides(shape = "none") 

rain.forestplot_2015


gllvm_plots_2015 <- afterburn_2015 + 
                    aftermech_2015 + 
                    rain.forestplot_2015 + 
                    plot_annotation(tag_levels = "a")

gllvm_plots_2015


file_path <- file.path(Sys.getenv("HOME"), "Downloads", "gllvm_plots_2015.png")
#
# # Save the plot using ggsave
ggsave(file_path, plot = gllvm_plots_2015,
       width = 12, height = 6,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")

