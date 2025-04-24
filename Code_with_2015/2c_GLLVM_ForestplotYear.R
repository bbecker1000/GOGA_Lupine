source("Code_with_2015/2a_GLLVM_Setup.R")

# Create a variable that codes in the stripes
stripe_df <- gllvm.burnmech|>
  mutate(Species = factor(Species, levels = unique(Species))) |>  # set order
  distinct(Species) |>
  mutate(
    x    = as.numeric(Species),     # now safe: 1, 2, 3 …
    xmin = x - 0.5,
    xmax = x + 0.5,
    fill = rep(c("grey95", "white"), length.out = n()))


# Forest Plot for Post-treatment burn
gllvm_burnmech_2015 <- ggplot(gllvm.burnmech, 
  aes(Species, Estimate, color = Year)) + 
  geom_rect(data = stripe_df,
            aes(xmin = xmin, xmax = xmax, fill = fill),
            ymin = -Inf, ymax =  Inf,
            colour = NA, inherit.aes = FALSE) +
  scale_fill_identity() +
  geom_pointrange(aes(ymin = Estimate - 2 * Estimate.sd, ymax = Estimate + 2 * Estimate.sd),
                  position = position_dodge(width = 1)) +
  facet_wrap(~Treatment) +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(color = "Year") + 
  ylim(-5, 5) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(color = "black", size = 15),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_text(face = "bold", size = 15,hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = .7)
  )

gllvm_burnmech_2015


# Forestplot for rainfall
gllvm_rain_2015 <- ggplot(gllvm.rain, aes(Species, Estimate)) + 
  geom_rect(data = stripe_df,
            aes(xmin = xmin, xmax = xmax, fill = fill),
            ymin = -Inf, ymax =  Inf,
            colour = NA, inherit.aes = FALSE) +
  scale_fill_identity() +
  geom_pointrange(aes(ymin = Estimate - 2 * Estimate.sd, ymax = Estimate + 2 * Estimate.sd)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(title = "Rainfall") + 
  ylim(-5, 5) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.y = element_blank(), #element_text(color = "black", size = 15),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA, size = .7)
  ) 


gllvm_rain_2015


gllvm_covariate <- gllvm_burnmech_2015 + gllvm_rain_2015 


