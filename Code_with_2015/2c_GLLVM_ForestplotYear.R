source("Code_with_2015/2a_GLLVM_Setup.R")

# Create a variable that codes in the stripes
stripe_df_1 <- gllvm.burn|>
  mutate(Species = factor(Species, levels = unique(Species))) |>  # set order
  distinct(Species) |>
  mutate(
    x    = as.numeric(Species),     # now safe: 1, 2, 3 …
    xmin = x - 0.5,
    xmax = x + 0.5,
    fill = rep(c("grey95", "white"), length.out = n()))



# Forest Plot for Post-treatment burn
gllvm_burn_2015 <- ggplot(gllvm.burn, 
  aes(Species, Estimate, color = Year, shape = code)) + 
  geom_rect(data = stripe_df_1,
            aes(xmin = xmin, xmax = xmax, fill = fill),
            ymin = -Inf, ymax =  Inf,
            colour = NA, inherit.aes = FALSE) +
  scale_fill_identity() +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) +
  geom_pointrange(aes(ymin = Estimate - 2 * Estimate.sd, ymax = Estimate + 2 * Estimate.sd),
                  position = position_dodge(width = 1)) +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(color = "Year",
       title = "Prescribed Burn") + 
  ylim(-5, 5) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(color = "black", size = 20, face = "bold"),
    axis.text.y = element_text(color = "black", size = 20, face = "bold"),
    legend.title = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 20),
    strip.text = element_text(face = "bold", size = 15,hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    plot.tag.position = c(0.3, 1)) +
    guides(shape  = "none")

# View the graph
gllvm_burn_2015


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #



# Create a variable that codes in the stripes
stripe_df_2 <- gllvm.mech|>
  mutate(Species = factor(Species, levels = unique(Species))) |>  # set order
  distinct(Species) |>
  mutate(
    x    = as.numeric(Species),     # now safe: 1, 2, 3 …
    xmin = x - 0.5,
    xmax = x + 0.5,
    fill = rep(c("grey95", "white"), length.out = n()))


# Forest Plot for Post-treatment burn
gllvm_mech_2015 <- ggplot(gllvm.mech, 
  aes(Species, Estimate, color = Year, shape = code)) + 
  geom_rect(data = stripe_df_2,
            aes(xmin = xmin, xmax = xmax, fill = fill),
            ymin = -Inf, ymax =  Inf,
            colour = NA, inherit.aes = FALSE) +
  scale_fill_identity() +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) +
  geom_pointrange(aes(ymin = Estimate - 2 * Estimate.sd, ymax = Estimate + 2 * Estimate.sd),
                  position = position_dodge(width = 1)) +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(color = "Year",
       title = "Mechanical Treatment") + 
  ylim(-5, 5) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 20, face = "bold"),
    legend.title = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 20),
    strip.text = element_text(face = "bold", size = 15,hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = .7)) +
    guides(shape  = "none")


# View the graph
gllvm_mech_2015


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #


# Create a variable that codes in the stripes
stripe_df_3 <- gllvm.rain|>
  mutate(Species = factor(Species, levels = unique(Species))) |>  # set order
  distinct(Species) |>
  mutate(
    x    = as.numeric(Species),     # now safe: 1, 2, 3 …
    xmin = x - 0.5,
    xmax = x + 0.5,
    fill = rep(c("grey95", "white"), length.out = n()))


# Forestplot for rainfall
gllvm_rain_2015 <- ggplot(gllvm.rain, aes(Species, Estimate, shape = code)) + 
  geom_rect(data = stripe_df_3,
            aes(xmin = xmin, xmax = xmax, fill = fill),
            ymin = -Inf, ymax =  Inf,
            colour = NA, inherit.aes = FALSE) +
  scale_fill_identity() +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) +
  geom_pointrange(aes(ymin = Estimate - 2 * Estimate.sd, ymax = Estimate + 2 * Estimate.sd)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(title = "Rainfall") + 
  ylim(-5, 5) + 
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.y = element_blank(), 
    axis.text.x = element_text(color = "black", size = 20, face = "bold"),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = .7)) +
    guides(shape  = "none")


# View the graph
gllvm_rain_2015


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #

# Create a panel with all three graphs together
gllvm_covariate <- gllvm_burn_2015 + 
                   gllvm_mech_2015 + 
                   gllvm_rain_2015 +
                   plot_annotation(tag_levels = "a") +
                   plot_layout(guides = "collect") +
                   theme(legend.position = "bottom")
                    


# View the graph
gllvm_covariate



# FOR SAVING GRAPHS

# Tell R where to save the graphs
file_path <- file.path(Sys.getenv("HOME"), "Downloads", "gllvm_allyears_Treatment.png")

# Save the plot using ggsave
ggsave(file_path, plot = gllvm_covariate,
       width = 20, height = 15, 
       dpi = 300,
       units = "in",          
       device = "png")


