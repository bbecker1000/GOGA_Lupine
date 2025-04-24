source("Code_with_2015/1b_NMS_ModelRun.R")
library(ggrepel)


# Plot NMS that include species groups with base R
plot(nms_groupings_2015)
plot(en_groupings_2015)

# Plot a simpler version of the envfit
gg_groups_2015 <- ggplot(data = data.scores.group_2015, 
                             aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores.group_2015, 
             aes(colour = Treatment, shape = Trt_Status), 
             size = 3, alpha = 0.9) + 
  scale_shape_manual(values = c("before" = 16, "after" = 1)) + 
  labs(shape = "Treatment Status") +
  scale_fill_manual(labels = c("before", "after",
                               "before", "after",
                               "Control", "Burn", "Mechanical")) +
  stat_ellipse(data = data.scores.group_2015, 
               aes(x = NMDS1, y = NMDS2, colour  = Treatment, linetype = Trt_Status), 
               level = 0.8, 
               alpha = 0.9, 
               size = 1) +
  geom_segment(data = en_coord_cont_g_2015,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               linewidth = 0.75,
               alpha = 1,
               color = "black") +
  geom_text(data = en_coord_cont_g_2015, 
            aes(x = NMDS1, y = NMDS2, label = c("Annual Rainfall", "Time Since Treatment")), 
            vjust = -1, hjust = 1, size = 5, colour = "black", fontface = "bold") +
  geom_text_repel(data = as_tibble(nms_groupings_2015$species), 
                  aes(x = MDS1, y = MDS2), 
                  colour = "blue",
                  label = row.names(nms_groupings_2015$species)) + 
  theme(axis.title = element_text(size = 14, face = "bold", colour = "black"),
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1), 
        axis.ticks = element_line(colour = "black"), 
        axis.text = element_text(colour = "black", size = 13),
        panel.grid.major = element_line(color = "grey", linewidth = 0.2), 
        panel.grid.minor = element_line(color = "grey", linewidth  = 0.2),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14)) +
  #coord_cartesian(xlim = c(-0.7, 0.7), ylim = c(-0.75, 0.7)) +
  guides(
    shape = guide_legend(title = "Treatment Status"),
    linetype = guide_legend(title = "Treatment Status")
  )

# View the plot
gg_groups_2015



gg_year_2015 <- ggplot(data.scores.group_2015,
  aes(NMDS1, NMDS2)) +
  facet_wrap(~ Year) +
  geom_point(aes(colour = Treatment), size = 2, alpha = 0.9) +
  stat_ellipse(aes(colour = Treatment), level = .80) +
  geom_text_repel(data = species.scores,
            aes(NMDS1, NMDS2, label = species),
            colour = "black",
            size = 3,
            max.overlaps = Inf) +
  coord_fixed(xlim = c(-1, 1),
              ylim = c(-1, 1)) + 
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 20, colour = "black"),
    axis.title.x = element_text(face = "bold", size = 20,
                                    margin = margin(t = 15, b = 15),
                                    colour = "black"),
    axis.title.y = element_text(face = "bold", size = 20,
                                    margin = margin(l = 15, r = 15),
                                    colour = "black"),
    axis.text = element_text(size = 17, colour = "black"),
    legend.title = element_text(face = "bold", size = 19, colour = "black"),
    legend.text = element_text(size = 17,  colour = "black"),
    strip.text = element_text(face = "bold", size = 19, colour = "black"),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = .7),
    panel.grid.minor = element_line(colour = "grey80", linewidth = 0.07)) +
  labs(colour = "Treatment") 
 



gg_year_2015 





file_path <- file.path(Sys.getenv("HOME"), "Downloads", "nmds_plot_status.png")
#
# # Save the plot using ggsave
ggsave(file_path, plot = gg_groups_2015,
       width = 10, height = 7,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")



