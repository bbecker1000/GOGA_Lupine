source("Code/2b_NMS.R")
library(ggrepel)

# Plot NMS that includes all species with base R
plot(nms_allspp)
plot(en_allspp)

# Plot NMS for all species with all co-variates mapped
gg_all <- ggplot(data = data.scores.all, 
                 aes(x = NMDS1, y = NMDS2)) + 
  # plot the points
  geom_point(data = data.scores.all, 
             aes(colour = Treatment, shape = Trt_Status), 
             size = 3, alpha = 0.9) + 
  # edit the style of the points
  scale_shape_manual(values = c("before" = 1, "after" = 16)) + 
  # 
  labs(shape = "Treatment Status") +
  # add the ellipses
  stat_ellipse(data = data.scores.all, 
               aes(x = NMDS1, y = NMDS2, colour  = Treatment, linetype = Trt_Status), 
               level = 0.8, 
               alpha = 0.9, 
               size = 1) +
  # add the arrows for the effects of yearly rainfall
  geom_segment(data = en_coord_cont_all,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               linewidth = 0.75, # Vary arrow thickness
              # linetype = c("solid", "dashed"),
               alpha = 1,
               color = c("black", "gray60")) +
  # 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"),
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_line(), 
        panel.grid.major = element_line(color = "gray", size = 0.25), 
        panel.grid.minor = element_line(color = "gray", size = 0.25))

# View the plot
gg_all



# Plot NMS that include species groups with base R
plot(nms_groupings)
plot(en_groupings)

# Plot a simpler version of the envfit
gg_groups = gg_all <- ggplot(data = data.scores.group, 
                             aes(x = NMDS1, y = NMDS2)) + 
  # plot the points
  geom_point(data = data.scores.group, 
             aes(colour = Treatment, shape = Trt_Status), 
             size = 3, alpha = 0.9) + 
  # edit the style of the points
  scale_shape_manual(values = c("before" = 16, "after" = 1)) + 
  # edit the legend
  labs(shape = "Treatment Status") +
  # make the treatment labels lower case
  scale_fill_manual(labels = c("before", "after",
                               "before", "after",
                               "control", "burn", "mechanical")) +
  # add the ellipses
  stat_ellipse(data = data.scores.group, 
               aes(x = NMDS1, y = NMDS2, colour  = Treatment, linetype = Trt_Status), 
               level = 0.8, 
               alpha = 0.9, 
               size = 1) +
  # add the arrows for the effects of yearly rainfall
  geom_segment(data = en_coord_cont_g,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               linewidth = 0.75, # Vary arrow thickness
               # linetype = c("solid", "dashed"),
               alpha = 1,
               color = c("black", "gray50")) +
  # geom_text(data = en_coord_cont_g, 
  #           aes(x = NMDS1, y = NMDS2, label = rownames(en_coord_cont_g)), 
  #           vjust = -1, hjust = 1, size = 5, colour = "black") +
  # add species groups to the graph
  geom_text_repel(data = as_tibble(nms_groupings$species), 
            aes(x = MDS1, y = MDS2), 
            colour = "blue",
            label = row.names(nms_groupings$species)) + 
  # edit the look of the figure
  theme(axis.title = element_text(size = 14, face = "bold", colour = "black"),
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "black", size = 1), 
        axis.ticks = element_line(colour = "black"), 
        axis.text = element_text(colour = "black", size = 13),
        panel.grid.major = element_line(color = "grey", size = 0.2), 
        panel.grid.minor = element_line(color = "grey", size = 0.2),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14)) +
  coord_cartesian(xlim = c(-0.7, 0.7), ylim = c(-0.75, 0.7))

# View the plot
gg_groups 

file_path <- file.path(Sys.getenv("HOME"), "Downloads", "nmds_plot.png")
# 
# # Save the plot using ggsave
ggsave(file_path, plot = gg_groups,
       width = 8, height = 6,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")

