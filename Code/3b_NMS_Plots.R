source("Code/2b_NMS.R")

# Plot NMS that includes all species with base R
plot(nms_allspp)
plot(en_allspp)

# Plot NMS for all species with all co-variates mapped
gg_all = ggplot(data = data.scores.all, 
                aes(x = NMDS1, y = NMDS2)
                ) + 
  geom_point(data = data.scores.all, 
             aes(colour = Treatment, shape = Trt_Status), 
             size = 3, alpha = 0.9
             ) + 
  stat_ellipse(data = data.scores.all, 
               aes(x = NMDS1, y = NMDS2, colour  = Treatment, linetype = Trt_Status), 
               level = 0.8, 
               alpha = 0.9, 
               size = 1
               ) +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               data = en_coord_cont_all, 
               arrow = arrow(length = unit(0.25, "cm")),
               linewidth = 1, 
               alpha = 1, 
               colour = "grey30"
               ) +
  geom_text(data = en_coord_cont_all, 
            aes(x = NMDS1, y = NMDS2), 
            colour = "grey30",
            fontface = "bold", 
            label = c("Yearly Rainfall", "Time Since Treatment")
            ) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_line(), 
        #axis.text = element_line(), 
        #legend.key = element_blank(), 
        #legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        #legend.text = element_text(size = 9, colour = "grey30"),
        panel.grid.major = element_line(color = "gray", size = 0.25), 
        panel.grid.minor = element_line(color = "gray", size = 0.25)
        ) + 
  labs(shape = "Treatment Status"
       ) 

# View the plot
gg_all + labs(shape = "Treatment Status")
  

# Plot NMS that include species groups with base R
plot(nms_groupings)
plot(en_groupings)

# Plot a simpler version of the envfit
gg_groups2 = ggplot(data = data.scores.group, 
                    aes(x = NMDS1, y = NMDS2)
                    ) + 
  geom_point(data = data.scores.group, 
             aes(colour = Treatment, shape = Trt_Status), 
             size = 3, 
             alpha = 1
             ) + 
  stat_ellipse(data = data.scores.group, 
               aes(x = NMDS1, y = NMDS2, colour  = Treatment, linetype = Trt_Status), 
               level = 0.8, 
               alpha = 1, 
               size = 1
               ) +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont_g, 
               size = 1, 
               alpha = 1,
               colour = "grey30"
               ) +
  geom_text(data = en_coord_cont_g, 
            aes(x = NMDS1, y = NMDS2), 
            colour = "grey30", 
            fontface = "bold", 
            label = row.names(en_coord_cont_all)
            ) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA), 
        axis.ticks = element_blank(), 
        #axis.text = element_blank(), 
        legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30"),
        panel.grid.major = element_line(color = "gray", size = 0.25), 
        panel.grid.minor = element_line(color = "gray", size = 0.25)
        ) +
  geom_text(data = as_tibble(nms_groupings$species), 
            aes(x = MDS1, y = MDS2), 
            colour = "blue",
            label = row.names(nms_groupings$species)) + 
  labs(shape = "Treatment Status"
  )

# View the plot
gg_groups2

