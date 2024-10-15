source("Code/2b_NMS.R")

# Plot NMS that includes all species with base R
plot(nms_allspp)
plot(en_allspp)

gg_all = ggplot(data = data.scores.all, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores.all, aes(colour = Treatment), size = 3, alpha = 0.5) + 
  #stat_ellipse() +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont_all, size =1, alpha = 0.5, colour = "grey30") +
  geom_point(data = en_coord_cat_all, aes(x = NMDS1, y = NMDS2), 
             shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
  geom_text(data = en_coord_cont_all, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont_all)) +
  geom_text(data = en_coord_cat_all, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cat_all)) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30"))

gg_all


# Plot NMS that include species groups with base R
plot(nms_groupings)
plot(en_groupings)

gg_groups = ggplot(data = data.scores.group, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores.group, aes(colour = Treatment), size = 3, alpha = 0.5) + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = en_coord_cont_g, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont_g)) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30"))

gg_groups


# Plots made with ggord don't seem to be working on the newer version of R, 
# ggord now ggordiplots package?

# Plot by Year and Treatment
# p.nmds_allspp <- ggord(nms_allspp, 
#                 grp_in = data_plot_allspp$yr_trt, #<-- BB edited to new variable
#                 pbslab = FALSE,
#                 arrow = NULL, 
#                 #size = 3,
#                 alpha_el = 0.2, 
#                 ptslab = TRUE,           # darkness of polygon
#                 poly = TRUE,
#                 labcol = "black",
#                 ellipse = T,
#                 ellipse_pro = 0.80, # confidence intervals for ellipse
#                 grp_title = "Treatment and Year",  
#                 repel = TRUE,            # make text not overlap
#                 txt = NULL,                 # size of text
#                 #cols = c('purple', 'orange', 'blue', 'green', 'red', 'yellow'), # you want to select colors
#                 facet = F, #make each grp_in on its own panel
#                 nfac = 0) +   # number of facet columns
#   theme_gray(base_size = 18) # can add ggplot commands !
# 
# p.nmds_allspp

# Plot by Year and Treatment
# p.nmds_groupings <- gg_ordicluster(nms_groupings, 
#                        grp_in = data_plot_groupings$yr_trt, #<-- BB edited to new variable
#                        pbslab = FALSE,
#                        arrow = NULL, 
#                        #size = 3,
#                        alpha_el = 0.2, 
#                        ptslab = TRUE,           # darkness of polygon
#                        poly = TRUE,
#                        labcol = "black",
#                        ellipse = T,
#                        ellipse_pro = 0.80, # confidence intervals for ellipse
#                        grp_title = "Treatment and Year",  
#                        repel = TRUE,            # make text not overlap
#                        txt = NULL,                 # size of text
#                        #cols = c('purple', 'orange', 'blue', 'green', 'red', 'yellow'), # you want to select colors
#                        facet = F, #make each grp_in on its own panel
#                        nfac = 0) +   # number of facet columns
#   theme_gray(base_size = 18) # can add ggplot commands !
# 
# p.nmds_groupings