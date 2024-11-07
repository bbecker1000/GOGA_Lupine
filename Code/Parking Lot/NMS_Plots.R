
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

