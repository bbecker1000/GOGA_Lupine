source("Code/1b_Data_setup_NMS.R")

# # NMS that includes all species

set.seed(10) # for repeatability
nms_allspp <- metaMDS(wide_data_allspp.nms, trymax = 25)
plot(nms_allspp)

# Plot by Year and Treatment
p.nmds_allspp <- ggord(nms_allspp, 
                grp_in = data_plot_allspp$yr_trt, #<-- BB edited to new variable
                pbslab = FALSE,
                arrow = NULL, 
                #size = 3,
                alpha_el = 0.2, 
                ptslab = TRUE,           # darkness of polygon
                poly = TRUE,
                labcol = "black",
                ellipse = T,
                ellipse_pro = 0.80, # confidence intervals for ellipse
                grp_title = "Treatment and Year",  
                repel = TRUE,            # make text not overlap
                txt = NULL,                 # size of text
                #cols = c('purple', 'orange', 'blue', 'green', 'red', 'yellow'), # you want to select colors
                facet = F, #make each grp_in on its own panel
                nfac = 0) +   # number of facet columns
  theme_gray(base_size = 18) # can add ggplot commands !

p.nmds_allspp

h_allspp <- how(within = Within(type = "series"),
          plots = Plots(strata = data_plot_allspp$MacroPlot),
          blocks =  data_plot_allspp$Plot,
          nperm = 499)

# Run adonis
adonis2(wide_data_allspp.nms ~ Year + Treatment,
        data = data_plot_allspp,
        permutations = 1000)

adonis2(wide_data_allspp.nms ~ Year * Treatment,
        data = data_plot_allspp,
        permutations = 1000)


# # NMS that include species groups

set.seed(10) # for repeatability
nms_groupings <- metaMDS(wide_data_groupings.nms, trymax = 25)
plot(nms_groupings)

# Plot by Year and Treatment
p.nmds_groupings <- ggord(nms_groupings, 
                       grp_in = data_plot_groupings$yr_trt, #<-- BB edited to new variable
                       pbslab = FALSE,
                       arrow = NULL, 
                       #size = 3,
                       alpha_el = 0.2, 
                       ptslab = TRUE,           # darkness of polygon
                       poly = TRUE,
                       labcol = "black",
                       ellipse = T,
                       ellipse_pro = 0.80, # confidence intervals for ellipse
                       grp_title = "Treatment and Year",  
                       repel = TRUE,            # make text not overlap
                       txt = NULL,                 # size of text
                       #cols = c('purple', 'orange', 'blue', 'green', 'red', 'yellow'), # you want to select colors
                       facet = F, #make each grp_in on its own panel
                       nfac = 0) +   # number of facet columns
  theme_gray(base_size = 18) # can add ggplot commands !

p.nmds_groupings

h_groupings <- how(within = Within(type = "series"),
                plots = Plots(strata = data_plot_groupings$MacroPlot),
                blocks =  data_plot_groupings$Plot,
                nperm = 499)

# Run adonis
adonis2(wide_data_groupings.nms ~ Year + Treatment,
        data = data_plot_groupings,
        permutations = 1000)

adonis2(wide_data_groupings.nms ~ Year * Treatment,
        data = data_plot_groupings,
        permutations = 1000)
