source("Code_with_2015/1a_NMS_Setup.R")

library(ggordiplots)
library(vegan)

# # NMS that include species groups

set.seed(10) # for repeatability
nms_groupings_2015 <- metaMDS(wide_data_groupings_2015.nms, trymax = 25)

data_plot_groupings_2015$yearly_rain <- scale(data_plot_groupings_2015$yearly_rain)

en_groupings_2015 = envfit(nms_groupings_2015, 
                           data_plot_groupings_2015, 
                           permutations = 999, na.rm = TRUE)

data.scores.group_2015 = as.data.frame(scores(nms_groupings_2015)$site)
data.scores.group_2015$Trt_Status = wide_data_groupings_2015$Trt_Status
data.scores.group_2015$Treatment = wide_data_groupings_2015$Treatment

group.scores_2015 = as.data.frame(scores(nms_groupings_2015)$species)

en_coord_cont_g_2015 = as.data.frame(scores(en_groupings_2015, "vectors")) 

# Tell adonis that the plots are being remeasured
h_groupings_2015 <- how(within = Within(type = "series"),
                        plots = Plots(strata = data_plot_groupings_2015$MacroPlot),
                        blocks =  data_plot_groupings_2015$Plot,
                        nperm = 499)

# Run adonis (without interactions between year and treatment)
adonis2(wide_data_groupings_2015.nms ~ Trt_Status + Treatment + scale(yearly_rain),
        data = data_plot_groupings_2015,
        perm = h_groupings_2015,
        by = "terms",
        permutations = 1000)


