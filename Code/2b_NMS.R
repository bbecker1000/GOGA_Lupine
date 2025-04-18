source("Code/1b_Data_setup_NMS.R")

library(ggordiplots)
library(vegan)

# # NMS that include species groups

set.seed(10) # for repeatability
nms_groupings <- metaMDS(wide_data_groupings.nms, trymax = 25)

data_plot_groupings$yearly_rain <- scale(data_plot_groupings$yearly_rain)

en_groupings = envfit(nms_groupings, 
                      data_plot_groupings, 
                      permutations = 999, na.rm = TRUE)

data.scores.group = as.data.frame(scores(nms_groupings)$site)
data.scores.group$Trt_Status = wide_data_groupings$Trt_Status
data.scores.group$Treatment = wide_data_groupings$Treatment

group.scores = as.data.frame(scores(nms_groupings)$species)

en_coord_cont_g = as.data.frame(scores(en_groupings, "vectors")) 

# Tell adonis that the plots are being remeasured
h_groupings <- how(within = Within(type = "series"),
                plots = Plots(strata = data_plot_groupings$MacroPlot),
                blocks =  data_plot_groupings$Plot,
                nperm = 499)

# Run adonis (without interactions between year and treatment)
adonis2(wide_data_groupings.nms ~ Trt_Status + Treatment + yearly_rain,
        data = data_plot_groupings,
        perm = h_groupings,
        by = "terms",
        permutations = 1000)


