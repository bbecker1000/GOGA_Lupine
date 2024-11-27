source("Code/1b_Data_setup_NMS.R")

library(ggordiplots)
library(vegan)

# # NMS that includes all species

set.seed(10) # for repeatability
nms_allspp <- metaMDS(wide_data_allspp.nms, trymax = 25)

en_allspp = envfit(nms_allspp, 
                   data_plot_allspp, 
                permutations = 999, na.rm = TRUE)

data.scores.all = as.data.frame(scores(nms_allspp)$site)
data.scores.all$Trt_Status = wide_data_allspp$Trt_Status
data.scores.all$Treatment = wide_data_allspp$Treatment

en_coord_cont_all = as.data.frame(scores(en_allspp, "vectors")) 
en_coord_cont_all <- en_coord_cont_all %>% rownames_to_column(var = "label")

# Tell adonis that the plots are being remeasured
h_allspp <- how(within = Within(type = "series"),
          plots = Plots(strata = wide_data_allspp$MacroPlot),
          blocks =  wide_data_allspp$Plot,
          nperm = 1000)

# Run adonis (without interactions between year and treatment)
adonis2(wide_data_allspp.nms ~ Trt_Status * Treatment + yearly_rain,
        data = wide_data_allspp,
        perm = h_allspp, # new 2024-11-18 
        by = "terms",    # new 2024-11-18 
        permutations = 1000)

# visual break ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# # NMS that include species groups

set.seed(10) # for repeatability
nms_groupings <- metaMDS(wide_data_groupings.nms, trymax = 25)

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


