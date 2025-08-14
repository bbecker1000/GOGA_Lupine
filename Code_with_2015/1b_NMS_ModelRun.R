source("Code_with_2015/1a_NMS_Setup.R")

library(ggordiplots)
library(vegan)

# Run NMDS model
set.seed(10) 
nms_groupings_2015 <- metaMDS(wide_data_groupings_2015.nms, trymax = 25)

# Scale yearly rainfall
data_plot_groupings_2015$yearly_rain <- scale(data_plot_groupings_2015$yearly_rain)

# Run environmental fit
en_groupings_2015 = envfit(nms_groupings_2015, 
                           data_plot_groupings_2015, 
                           permutations = 999, na.rm = TRUE)

# 12 vectors (rows) with NMDS1, NMDS2
env_vec <- scores(en_groupings_2015, "vectors") %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.))

# Extract the two vectors
vec1 <- env_vec %>% filter(variable == "Time_Since_Trt") %>% select(NMDS1, NMDS2) %>% as.numeric()
vec2 <- env_vec %>% filter(variable == "yearly_rain") %>% select(NMDS1, NMDS2) %>% as.numeric()

# Calculate lengths (magnitudes)
length1 <- sqrt(sum(vec1^2))
length2 <- sqrt(sum(vec2^2))
length_ratio <- length1 / length2

# Compare
print(paste("Length of var1:", length1))
print(paste("Length of var2:", length2))
print(paste("Difference in length:", abs(length1 - length2)))
print(length_ratio)

en_groupings_2015$vectors$r
en_groupings_2015$vectors$r^2  # This is often reported



# all Year values that appear in the site scores
years <- sort(unique(data.scores.group_2015$Year))

# repeat the same arrows for every year facet
env_vec_faceted <- tidyr::crossing(Year = years, env_vec)


# Extract data from envfit and model
data.scores.group_2015 <- as.data.frame(scores(nms_groupings_2015)$site)
data.scores.group_2015$Trt_Status <- wide_data_groupings_2015$Trt_Status
data.scores.group_2015$Treatment <- wide_data_groupings_2015$Treatment
data.scores.group_2015$Year <- wide_data_groupings_2015$Year
group.scores_2015 <- as.data.frame(scores(nms_groupings_2015)$species)
species.scores <- as.data.frame(scores(nms_groupings_2015, "species"))
species.scores$species <- rownames(species.scores)
en_coord_cont_2015 = as.data.frame(scores(en_groupings_2015, "vectors")) 



# Tell adonis that the plots are being remeasured
h_groupings_2015 <- how(within = Within(type = "series"),
                        plots = Plots(strata = data_plot_groupings_2015$MacroPlot),
                        blocks =  data_plot_groupings_2015$Plot,
                        nperm = 499)



# Run adonis with interactions between treatment status and treatment
adonis2(wide_data_groupings_2015.nms ~ Trt_Status * Treatment + scale(yearly_rain),
        data = data_plot_groupings_2015,
        perm = h_groupings_2015,
        by = "terms",
        permutations = 1000)


# Run adonis with interactions between year and treatment
adonis_Year <- adonis2(wide_data_groupings_2015.nms ~ Year * Treatment + yearly_rain,
        data = data_plot_groupings_2015,
        perm = h_groupings_2015,
        by = "terms",
        permutations = 1000)


