# Run power simulations and store results
power_result <- c(
  P_Lupine_PrePost_MechAfter = powerSim(m2_Lupin_PrePost, nsim = Nsim, test=fixed("TreatmentMECHANICAL:Trt_Statusafter", "z")),
  P_Lupine_PrePost_BurnAfter = powerSim(m2_Lupin_PrePost, nsim = Nsim, test=fixed("TreatmentBURN:Trt_Statusafter", "z")),
  P_Lupine_PrePost_Rain = powerSim(m2_Lupin_PrePost, nsim = Nsim, test=fixed("scale(yearly_rain)", "z"))
)

# Extract key values
sim_name <- names(power_result)
test <- sapply(power_result[grep("\\.text$", names(power_result))], function(x) x)
successes <- sapply(power_result[grep("\\.x$", names(power_result))], function(x) x)
trials <- sapply(power_result[grep("\\.n$", names(power_result))], function(x) x)
lower_CI <- attr(power_summary, "conf.int")[1]
upper_CI <- attr(power_summary, "conf.int")[2]

# Extract the second word from the simulation name
second_word <- str_split(sim_name, "_", simplify = TRUE)[, 2]

data.frame(
  Fixed_Effect = test,  # Stores the fixed effect being tested
  Successes = successes,
  Trials = trials,
  Lower_CI = lower_CI,
  Upper_CI = upper_CI,
  Simulation_Name = sim_name,
  Second_Word = second_word
)


power_result2 <- c(
  P_Lupine_PrePost_MechAfter = summary(powerSim(m2_Lupin_PrePost, nsim = Nsim, test=fixed("TreatmentMECHANICAL:Trt_Statusafter", "z"))),
  P_Lupine_PrePost_BurnAfter = summary(powerSim(m2_Lupin_PrePost, nsim = Nsim, test=fixed("TreatmentBURN:Trt_Statusafter", "z"))),
  P_Lupine_PrePost_Rain = summary(powerSim(m2_Lupin_PrePost, nsim = Nsim, test=fixed("scale(yearly_rain)", "z")))
)

view(power_result2)

sim_name2 <- names(power_result2)
mean <- sapply(power_result2[grep("\\.mean$", names(power_result2))], function(x) x)
lower_CI <- sapply(power_result2[grep("\\.lower$", names(power_result2))], function(x) x)
upper_CI <- sapply(power_result2[grep("\\.upper$", names(power_result2))], function(x) x)
Group <- str_split(sim_name2, "_", simplify = TRUE)[, 2]
ModelType <- str_split(sim_name2, "_", simplify = TRUE)[, 3]
Test1 <- str_split(sim_name2, "_", simplify = TRUE)[, 4]
Test2 <- gsub("\\..*", "", Test1)
  
w <- data.frame(
  Group = Group,
  Type = ModelType,
  Fixed_Effect = Test2, 
  Mean = mean, Lower_CI = lower_CI, Upper_CI = upper_CI)

w2 <- w[!duplicated(w), ]
