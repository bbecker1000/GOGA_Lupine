source("Code/1d_Data_setup_Census.R")

# Run a GLMER on the Lupine census data with negative binomial
# BY YEAR
m_Lupin_census_nb <- glmer.nb(RowCount ~ Treatment * 
                          as.factor(Year) +
                          (1|Plot),
                        data = Lupin_Census)

summary(m_Lupin_census_nb)
# 
# model2 <- extend(m_Lupin_census_nb, along="Plot", n=20)
# summary(model2)
# 
# fixef(model2)["TreatmentB:as.factor(Year)2011"] <- 0.5
# 
# t <- powerSim(model2, nsim = Nsim, test=fixed("TreatmentB:as.factor(Year)2011","z"))
# 
# summary(t)
# 
# t2 <- powerCurve(model2, nsim = Nsim, test=fixed("TreatmentB:as.factor(Year)2011","z"))
# 
# Nsim <- 10

# Run a GLMER on the Lupine census data with negative binomial
# BY PRE/POST TREATMENT
m_Lupin_census_nb_PrePost <- glmer.nb(RowCount ~ Treatment * 
                                Trt_Status + 
                                Status +
                                (1|Site/Plot),
                              data = Lupin_Census)

summary(m_Lupin_census_nb_PrePost)
