source("Code/6a_UpdatedCensusData_Setup.R")

library(lme4)

# Run a GLMER on the Lupine census data with negative binomial
# BY YEAR
m_Lupin_census_allyears_nb <- glmer.nb(Count ~ Treatment * 
                                as.factor(Year) +
                                (1|Plot),
                              data = Lupine_Density_2009_2015_grouped_live)

summary(m_Lupin_census_allyears_nb)


# Run a GLMER on the Lupine census data with negative binomial
# BY PRE/POST TREATMENT
m_Lupin_census_allyears_nb_PrePost <- glmer.nb(Count ~ Treatment * 
                                        Trt_Status +
                                        (1|Site/Plot),
                                      data = Lupine_Density_2009_2015_grouped_live)

summary(m_Lupin_census_allyears_nb_PrePost)


# Run GLMERs on UV1 (life stage) 

# looking at the change in the ratio of immature individuals
m_lupin_allyears_immature <- glmer(cbind(Count_I, Total_Count) ~ Treatment * 
                            Year +
                            (1|Plot),
                          family = binomial,
                          data = Lupin_Ratio_2009_2015)

summary(m_lupin_allyears_immature)


