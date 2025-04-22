# Run GLMERs on the Lupine census data 

source("Code_with_2015/4a_UpdatedCensusData_Setup.R")
library(lme4)



# # LUPINE CENSUS NEGATIVE BINOMIAL BY YEAR # #

# Run negative binomial glmer
m_Lupin_census_allyears_nb <- glmer.nb(Count ~ Treatment * 
                                Year +
                                #scale(yearly_rain) +
                                (1|Plot),
                              data = Lupine_Density_2009_2015_grouped_live)

# View model output
summary(m_Lupin_census_allyears_nb)


# Extract the model data
predicted_Lupin_nb_yr <- Lupine_Density_2009_2015_grouped_live %>%
  mutate(predicted = predict(m_Lupin_census_allyears_nb, type = "response"))




# # PERCENT IMMATURE LUPINE BINOMIAL BY YEAR # #

# Run binomial glmer
m_lupin_allyears_immature <- glmer(cbind(Count_I, Total_Count) ~ 
                            Treatment * 
                            Year + 
                            #scale(yearly_rain) +
                            (1|Plot),
                          family = binomial,
                          data = Lupin_Ratio_2009_2015)


# View model output
summary(m_lupin_allyears_immature)

# Extract the model data
predicted_ImmatureLupin_b <- Lupin_Ratio_2009_2015 %>%
  mutate(predicted = predict(m_lupin_allyears_immature, type = "response"))






# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #






# # LUPINE CENSUS NEGATIVE BINOMIAL BY STATUS # #

# Run negative binomial glmer
m_Lupin_census_status_nb <- glmer.nb(Count ~ Treatment * 
                                         Trt_Status +
                                         #scale(yearly_rain) +
                                         (1|Plot),
                                       data = Lupine_Density_2009_2015_grouped_live)

# View model output
summary(m_Lupin_census_status_nb)


# Extract the model data
predicted_Lupin_nb_status <- Lupine_Density_2009_2015_grouped_live %>%
  mutate(predicted = predict(m_Lupin_census_status_nb, type = "response"))




# # PERCENT IMMATURE LUPINE BINOMIAL BY STATUS # #

# Run binomial glmer
m_lupin_status_immature <- glmer(cbind(Count_I, Total_Count) ~ Treatment * 
                                     Trt_Status + 
                                     #scale(yearly_rain) +
                                     (1|Plot),
                                   family = binomial,
                                   data = Lupin_Ratio_2009_2015)


# View model output
summary(m_lupin_status_immature)

# Extract the model data
predicted_ImmatureLupin_status <- Lupin_Ratio_2009_2015 %>%
  mutate(predicted = predict(m_lupin_status_immature, type = "response"))



