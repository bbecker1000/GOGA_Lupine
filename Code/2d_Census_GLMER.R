source("Code/1d_Data_setup_Census.R")

# Run a GLMER on the Lupine census data with negative binomial
# BY YEAR
m_Lupin_census_nb <- glmer.nb(RowCount ~ Treatment * 
                          as.factor(Year) +
                          Status +
                          (1|Plot),
                        data = Lupin_Census)

summary(m_Lupin_census_nb)


# Run a GLMER on the Lupine census data with negative binomial
# BY PRE/POST TREATMENT
m_Lupin_census_nb_PrePost <- glmer.nb(RowCount ~ Treatment * 
                                Trt_Status + 
                                Status +
                                (1|Site/Plot),
                              data = Lupin_Census)

summary(m_Lupin_census_nb_PrePost)
