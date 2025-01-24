#Load Packages
library(tidyverse)
library(lme4)
library(sjPlot)

#Upload data
Lupin_Census <- read_csv("Data/Lupine_Combined_AllYears.csv")

# Separate our MacroPlot so you have columns with the Plot and Site
Lupin_Census$Plot <- str_extract(Lupin_Census$MacroPlot, "\\d+")
Lupin_Census$Site = str_extract(Lupin_Census$MacroPlot, "[^_]+$")

# Make year a factor
Lupin_Census$Year = as.factor(Lupin_Census$Year)

# Make a pre/post-treatment cateogry
Lupin_Census <- Lupin_Census %>%
  mutate(Trt_Status = case_when(
    Year %in% c(2009, 2010) ~ "before",
    Year %in% c(2011, 2012, 2013) ~ "after"))

# Set control as base level
Lupin_Census$Treatment <- factor(Lupin_Census$Treatment, 
                                levels = c("C", "B", "M"))

# set Pre-treatment as base level
Lupin_Census$Trt_Status <- factor(Lupin_Census$Trt_Status, 
                                 levels = c("before", "after"))


# Take a preliminary look at the data
ggplot(Lupin_Census, aes(x=as.factor(Year), y = RowCount, color = Treatment)) +
  geom_boxplot() +
  facet_wrap(~Status) +
  geom_point(aes(color = Treatment),
             alpha = 0.5,
             position = position_dodge(width = .75)) +
  labs(x = "Year",
       y = "Lupine Count",
       title = "Actual Lupine Census Data")

# Plot all data in a histogram to determine distribution
hist(Lupin_Census$RowCount)
# Based on output maybe Poisson or Negative binomial

# Run a GLMER on the Lupine census data with Poisson
m_Lupin_census_poisson <- glmer(RowCount ~ Treatment + 
                                    Year + 
                                    (1|Site/Plot),
                                    family = poisson, 
                                    data = Lupin_Census)

summary(m_Lupin_census_poisson)

# Plot residuals for Poisson
plot_model(m_Lupin_census_poisson,type = "diag")
plot(m_Lupin_census_poisson)


# Run a GLMER on the Lupine census data with negative binomial
m_Lupin_census_nb <- glmer.nb(RowCount ~ Treatment + 
                          Year + Status +
                          (1|Site/Plot),
                        data = Lupin_Census)

summary(m_Lupin_census_nb)

# Plot residuals with negative binomial
plot_model(m_Lupin_census_nb,type = "diag")
plot(m_Lupin_census_nb)

# Plot basic view of model
plot_model(m_Lupin_census_nb, type = "eff", 
           terms = c("Year", "Treatment", "Status")) +
  theme_classic() 

# Extract the model data
predicted_census <- Lupin_Census %>%
  mutate(predicted = predict(m_Lupin_census_nb, type = "response"))

# Use ggplot to create a boxplot of the model data
predicted_lupin_census_plot <- ggplot(predicted_census, 
  aes(x = Year, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Lupin_Census, 
             aes(Year, RowCount),
             position = position_dodge(width = .75),
             alpha = 0.5) +
  labs(
    title = "Predicted Lupin Count by Year",
    x = "Treatment Status", 
    y = "Predicted Percent Cover of Lupine",
    fill = "Treatment"
  ) +
  facet_wrap(~Status) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

# View graph
predicted_lupin_census_plot

# Run a GLMER on the Lupine census data with negative binomial
m_Lupin_census_nb_PrePost <- glmer.nb(RowCount ~ Treatment * 
                                Trt_Status + Status +
                                (1|Site/Plot),
                              data = Lupin_Census)

summary(m_Lupin_census_nb_PrePost)

# Plot residuals with negative binomial
plot_model(m_Lupin_census_nb_PrePost,type = "diag")
plot(m_Lupin_census_nb_PrePost)

# Plot basic view of model
plot_model(m_Lupin_census_nb_PrePost, type = "eff", 
           terms = c("Trt_Status", "Treatment", "Status")) +
  theme_classic() 

# Extract the model data
predicted_census_PrePost <- Lupin_Census %>%
  mutate(predicted = predict(m_Lupin_census_nb_PrePost, type = "response"))

# Use ggplot to create a boxplot of the model data
predicted_lupin_census_plot_PrePost <- ggplot(predicted_census_PrePost, 
                                      aes(x = Trt_Status, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Lupin_Census, 
             aes(Trt_Status, RowCount),
             position = position_dodge(width = .75),
             alpha = 0.5) +
  labs(
    title = "Predicted Lupin Count by Treatment Status",
    x = "Treatment Status", 
    y = "Predicted Percent Cover of Lupine",
    fill = "Treatment"
  ) +
  facet_wrap(~Status) +
  theme(
  plot.title = element_text(face = "bold", size = 20),
  axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
  axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
  axis.text = element_text(size = 17),
  legend.title = element_text(face = "bold", size = 19), # Bold legend title
  legend.text = element_text(face = "plain", size = 17) # Lowercase text
)

# View graph
predicted_lupin_census_plot_PrePost

# FOR SAVING GRAPHS

file_path <- file.path(Sys.getenv("HOME"), "Downloads", "predicted_lupin_census_plot.png")
# 
# # Save the plot using ggsave
ggsave(file_path, plot = predicted_lupin_census_plot,
       width = 15, height = 10,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")
