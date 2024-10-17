#Load Packages
library(tidyverse)

#Upload data
Lupin_Census <- read_csv("Data/Lupine_Combined_AllYears.csv")

ggplot(Lupin_Census, aes(x=Year, fill = Status)) +
  geom_bar() +
  facet_wrap(~Treatment)
