# Most commonly used Library offering the following functionalities in an intuitive & consistent way
# Data Import (readr)
# Data cleaning (tidyr, stringr, forcats, purrr)
# Data Wrangling (dplyr)
# Data Visualization (ggplot2)
# Reporting (rmarkdonw)
# shiny (interactive dashboards)

library(tidyverse)

view(diamonds)

# top 6 rows
head(diamonds)

# Box plot
# Most tidyverse functions take a dataset as their first argument
diamonds %>%  
  ggplot(aes(cut, price, color = cut))+                                  # Aesthetics mapping 
  geom_boxplot()+                                                        # plotting layer
  labs(title =  "Distribution of Price by Cut in the Diamonds Dataset")+ # adding plot title
  theme_minimal()                                                        # Adding a custom theme 

# Histogram
diamonds %>%  
  ggplot(aes(price))+                                 
  geom_histogram(fill = "steelblue")+                                                       
  labs(title =  "Distribution of Price in the Diamonds Dataset")+ 
  theme_minimal()
