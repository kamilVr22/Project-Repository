#loading all necessary libraries
library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

#Importing data
gaza_cpi_df <- read_csv("gaza_cpi_updated.csv")
#making a copy just in case
gaza_cpi_df_copy <- gaza_cpi_df

#converting data from wide format to long
gaza_cpi_long <- pivot_longer(gaza_cpi_df, 
                              cols = -...1, 
                              names_to = "Date", 
                              values_to = "Price_Index")

#changing date format in my data frame
gaza_cpi_long$Date <- my(gaza_cpi_long$Date)

#renaming columns for convenience
gaza_cpi_long <- gaza_cpi_long %>%
  rename(product_name = ...1) %>% 
  rename(date = Date) %>% 
  rename(price_index = Price_Index)

#exploring data
latest_indexes <- gaza_cpi_long %>%
  group_by(product_name) %>%
  filter(date == max(date)) %>% 
  select(-date) %>% 
  mutate(product_name = case_when(
    product_name == "TOBACCO" ~ "Tobacco",
    TRUE ~ product_name)) %>% 
  arrange(desc(price_index))

#out of all these variables, I will need to choose the most essential products for the visualization first
library(stringr)

gaza_cpi_long <- gaza_cpi_long %>%
  mutate(product_name = case_when(
    product_name == "TOBACCO" ~ "Tobacco",
    TRUE ~ product_name))
  

# Define a regex pattern to match the desired words in the product names
pattern <- "(Petrol|Tobacco|Sugar|confectionery\\sand\\sdesserts|Berries,\\sfresh|Cabbages|Eggs|Citrus\\sfruits,\\sfresh|Potatoes\\sand\\sother\\stubers|Coal,\\scoal\\sbriquettes\\sand\\speat|Diesel|Salt|Semolina|Burghul|Fresh\\smeat|White\\sflour|Coffee|Raw\\sand\\swhole\\smilk|Macaroni,\\snoodles,\\scouscous\\sand\\ssimilar\\spasta\\sproducts|Homogenized\\sbaby\\sfood|Dates|Rice|Olive\\soil|Tobacco)"

# Filter based on the regex pattern
gaza_cpi_long_selection <- gaza_cpi_long %>% 
  filter(str_detect(product_name, pattern)) %>%
  group_by(product_name) %>%
  filter(date == "2023-08-01" | date == max(date)) %>%
  ungroup() %>%
  select(product_name, date, price_index)

# Filter the dataset based on the updated regex pattern
gaza_cpi_selected <- gaza_cpi_long_selection %>%
  filter(str_detect(product_name, pattern))

###Visualizing and creating a lollipop chart

# Create a new column for the x-coordinate of each label
gaza_cpi_selected$label_x <- ifelse(gaza_cpi_selected$date == min(gaza_cpi_selected$date),
                                    gaza_cpi_selected$price_index - 1,  # Initial label position
                                    gaza_cpi_selected$price_index)  # Final label position

# Create the lollipop chart with manually adjusted initial and final labels
lollipop <- ggplot(gaza_cpi_selected, aes(x = price_index, y = reorder(product_name, price_index))) +
  geom_segment(aes(x = 0, xend = price_index, yend = product_name), color = "gray") +
  geom_point(color = "blue", size = 1.5) +
  geom_text(aes(label = round(price_index, 2), x = label_x, y = product_name), 
            hjust = ifelse(gaza_cpi_selected$date == min(gaza_cpi_selected$date), 1, 0),  # Align the initial label to the right
            nudge_y = -0.3, size = 3) +  # Adjust the vertical position
  labs(title = "Rise in Price Index for Essential Products in Gaza",
       x = "Price Index",
       y = "Product Name") +
  annotate("text", x = Inf, y = -Inf, label = "Source: Palestinian Central Bureau of Statistics, March 2024", 
           hjust = 1, vjust = -1, size = 4, color = "black") +  # Add source label
  theme_minimal()

lollipop
