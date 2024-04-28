#loading all necessary libraries
library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(tm)
library(topicmodels)
library(tidytext)
library(stm)
library(ggplot2)

#importing the scraped data
text_document <- read_csv("/Users/kamilvokhidov/Desktop/Data for cool analysis/Bibi_Bibi.csv")

#cleaning it
text_document$Date <- dmy(text_document$Date)
text_document$Date <- as.character(text_document$Date)


text_document_1 <-
  text_document %>%
  mutate(Content = Content) %>%
  unnest_tokens(word, Content) %>%
  anti_join(get_stopwords()) %>% 
  filter(!word %in% c("prime", "minister", "netanyahu", "also", "benjamin")) # Corrected filter condition
  
#doing some exploration of data
text_sparse <- text_document_1  %>% 
  count(Date, word, sort = TRUE) %>%
  cast_sparse(Date, word, n)

dim(text_sparse)  # Added closing parenthesis for count function

#Topic Modeling
topic_model <- stm(text_sparse, K = 3)

speech_topic <- tidy(topic_model, matrix = "beta")

library(dplyr)
library(forcats)

# Define the desired labels for each topic
topic_labels <- c("Netanyahu's Nationalism", "Israeli State Affairs", "Hostages and Families")

speech_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  mutate(term = fct_reorder(term, beta),
         topic = factor(topic, labels = topic_labels)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_y_reordered() +
  labs(x = "Probability of Co-Occurrence", y = NULL)


# Topic Modeling for gamma matrix
topic_model <- stm(text_sparse, K = 3)

# Visualize topics changing over time (gamma matrix)
date_topics <- tidy(topic_model, matrix = "gamma",
                    document_names = rownames(text_sparse))


#Convert 'Date' column to Date format
date_topics$Date <- as.Date(date_topics$document)

library(dplyr)
library(forcats)

# Define the desired labels for each topic
topic_labels <- c("Netanyahu's Nationalism", "Israeli State Affairs", "Hostages and Families")

date_topics$MonthAbbrev <- substr(month.abb[month(date_topics$Date)], 1, 1)

# Convert 'topic' variable to factor with desired labels
date_topics$topic <- factor(date_topics$topic, labels = topic_labels)

ggplot(date_topics, aes(x = Date, y = gamma, color = topic)) +
  geom_smooth(aes(group = topic), method = "loess", size = 1.5, alpha = 0.8, se = FALSE, span = 0.75) +
  scale_color_manual(values = c("red", "blue", "green")) +  # Assign colors directly without using labels
  labs(x = "Date", y = "Probability", color = "Topic") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(title = "Topic"))) +  # Override legend title
  scale_x_date(date_labels = "%b", breaks = "1 month")  # Format x-axis labels to display only the first letter of each month

library(wordcloud)

# Assuming your text document is named text_document_1 and the word frequencies are counted
word_freq <- text_document_1 %>%
  count(word, sort = TRUE)

# Define a color palette
my_colors <- brewer.pal(8, "Dark2")

# Generate word cloud with colors
wordcloud(words = word_freq$word, freq = word_freq$n, max.words = 80, random.order = FALSE)



