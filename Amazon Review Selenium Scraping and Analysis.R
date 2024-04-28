library(RSelenium)
library(wdman)
library(netstat)
library(tidyverse)
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(udpipe)
library(tm)
library(wordcloud2)



selenium()
 
selenium_object <- selenium(retcommand =T, check =T)

#google chrome
binman::list_versions("chromedriver") 

rs_driver_object <- rsDriver(browser = "chrome", chromever = "122.0.6261.128",  verbose = F,
                   port = free_port())

#Create a client object
remDr <- rs_driver_object$client

#open browser
remDr$open()

#maximize window size

remDr$maxWindowSize()


#navigate a website

remDr$navigate("https://www.amazon.com/")

search_box <- remDr$findElement(using = 'id', 'twotabsearchtextbox') 

#now that I have identified the search box, I passing text there
search_box$sendKeysToElement(list('gaza an inquest to its martyrdom', key = 'enter'))


#finding elements (i want electronics button) you use a method, and 
#identify it based on the link, its type, or whatever,
#its distinguishing aspect
book_choice <- remDr$findElement(using = 'xpath', 
                                 value = "//*[contains(text(), 'Gaza: An Inquest into Its Martyrdom')]")

#now I have chosen a particular button, and I want to click it
book_choice$clickElement()

#see more reviews button

more_reviews <- remDr$findElement(using = 'xpath', '//a[@data-hook="see-all-reviews-link-foot"]')
more_reviews$clickElement()

#now I want to identify the verified reviews button and choose verified reviews only
verified_reviews <- remDr$findElement(using = 'xpath', value = "//span[@class='a-dropdown-prompt' and text()='All reviewers']")
verified_reviews$clickElement()

verified_reviews_only <- remDr$findElement(using = 'xpath', value = "//a[contains(text(), 'Verified purchase only')]")
verified_reviews_only$clickElement()


#now I want to identify the top reviews button and choose most recent option
recent_reviews <- remDr$findElement(using = 'xpath', value = "//span[@class='a-dropdown-prompt']")
recent_reviews$clickElement()

most_recent_reviews <- remDr$findElement(using = 'xpath', value = "//a[contains(text(), 'Most recent')]")
most_recent_reviews$clickElement()

###SCRAPING 

#identifying bigger review bodies
rating_body <- remDr$findElements(using = 'xpath', value = "//div[contains(@class, 'a-section review aok-relative')]")

collect_review <- function(rating) {
  
  # Extract based on class attribute
  review_stars <- rating$findChildElement(using = 'xpath', value = ".//i[contains(@class, 'a-star-')]")$getElementAttribute("class") %>% 
    unlist()
  review_date <- rating$findChildElement(using = 'xpath', value = ".//span[@data-hook='review-date']")$getElementText() %>% 
    unlist()
  review_content <- rating$findChildElement(using = 'xpath', value = ".//span[@data-hook='review-body']")$getElementText() %>% 
    unlist()
  return(list(Review_Stars = review_stars,
         Review_Date = review_date,
         Review_Content = review_content))
}

# Function to collect reviews from the current page
collect_reviews_from_page <- function(driver) {
  # Identifying bigger review bodies
  rating_body <- driver$findElements(using = 'xpath', value = "//div[contains(@class, 'a-section review aok-relative')]")
  
  # Extract reviews using the provided collect_review function
  reviews <- map_dfr(rating_body, collect_review)
  
  return(reviews)
}

navigate_to_next_page <- function(driver) {
  next_page_element <- remDr$findElement(using = 'xpath', value = "//a[contains(text(), 'Next page')]")
  next_page_element$clickElement()
}

# Initialize an empty data frame to hold all reviews
all_reviews_df_3 <- data.frame()

# Loop through the desired number of pages
for (page in 1:10) {
  # Scrape reviews from the current page
  page_reviews_df <- collect_reviews_from_page(remDr)
  all_reviews_df_3 <- bind_rows(all_reviews_df_3, page_reviews_df)
  
  # Introduce a delay before navigating to the next page
  Sys.sleep(time = sample(seq(1, 3, by=0.5), 1))
  
  if (page < 10) {
    navigate_to_next_page(remDr) # Navigate to the next page, avoiding this on the last iteration
    Sys.sleep(time = sample(seq(1, 3, by=0.5), 1))
  }
}

# View the final combined reviews data frame
print(all_reviews_df_3)


#Scraping is done, time for cleaning
#making a copy just in case if things get lost
all_reviews_df_copy <- all_reviews_df

all_reviews_df_2_copy <- all_reviews_df_2

all_reviews_df_2 <- all_reviews_df_2_copy

all_reviews_df_3_copy <- all_reviews_df_3

#Scraping is done, time for cleaning
all_reviews_df_3 <- all_reviews_df_3 %>% 
  mutate(book_label="Gaza martyrdom") %>% 
  select(book_label, Review_Date, Review_Stars, Review_Content)

all_reviews_df_3$Review_Date <- str_extract(all_reviews_df_3$Review_Date, 'on\\s\\w+\\s\\d{1,2},\\s\\d{4}')
all_reviews_df_3$Review_Date <- str_remove(all_reviews_df_3$Review_Date, 'on\\s')
all_reviews_df_3$Review_Date <- mdy(all_reviews_df_3$Review_Date)

all_reviews_df_3$Review_Stars <- as.numeric(str_extract(all_reviews_df_3$Review_Stars, "(?<=a-star-)\\d"))

#NOW ALL DATA IS CLEANED. I WANT TO JOIN DATASETS TOGETHER AND ANALYZE AS ONE
combined_dataset <- rbind(all_reviews_df, all_reviews_df_2, all_reviews_df_3)


#now it's time for some analysis. It's daily average rating, with geom_smooth around the line
reviews_daily <- combined_dataset %>% 
  mutate(year=year(Review_Date)) %>% 
  arrange(Review_Date) %>% 
  filter(year > 2018) %>% 
  group_by(Review_Date) %>%
  summarise(Rating=mean(Review_Stars))
  

  ggplot(reviews_daily, aes(x = (as.Date(Review_Date)), y = Rating)) +
    geom_vline(xintercept = as.numeric(as.Date("2023-10-07")), colour = "indianred2", linetype = "dashed")+
  geom_smooth(method = "loess", size = 1, colour = "lightseagreen", fill = "purple") +
  geom_point(alpha = 1, colour = "lightseagreen") +
  theme_light() +
    theme(plot.title = element_text(size=16))+
    scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") +
    labs(x = "Month and Year", y = "Rating", title = "Daily Review Ratings Over Time")

  
#next I want monthly frequency of reviews
reviews_monthly <- combined_dataset %>% 
    mutate(month=month(Review_Date)) %>% 
    mutate(year=year(Review_Date)) %>% 
    group_by(year, month) %>%
    summarise(frequency=n()) %>% 
    arrange(desc(year), desc(month))
  
# Assuming your data starts and ends in these years
start_year <- min(reviews_monthly$year)
end_year <- max(reviews_monthly$year)

# Create a data frame of all months and years in the range
all_months <- expand.grid(
  year = start_year:end_year,
  month = 1:12
)

# Ensure 'month' and 'year' are in the same format as your reviews_monthly
all_months <- all_months %>% 
  mutate(month = as.integer(month), 
         year = as.integer(year))

# Join with your data to include months with zero reviews
reviews_monthly_complete <- left_join(all_months, reviews_monthly, by = c("year", "month")) %>%
  replace_na(list(frequency = 0)) %>%
  arrange(desc(year), desc(month)) %>%
  filter(year > 2018) %>%
  mutate(date = make_date(year, month, 1)) %>%
  filter(date <= as.Date("2024-03-31")) # Limit to data up to March 2024

year_breaks <- seq(from = as.Date("2020-01-01"), to = as.Date("2023-01-01"), by = "1 year")

# Explicitly add the dates for Jan 2020 and March 2024 to your breaks
final_breaks <- c(as.Date("2019-01-01"), year_breaks, as.Date("2024-03-01"))

# Create labels for the breaks, starting with "Jan 2020", then the years 2021 to 2023, and adding "March 2024" specifically
final_labels <- c("Jan 2019", format(year_breaks, "%Y"), "March 2024")

# Plot
ggplot(reviews_monthly_complete, aes(x = date, y = frequency)) +
  geom_vline(xintercept = as.numeric(as.Date("2023-10-07")), colour = "red3", linetype = "dashed")+
  geom_col(width = 20) +
  geom_col(fill = "violet", width = 20) +# Adjust width as needed
  scale_x_date(breaks = final_breaks,
               labels = final_labels,
               limits = c(as.Date("2019-01-01"), as.Date("2024-03-31"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "Number of Reviews", title = "Monthly Reviews from Jan 2019 to Mar 2024") +
  theme_minimal()

#Now I want to do some linguistic analysis of the reviews. I will use Samir's friend's technique and prepare text data for visualization
comments_only <- combined_dataset %>% 
  mutate(content=Review_Content) %>% 
  mutate(date=Review_Date) %>% 
  select(date,content) 

# Download the English model (this only needs to be done once)
ud_model <- udpipe_download_model(language = "english", model_dir = ".")
# Load the model
model <- udpipe_load_model(ud_model$file_model)

# Assuming 'comments_only_df$content' is your text data
annotated <- udpipe_annotate(model, x = comments_only$content)

# Convert annotations to data frame
annotated_df <- as.data.frame(annotated)

selected_words <- c(
  "truth", "Palestinian", "facts", "cleansing", "ethnic", "great", "important", 
  "lies", "killed", "researched", "documented", "Zionist", "recommend", 
  "crimes", "detailed", "evidence", "informative", "systematic", "thorough", "massacres", 
  "scholarship", "genocide", "outrage", "excellent", "martyrdom", "meticulous", "necessary", 
  "accurate", "biased", "indigenous", "occupation", "sad", "Amazing", "atrocities", 
  "comprehensive", "destroy", "devastating", "expulsion", "incredible", "justice", 
  "heartbreaking", "courageous", "exposes", "groundbreaking", "injustice", "investigation", 
  "terror", "valuable", "apartheid", "archival", "appalling", "cynicism", "humiliate", "murderous", "plight"
)
filter(token %in% selected_words)

# Filter for adjectives. Common POS tags for adjectives include 'JJ' (general adjective), 'JJR' (comparative adjective), and 'JJS' (superlative adjective).
nouns_adjs <- annotated_df %>%
  filter(upos %in% c("NOUN", "VERB", "ADJ")) %>% 
  filter(token %in% selected_words) %>% 
  select(token) %>% 
  group_by(token) %>% 
  summarize(total=n()) %>% 
  arrange(desc(total))

#visualization part
wordcloud(words = nouns_adjs$token, max.words = 50, random.order = FALSE, 
          rot.per = 0, scale = c(2.5, 0.5), 
          colors = brewer.pal(8, "Set1"))

wordcloud2(data = data.frame(words = nouns_adjs$token, freq = rep(1, length(nouns_adjs$token))), size = 0.3)




filter(date >= as.Date("2019-01-01")) %>% 
  mutate(review_num=row_number()) %>% 
  unnest_tokens(output = word, input = content) %>%
  anti_join(stop_words) %>% 
  group_by(review_num) %>% 
  mutate(content = paste(word, collapse = " ")) %>%
  ungroup() %>% 
  select(-word) %>% 
  distinct()

# visualization  part
# Combine all reviews into one big text blob
big_text_blob <- paste(comments_only_df$content, collapse = " ")

# Split the big text blob into individual words
words <- strsplit(big_text_blob, " ")[[1]]

wordcloud(words = words, 
          max.words = 50, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))







adj_words <- annotated_df %>% 
  filter(upos %in% c("ADJ")) %>% # Use "ADJ" for universal POS tags
  select(token) %>% 
  group_by(token) %>% 
  summarize(total=n()) %>% 
  arrange(desc(total))





selected_words <- c("Palestinian", "Cleansing", "truth", "Zionist", "facts", "systematic", 
                    "documented", "massacres", "genocide", "recommend", "crimes", "expulsion",
                    "biased", "expelled", "excellent", "archives", "compelling", "evidence",
                    "factual", "researched", "accurate")



















































first_review <- remDr$findElement(using = 'xpath', value = "//*[@id='customer_review-RFRTTJL3WM474']")
first_review$getElementText()




review_date <- rating$findChildElement(using = 'xpath', value = ".//span[@data-hook='review-date']")$getElementText() %>% 
  unlist()

date1 <- remDr$findElements(using = 'xpath', value = "//span[@data-hook='review-date']")

review_date1 <- lapply(date1, function (x) x$getElementText()) %>% 
  unlist()

view(review_date1)

stars1 <- remDr$findElements(using = 'xpath', value = '//*[(@id = "cm_cr-review_list")]//*[contains(concat( " ", @class, " " ), concat( " ", "review-rating", " " ))]')

length(stars1)

review_stars1 <- lapply(stars1, function (x) x$getElementText()) %>% 
  unlist()

view(review_stars1)

















#going back to the previous page
remDr$goBack()
remDr$goForward() 

#now i want to search item on the webiste, and i locate search box



#scrolling to the end of the page with the Java script
remDr$executeScript("window.scrollTo(document.body.scrollHeight, 0);")

#click on the United States filter box; // - means looking for input tags only, where the aria label attribute of input is the United States
us_checkbox <- remDr$findElement(using = 'xpath', '//input[@aria-label="United States"]')
# we double check if the path has worked
us_checkbox$getElementAttribute('aria-label')

#now i want to click on it
us_checkbox$clickElement()

remDr$refresh()

#now if you need to open a dropdown filter, you gotta click on the arrow key
remDr$findElement(using = 'xpath', '//*[text()="Color"]')$clickElement()
#choosing white color
us_checkbox <- remDr$findElement(using = 'xpath', '//input[@aria-label="White"]')$clickElement()
# if I click on that thing again, it will unclick

#now I want the average price for playstation 5 on the first page of the website

#first, I want to identify the prices of many items, so it's findelemtS
prices <- remDr$findElements(using = 'class name', 's-item__price')

#now I want to extract the text from a list of items
length(prices)

price_values <- lapply(prices, function (x) x$getElementText()) %>% 
  unlist() %>% 
  str_remove_all('[$]')

price_values = price_values[-c(1,2,4,5,16,17,21,30,35,43, 45, 47)]

#convert from string to number
price_values = price_values %>% 
  as.numeric()

mean(price_values)














































































































#close the server 
rs_driver_object$server$stop() # OR
