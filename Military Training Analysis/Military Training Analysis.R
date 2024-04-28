library(igraph)
library(ggraph)
library(dplyr)
library(tidyr)
library(circlize)
library(networkD3)
library(htmlwidgets)

sheets <- list.files("/Users/kamilvokhidov/Desktop/Data for cool analysis/Israel Annual Training Data", full.names = TRUE)

sheets

for (i in 1:length(sheets)) {
  df <- read_csv(sheets[i])

  if (i==1) {
    big_df <- df
  } else {
    big_df <- rbind(big_df, df)
  }
  print(nrow(big_df))
}

israel_training_data <- data.frame(
  course_title = rep(big_df$`Course Title`, big_df$Qty),
  training_location = rep(big_df$`Training Location`, big_df$Qty),
  student_unit = rep(big_df$`Student's Unit`, big_df$Qty))


israel_training_data_squadrons <- israel_training_data %>% 
  select(student_unit, course_title)

israel_training_data_squadrons <- israel_training_data_squadrons[israel_training_data_squadrons$student_unit!= "N/A", ]

israel_training_data_squadrons$student_unit <- toupper(israel_training_data_squadrons$student_unit)

# Define a function to merge similar course titles
merge_similar_titles <- function(student_unit) {
  # Convert course titles to lowercase for case-insensitive matching
  original_case <- tolower(student_unit)
  
  # Existing rules for merging similar course titles
  if (grepl("120th squadron", tolower(original_case))) {
    student_unit <- "120ND SQUADRON"
  } else if (grepl("131s squadron", tolower(original_case))) {
    student_unit <- "131ST SQUADRON"
  } else if (grepl("iaf[ -]*air wing 25", tolower(original_case))) {
    student_unit <- "IAF AIR WING 25"
  }
  
  # Restore original case if it starts with lowercase letters only
  if (grepl("^[a-z]+$", student_unit)) {
    student_unit <- original_case
  }
  
  return(student_unit)
}

# Apply the function to merge similar course titles
israel_training_data_squadrons$student_unit <- sapply(israel_training_data_squadrons$student_unit, merge_similar_titles)

########################################################################


merge_similar_titles_1 <- function(course_title) {
  # Convert course titles to lowercase for case-insensitive matching
  original_case <- tolower(course_title)
  
  # Define rules for merging similar course titles
  if (grepl("ah-64", original_case)) {
    course_title <- "AH-64 Training"
  } else if (grepl("patriot", original_case)) {
    course_title <- "Patriot Operator (ICC)"
  } else if (grepl("b-707", original_case)) {
    course_title <- "B-707 Training"
  } else if (grepl("c-130", original_case) && !grepl("^hc-130j", original_case)) {
    course_title <- "C-130 Training"
  } else if (grepl("g[- ]?550", original_case)) { 
    course_title <- "G-505 REC Training"
  } else if (grepl("^hc-130j ", original_case)) {
    course_title <- "HC-130J Training"
  } else if (grepl("survival", original_case)) {
    course_title <- "Survival System"
  }
  
  
  # Restore original case if it starts with lowercase letters only
  if (grepl("^[a-z]+$", course_title)) {
    course_title <- original_case
  }
  
  return(course_title)
}

# Apply the function to merge similar course titles
israel_training_data_squadrons$course_title <- sapply(israel_training_data_squadrons$course_title, merge_similar_titles_1)


########################################################################


unit_course_totals <- israel_training_data_squadrons %>%
  group_by(student_unit) %>%
  summarise(total_courses = n()) %>%
  filter(total_courses >= 85) %>%
  pull(student_unit)  # Get a vector of unit names

# Only consider courses from units with enough courses
relevant_courses <- israel_training_data_squadrons %>%
  filter(student_unit %in% unit_course_totals)

data_sankey <- relevant_courses %>%
  group_by(student_unit, course_title) %>%
  summarise(count = n(), .groups = 'drop')


nodes <- data.frame(name = unique(c(as.character(data_sankey$student_unit), as.character(data_sankey$course_title))))

data_sankey$IDsource <- match(data_sankey$student_unit, nodes$name) - 1
data_sankey$IDtarget <- match(data_sankey$course_title, nodes$name) - 1

links <- data.frame(
  source = data_sankey$IDsource,
  target = data_sankey$IDtarget,
  value = data_sankey$count
)

library(networkD3)

sankey <- sankeyNetwork(
  Links = links, 
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 20
)

library(htmlwidgets)
saveWidget(sankey, "SankeyDiagramUpdate.html", selfcontained = TRUE)

sankey












