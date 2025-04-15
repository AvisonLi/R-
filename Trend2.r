# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidytext)
library(tm)

# Load the filtered metadata from Trend1
filtered_metadata <- read.csv("filtered_articles_metadata.csv")

# Get the list of relevant .txt files
relevant_files <- unique(filtered_metadata$file)

# Specify the directory containing the .txt files
directory_path <- "txt_folder"

# Define categories for reasons and impacts
reasons_terms <- c(
  "population", "dependency ratio", "demographic shift", "birth rate",
  "fertility rate", "migration", "urbanization", "life expectancy",
  "aging population", "median age")


impacts_terms <- c(
  "retirement", "healthcare", "pension", "productivity", "longevity",
  "elderly care", "social security", "labor shortage", "economic growth",
  "healthcare costs", "dependency burden", "housing demand", "intergenerational equity"
)

# Initialize a data frame to store term frequencies
tf_results <- data.frame(file = character(), term = character(), freq = numeric(), stringsAsFactors = FALSE)

# Process each relevant file
for (file_name in relevant_files) {
  file_path <- file.path(directory_path, file_name)
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    next
  }
  
  # Read the file content
  file_content <- tolower(paste(readLines(file_path, warn = FALSE), collapse = " "))
  
  # Tokenize the content
  tokens <- unlist(strsplit(file_content, "\\W+"))
  tokens <- tokens[!tokens %in% stopwords("en")]  # Remove stopwords
  
  # Create a term frequency table
  term_freq <- table(tokens)
  term_freq_df <- as.data.frame(term_freq, stringsAsFactors = FALSE)
  colnames(term_freq_df) <- c("term", "freq")
  
  # Filter terms that are in reasons or impacts
  term_freq_df <- term_freq_df %>%
    filter(term %in% c(reasons_terms, impacts_terms)) %>% 
    mutate(file = file_name)
  
  # Append to the results
  tf_results <- bind_rows(tf_results, term_freq_df)
}

# Categorize terms as reasons or impacts
tf_results <- tf_results %>%
  mutate(category = case_when(
    term %in% reasons_terms ~ "Reason",
    term %in% impacts_terms ~ "Impact",
    TRUE ~ "Other"
  ))

# Summarize the total frequency of reasons and impacts
category_summary <- tf_results %>%
  group_by(category) %>%
  summarize(total_freq = sum(freq), .groups = "drop")

# Plot the distribution of reasons vs impacts
print(ggplot(category_summary, aes(x = category, y = total_freq, fill = category)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Distribution of Reasons and Impacts of Aging Population",
       x = "Category",
       y = "Total Frequency") +
  theme_minimal())

# Analyze the frequency of specific terms within each category
term_category_summary <- tf_results %>%
  group_by(category, term) %>%
  summarize(total_freq = sum(freq), .groups = "drop")

# Plot the frequency of terms within each category
print(ggplot(term_category_summary, aes(x = term, y = total_freq, fill = category)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Frequency of Terms by Category (Reasons vs Impacts)",
       x = "Term",
       y = "Total Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))
