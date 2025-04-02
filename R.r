# Load necessary libraries
library(tm)  # For text mining
library(stringr)  # For string operations
library(tidytext)  # For TF-IDF calculations
library(dplyr)  # For data manipulation
library(ggplot2)  # For plotting

# Specify the directory containing the .txt files
directory_path <- "path/to/your/txt/files"

# Get a list of all .txt files in the directory
txt_files <- list.files(path = directory_path, pattern = "\\.txt$", full.names = TRUE)

# Initialize a data frame to store TF-IDF results
tfidf_results <- data.frame(file = character(), term = character(), tfidf = numeric(), stringsAsFactors = FALSE)

# Define the target phrase and related terms
target_terms <- c("aging", "population", "aging population", "elderly", "productivity")

# Loop through each file and calculate TF-IDF
for (file in txt_files) {
  # Read the file content
  file_content <- tolower(paste(readLines(file), collapse = " "))
  
  # Tokenize the content
  tokens <- unlist(strsplit(file_content, "\\W+"))
  tokens <- tokens[!tokens %in% stopwords("en")]  # Remove stopwords
  
  # Create a term frequency table
  term_freq <- table(tokens)
  term_freq_df <- as.data.frame(term_freq, stringsAsFactors = FALSE)
  colnames(term_freq_df) <- c("term", "freq")
  
  # Calculate TF-IDF
  term_freq_df <- term_freq_df %>%
    mutate(tf = freq / sum(freq)) %>%
    mutate(idf = log(length(txt_files) / (1 + sum(tokens %in% term)))) %>%
    mutate(tfidf = tf * idf)
  
  # Filter terms related to "aging population"
  related_terms <- term_freq_df %>%
    filter(term %in% target_terms) %>%
    mutate(file = basename(file))
  
  # Append to results
  tfidf_results <- rbind(tfidf_results, related_terms)
}

# Check if the relative frequency of related terms is >= 5%
tfidf_results <- tfidf_results %>%
  group_by(file) %>%
  summarize(total_tfidf = sum(tfidf)) %>%
  mutate(relative_percentage = total_tfidf * 100) %>%
  filter(relative_percentage >= 5)

# Extract the year from the file names (assuming the year is part of the file name)
# Example: "document_2023.txt" -> Extract "2023"
tfidf_results <- tfidf_results %>%
  mutate(year = str_extract(file, "\\d{4}"))  # Extract 4-digit year from file name

# Count the number of files per year with >= 5% relative percentage
yearly_counts <- tfidf_results %>%
  group_by(year) %>%
  summarize(count = n())

# Plot the data
ggplot(yearly_counts, aes(x = year, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Files with >= 5% Related Terms per Year",
       x = "Year",
       y = "Count of Files") +
  theme_minimal()