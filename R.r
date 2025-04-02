# Load necessary libraries
library(tm)  # For text mining
library(stringr)  # For string operations
library(tidytext)  # For TF-IDF calculations
library(dplyr)  # For data manipulation
library(ggplot2)  # For plotting

# Specify the directory containing the .txt files
directory_path <- "txt_folder"

# Get a list of all .txt files in the directory
txt_files <- list.files(path = directory_path, pattern = "\\.txt$", full.names = TRUE)

# Debug: Check the list of files
print("List of .txt files:")
print(txt_files)

# Initialize a data frame to store TF-IDF results
tfidf_results <- data.frame(file = character(), term = character(), tfidf = numeric(), stringsAsFactors = FALSE)

# Define the target phrase and related terms
target_terms <- c("aging", "population", "aging population", "elderly", "productivity")

# Loop through each file and calculate TF-IDF
for (file in txt_files) {
  # Debug: Print the current file being processed
  print(paste("Processing file:", file))
  
  # Read the file content
  file_content <- tolower(paste(readLines(file, warn = FALSE), collapse = " "))
  
  # Debug: Print the entire file content
  print("Full file content:")
  print(file_content)
  
  # Tokenize the content
  tokens <- unlist(strsplit(file_content, "\\W+"))
  tokens <- tokens[!tokens %in% stopwords("en")]  # Remove stopwords
  
  # Debug: Print all tokens
  print("All tokens:")
  print(tokens)
  
  # Create a term frequency table
  term_freq <- table(tokens)
  term_freq_df <- as.data.frame(term_freq, stringsAsFactors = FALSE)
  colnames(term_freq_df) <- c("term", "freq")
  
  # Debug: Print the full term frequency table
  print("Full term frequency table:")
  print(term_freq_df)
  
  # Calculate TF-IDF
  term_freq_df <- term_freq_df %>%
    mutate(tf = freq / sum(freq)) %>%
    mutate(idf = log((length(txt_files) + 1) / (1 + sum(tokens %in% term)))) %>%  # Adjusted IDF formula
    mutate(tfidf = abs(tf * idf))  # Ensure TF-IDF values are non-negative
  
  # Debug: Print the full TF-IDF table
  print("Full TF-IDF table:")
  print(term_freq_df)
  
  # Filter terms related to "aging population"
  related_terms <- term_freq_df %>%
    filter(term %in% target_terms) %>%
    mutate(file = basename(file))
  
  # Debug: Print all related terms
  print("All related terms:")
  print(related_terms)
  
  # Append to results
  tfidf_results <- rbind(tfidf_results, related_terms)
}

# Debug: Print the full TF-IDF results
print("Full TF-IDF results:")
print(tfidf_results)

# Check if the relative frequency of related terms is >= 5%
tfidf_results <- tfidf_results %>%
  group_by(file) %>%
  summarize(total_tfidf = sum(tfidf)) %>%
  mutate(relative_percentage = total_tfidf * 100)

# Debug: Print TF-IDF results with relative percentage
print("TF-IDF results with relative percentage:")
print(tfidf_results)

# Adjust the filtering condition (lower threshold or remove temporarily)
tfidf_results <- tfidf_results %>%
  filter(relative_percentage >= 1)  # Lower threshold to 1% for testing

# Debug: Print filtered TF-IDF results
print("Filtered TF-IDF results (>= 1%):")
print(tfidf_results)

# Extract the year from the file names (assuming the year is part of the file name)
tfidf_results <- tfidf_results %>%
  mutate(year = str_extract(file, "\\d{4}"))  # Extract 4-digit year from file name

# Debug: Check if the year column has been extracted correctly
print("Extracted years:")
print(tfidf_results$year)

# Handle cases where the year is missing (e.g., NA values)
tfidf_results <- tfidf_results %>%
  filter(!is.na(year))  # Remove rows where the year could not be extracted

# Debug: Print TF-IDF results after filtering missing years
print("TF-IDF results after filtering missing years:")
print(tfidf_results)

# Count the number of files per year with >= 1% relative percentage
yearly_counts <- tfidf_results %>%
  group_by(year) %>%
  summarize(count = n())

# Debug: Check the yearly_counts data frame
print("Yearly counts:")
print(yearly_counts)

# Ensure the year column is treated as a factor for proper ordering in the plot
yearly_counts <- yearly_counts %>%
  mutate(year = as.factor(year))

# Plot the data
print(ggplot(yearly_counts, aes(x = year, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Files with >= 1% Related Terms per Year",
       x = "Year",
       y = "Count of Files") +
  theme_minimal())