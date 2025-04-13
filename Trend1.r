# Load necessary libraries
library(tm)  # For text mining
library(stringr)  # For string operations
library(dplyr)  # For data manipulation
library(ggplot2)  # For plotting

# Specify the directory containing the .txt files
directory_path <- "txt_folder"

# Get a list of all .txt files in the directory
txt_files <- list.files(path = directory_path, pattern = "\\.txt$", full.names = TRUE)

# Debug: Check the list of files
print("List of .txt files:")
print(txt_files)

# Initialize a data frame to store TF results
tf_results <- data.frame(file = character(), term = character(), tf = numeric(), stringsAsFactors = FALSE)

# Define the target phrase and related terms
target_terms <- c(
  "aging",
  "population",
  "elderly",
  "productivity",
  "retirement",
  "healthcare",
  "pension", #退休金
  "dependency ratio", # 依賴比率
  "demographic shift", # 人口結構變化
  "longevity" # 長壽
  ) 

# Calculate TF for each file
for (file in txt_files) {
  # Debug: Print the current file being processed
  print(paste("Processing file:", file))
  
  # Read the file content
  file_content <- tolower(paste(readLines(file, warn = FALSE), collapse = " "))
  
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
  
  # Calculate TF (Term Frequency)
  term_freq_df <- term_freq_df %>%
    mutate(tf = freq / sum(freq)) %>%
    mutate(file = basename(file))
  
  # Debug: Print the TF table
  print("TF table:")
  print(term_freq_df)
  
  # Filter terms related to "aging population"
  related_terms <- term_freq_df %>%
    filter(term %in% target_terms)
  
  # Append to results (avoid duplicates)
  related_terms <- related_terms %>%
    mutate(file = basename(file))  # Add file name to related terms

  if (!basename(file) %in% tf_results$file) {
    tf_results <- rbind(tf_results, related_terms)
  }
}

# Debug: Print the full TF results
print("Full TF results:")
print(tf_results)

# Extract the year from the file names
tf_results <- tf_results %>%
  mutate(year = str_extract(file, "\\d{4}")) %>%
  filter(!is.na(year) & year %in% 1900:2100)  # Ensure valid years

# Debug: Check if the year column has been extracted correctly
print("Extracted years:")
print(tf_results$year)

# # Handle cases where the year is missing (e.g., NA values)
# tf_results <- tf_results %>%
#   filter(!is.na(year))  # Remove rows where the year could not be extracted

# # Debug: Print TF results after filtering missing years
# print("TF results after filtering missing years:")
# print(tf_results)

# Extract all years from the file names
all_years <- str_extract(basename(txt_files), "\\d{4}")  # Extract 4-digit years
all_years <- unique(all_years[!is.na(all_years)])  # Remove NA values and get unique years

# Create a data frame with all years and initialize counts to 0
all_years_df <- data.frame(year = all_years, stringsAsFactors = FALSE)

# Count the number of files per year with related terms
yearly_counts <- tf_results %>%
  distinct(file, year) %>%  # Ensure each file is counted only once
  group_by(year) %>%
  summarize(count = n(), .groups = "drop")  # Count files per year

# Merge with all years to ensure all years are included
yearly_counts <- full_join(all_years_df, yearly_counts, by = "year") %>%
  mutate(count = ifelse(is.na(count), 0, count))  # Replace NA counts with 0

# Debug: Print the yearly_counts data frame
print("Yearly counts (including missing years):")
print(yearly_counts)

# Ensure the year column is treated as a factor for proper ordering in the plot
yearly_counts <- yearly_counts %>%
  mutate(year = as.factor(year))

# Plot the data with bars and a trend line
print(ggplot(yearly_counts, aes(x = year, y = count, group = 1)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +  # Bar chart
  geom_line(color = "red", size = 1) +  # Trend line
  geom_point(color = "red", size = 2) +  # Points on the trend line
  labs(title = "Number of Files with Related Terms per Year",
       x = "Year",
       y = "Count of Files") +
  theme_minimal())