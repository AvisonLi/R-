# Load necessary libraries
library(tm)
library(stringr)
library(dplyr)
library(ggplot2)

# Specify the directory containing the .txt files
directory_path <- "txt_folder"

# Get a list of all .txt files in the directory
txt_files <- list.files(path = directory_path, pattern = "\\.txt$", full.names = TRUE)

# Debug: Check the list of files
print("List of .txt files:")
print(txt_files)

# Initialize a data frame to store TF results
tf_results <- data.frame(file = character(), term = character(), freq = numeric(), stringsAsFactors = FALSE)

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

# Calculate term frequencies for each file
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
  
  # Filter terms related to target terms
  related_terms <- term_freq_df %>%
    filter(term %in% target_terms) %>%
    mutate(file = basename(file))  # Add file name to related terms
  
  # Append to results
  tf_results <- rbind(tf_results, related_terms)
}

# Debug: Print the full TF results
print("Full TF results:")
print(tf_results)

# Summarize the total frequency of each term across all files
term_summary <- tf_results %>%
  group_by(term) %>%
  summarize(total_freq = sum(freq), .groups = "drop")  # Sum frequencies for each term

# Debug: Print the term summary
print("Term summary:")
print(term_summary)

# Plot the data with x-axis as terms and y-axis as total frequency
print(ggplot(term_summary, aes(x = term, y = total_freq)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +  # Bar chart
  labs(title = "Total Frequency of Target Terms Across All Files",
       x = "Terms",
       y = "Total Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))  # Rotate x-axis labels for better readability