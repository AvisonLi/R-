# Define the directory containing the .txt files
txt_directory <- "c:\\Users\\liari\\OneDrive\\桌面\\R project\\R-\\txt_files"

# Get the list of .txt files in the directory
txt_files <- list.files(path = txt_directory, pattern = "\\.txt$", full.names = TRUE)

# Process each .txt file
for (file_path in txt_files) {
  # Read the file content
  file_content <- tolower(paste(readLines(file_path, warn = FALSE), collapse = " "))
  
  # Tokenize the content
  tokens <- unlist(strsplit(file_content, "\\W+"))
  tokens <- tokens[!tokens %in% stopwords("en")]  # Remove stopwords
  
  # Filter out very short words (length < 3)
  tokens <- tokens[nchar(tokens) >= 3]  # Keep words with 3 or more characters
  
  # Remove tokens that are numbers
  tokens <- tokens[!grepl("^\\d+$", tokens)]  # Exclude tokens that are entirely numeric
  
  # Create a term frequency table
  term_freq <- table(tokens)
  
  # Skip if the term_freq table is empty
  if (length(term_freq) == 0) {
    next
  }
  
  # Convert the term frequency table to a data frame
  term_freq_df <- as.data.frame(term_freq, stringsAsFactors = FALSE)
  colnames(term_freq_df) <- c("word", "freq")
  
  # Add the file name to the results
  term_freq_df$file <- basename(file_path)
  
  # Append to the results
  word_freq_results <- bind_rows(word_freq_results, term_freq_df)
}
# Write the results to a CSV file
write.csv(word_freq_results, "word_frequencies.csv", row.names = FALSE)

# Print the top 10 most frequent words
print(head(word_freq_results, 100))



