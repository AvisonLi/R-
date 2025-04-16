library(tm)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(furrr)
library(digest)
library(purrr)  # Ensure purrr is loaded

# 1. File Preparation --------------------------------------------
directory_path <- "txt_folder"
txt_files <- list.files(path = directory_path, pattern = "\\.txt$", full.names = TRUE)
dir.create("filtered_articles", showWarnings = FALSE)

# Print the list of files being processed
print("Files to be processed:")
print(txt_files)

# 2. Document Hashing (Same as Original) -------------------------
generate_file_hash <- function(file) {
  file_content <- paste(readLines(file, warn = FALSE), collapse = " ")
  digest(file_content, algo = "md5")
}

file_hashes <- sapply(txt_files, generate_file_hash)
unique_files <- txt_files[!duplicated(file_hashes)]

# Print unique files after deduplication
print("Unique files after deduplication:")
print(unique_files)

# 3. TF-IDF Calculation Core -------------------------------------
processed_hashes <- c()
idf_cache <- list()

# First Pass: Calculate IDF for all terms
calculate_idf <- function(files) {
  doc_freq <- list()
  
  walk(files, ~{
    print(paste("Calculating IDF for file:", .x))
    content <- tolower(paste(readLines(.x), collapse = " "))
    terms <- unique(unlist(strsplit(content, "\\W+")))
    terms <- terms[!terms %in% stopwords("en")]
    terms <- terms[nchar(terms) > 2]
    
    for(term in terms) {
      doc_freq[[term]] <<- doc_freq[[term]] %||% 0
      doc_freq[[term]] <<- doc_freq[[term]] + 1
    }
  })
  
  total_docs <- length(files)
  idf <- lapply(doc_freq, function(df) log(total_docs / (df + 1)))
  print("IDF calculation completed.")
  idf
}

idf_values <- calculate_idf(unique_files)

# Print a sample of IDF values
print("Sample IDF values:")
print(head(idf_values))

# Second Pass: Process files with TF-IDF
process_file_tfidf <- function(file, target_terms, idf) {
  print(paste("Processing file:", file))
  file_content <- tolower(paste(readLines(file), collapse = " "))
  file_hash <- digest(file_content, algo = "md5")
  
  if(file_hash %in% processed_hashes) {
    print(paste("File already processed, skipping:", file))
    return(NULL)
  }
  processed_hashes <<- c(processed_hashes, file_hash)
  
  # Calculate TF
  tokens <- unlist(strsplit(file_content, "\\W+"))
  tokens <- tokens[!tokens %in% stopwords("en") & nchar(tokens) > 2]
  total_terms <- length(tokens)
  tf <- table(tokens)/total_terms
  
  # Calculate TF-IDF
  tfidf <- sapply(names(tf), function(term) {
    tf[[term]] * (idf[[term]] %||% 0)  # Use 0 if term not in IDF
  })
  
  # Create result dataframe
  result <- data.frame(
    term = names(tfidf),
    tfidf = as.numeric(tfidf),
    file = basename(file),
    stringsAsFactors = FALSE
  ) %>% 
    filter(tfidf > 0.0001) %>%           # TF-IDF threshold
    filter(term %in% target_terms)      # Target terms filter
  
  print(paste("Number of relevant terms found in file:", nrow(result)))
  
  if(nrow(result) > 0) {
    writeLines(file_content, paste0("filtered_articles/", basename(file)))
  }
  
  result
}

# 4. Target Terms & Execution ------------------------------------
target_terms <- c(
  "aging", "population", "elderly", "productivity",
  "retirement", "healthcare", "pension", "dependency",
  "demographic", "longevity","social security", "labor market", "chronic disease",
  "intergenerational", "caregiver", "ageing society",
  "government policy", "economic burden", "demographic dividend",
  "life expectancy"
)

plan(multisession)
tfidf_results <- future_map_dfr(
  unique_files,
  ~process_file_tfidf(.x, target_terms, idf_values)
)

# Print the number of results generated
print("TF-IDF processing completed. Number of results:")
print(nrow(tfidf_results))

# 5. Yearly Analysis (Same Visualization) ------------------------
tfidf_results <- tfidf_results %>%
  mutate(year = str_extract(file, "\\d{4}")) %>%
  filter(!is.na(year) & year %in% 1900:2100)

yearly_counts <- tfidf_results %>%
  distinct(file, year) %>%
  group_by(year) %>%
  summarize(count = n())

# Save associated .txt file names into a CSV for Trend2
output_csv <- "associated_files_for_trend2.csv"

associated_files <- tfidf_results %>%
  select(file, term, tfidf, year) 

write.csv(associated_files, output_csv, row.names = FALSE)


# Print confirmation
print(paste("Associated file names and metadata saved to:", output_csv))
# Print yearly counts
print("Yearly counts of relevant articles:")
print(yearly_counts)

# 定义保存图表的文件路径
output_plot <- "yearly_trend_plot.png"

# 创建图表
plot <- ggplot(yearly_counts, aes(x = year, y = count, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "navy", size = 1) +
  labs(
    title = "Aging Population Coverage Trend (TF-IDF Filtered)",
    x = "Year",
    y = "Relevant Articles"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +
  theme_minimal()

# 显示图表
print(plot)

# 保存图表
ggsave(output_plot, plot = plot, width = 8, height = 6, dpi = 300)

# 打印确认信息
print(paste("Plot saved to:", output_plot))