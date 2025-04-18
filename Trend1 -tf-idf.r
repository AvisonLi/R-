library(tm)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(furrr)
library(digest)
library(purrr)  

print(getwd())
directory_path <- "txt_folder"
txt_files <- list.files(path = directory_path, pattern = "\\.txt$", full.names = TRUE)
dir.create("filtered_articles", showWarnings = FALSE)

print("Files to be processed:")
print(txt_files)

generate_file_hash <- function(file) {
  file_content <- paste(readLines(file, warn = FALSE), collapse = " ")
  digest(file_content, algo = "md5")
}

file_hashes <- sapply(txt_files, generate_file_hash)
unique_files <- txt_files[!duplicated(file_hashes)]

print("Unique files after deduplication:")
print(unique_files)

processed_hashes <- c()
idf_cache <- list()


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


print("Sample IDF values:")
print(head(idf_values))

process_file_tfidf <- function(file, target_terms, idf) {
  print(paste("Processing file:", file))
  file_content <- tolower(paste(readLines(file), collapse = " "))
  file_hash <- digest(file_content, algo = "md5")
  
  if(file_hash %in% processed_hashes) {
    print(paste("File already processed, skipping:", file))
    return(NULL)
  }
  processed_hashes <<- c(processed_hashes, file_hash)
  
  tokens <- unlist(strsplit(file_content, "\\W+"))
  tokens <- tokens[!tokens %in% stopwords("en") & nchar(tokens) > 2]
  total_terms <- length(tokens)
  tf <- table(tokens)/total_terms
  
  tfidf <- sapply(names(tf), function(term) {
    tf[[term]] * (idf[[term]] %||% 0) 
  })
  
  result <- data.frame(
    term = names(tfidf),
    tfidf = as.numeric(tfidf),
    file = basename(file),
    stringsAsFactors = FALSE
  ) %>% 
    filter(tfidf > 0.001) %>%          
    filter(term %in% target_terms)      
  
  print(paste("Number of relevant terms found in file:", nrow(result)))
  
  if(nrow(result) > 0) {
    writeLines(file_content, paste0("filtered_articles/", basename(file)))
  }
  
  result
}

target_terms <- c(
  "aging", "population", "elderly", 
  "retirement", "healthcare", "pension", "dependency",
  "demographic", "longevity"
)
plan(multisession)
tfidf_results <- future_map_dfr(
  unique_files,
  ~process_file_tfidf(.x, target_terms, idf_values)
)

print("TF-IDF processing completed. Number of results:")
print(nrow(tfidf_results))

tfidf_results <- tfidf_results %>%
  mutate(year = str_extract(file, "\\d{4}")) %>%
  filter(!is.na(year) & year %in% 1900:2100)

yearly_counts <- tfidf_results %>%
  distinct(file, year) %>%
  group_by(year) %>%
  summarize(count = n())

output_csv <- "associated_files_for_trend2.csv"

associated_files <- tfidf_results %>%
  select(file, term, tfidf, year) 

write.csv(associated_files, output_csv, row.names = FALSE)


print(paste("Associated file names and metadata saved to:", output_csv))
print("Yearly counts of relevant articles:")
print(yearly_counts)

custom_theme <- theme(
  panel.background = element_rect(fill = "black", color = NA),
  plot.background = element_rect(fill = "black", color = NA),
  panel.grid.major = element_line(color = "gray30"),
  panel.grid.minor = element_line(color = "gray30"),
  axis.text = element_text(color = "white"),
  axis.title = element_text(color = "white"),
  plot.title = element_text(color = "white", hjust = 0.5),
  legend.background = element_rect(fill = "black", color = NA),
  legend.text = element_text(color = "white"),
  legend.title = element_text(color = "white")
)

plot <- ggplot(yearly_counts, aes(x = year, y = count, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "navy", size = 1) +
  labs(
    title = "Aging Population Coverage Trend (TF-IDF Filtered)",
    x = "Year",
    y = "Relevant Articles"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +
  theme_minimal() +
  custom_theme  


print(plot)
ggsave("yearly_trend_plot_black.png", plot = plot, width = 8, height = 6, dpi = 300)


article_rankings <- tfidf_results %>%
  group_by(file) %>%
  summarize(total_tfidf = sum(tfidf, na.rm = TRUE)) %>%
  arrange(desc(total_tfidf)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 20)  


rank_plot <- ggplot(article_rankings, aes(x = reorder(file, -total_tfidf), y = total_tfidf, fill = file)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Top 20 Articles by Total TF-IDF",
    x = "Article",
    y = "Total TF-IDF Score"
  ) +
  theme_minimal() +
  custom_theme +  # 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


print(rank_plot)
ggsave("top_20_article_bar_chart_black.png", plot = rank_plot, width = 10, height = 6, dpi = 300)


output_top20_csv <- "top20_articles.csv"

top20_articles <- article_rankings %>%
  select(file, total_tfidf, rank) %>%
mutate(year = str_extract(file, "\\d{4}")) %>%  
  filter(!is.na(year)) 

write.csv(top20_articles, output_top20_csv, row.names = FALSE)

print(paste("Top 20 articles saved to:", output_top20_csv))