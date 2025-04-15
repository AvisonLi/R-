install.packages("furrr")
library(tm)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(furrr)
library(digest)

directory_path <- "txt_folder"
txt_files <- list.files(path = directory_path, pattern = "\\.txt$", full.names = TRUE)

dir.create("filtered_articles", showWarnings = FALSE)

generate_file_hash <- function(file) {
  file_content <- paste(readLines(file, warn = FALSE), collapse = " ")
  digest(file_content, algo = "md5")
}

file_hashes <- sapply(txt_files, generate_file_hash)
unique_files <- txt_files[!duplicated(file_hashes)]

processed_hashes <- c()

process_file <- function(file, target_terms) {
  file_content <- paste(readLines(file, warn = FALSE), collapse = " ")
  file_hash <- digest(file_content, algo = "md5")

  if (file_hash %in% processed_hashes) {
    return(NULL)
  }

  processed_hashes <<- c(processed_hashes, file_hash)
  file_content <- tolower(file_content)
  tokens <- unnest_tokens(tbl = data.frame(text = file_content), output = "tokens", input = "text")
  tokens <- tokens[!tokens$tokens %in% stopwords("en"), ]
  term_freq <- table(tokens)
  term_freq_df <- as.data.frame(term_freq, stringsAsFactors = FALSE)
  colnames(term_freq_df) <- c("term", "freq")

  # Calculate total frequency for the article
  total_freq <- sum(term_freq_df$freq)

  # Filter terms contributing more than 5% of the total frequency
  term_freq_df <- term_freq_df %>%
    filter(freq / total_freq > 0.0001) %>%
    filter(term %in% target_terms) %>%
    mutate(file = basename(file))

  if (nrow(term_freq_df) > 0) {
    writeLines(file_content, paste0("filtered_articles/", basename(file)))
  }

  return(term_freq_df)
}

target_terms <- c(
  "aging",
  "population",
  "elderly",
  "productivity",
  "retirement",
  "healthcare",
  "pension",
  "dependency ratio",
  "demographic shift",
  "longevity"
)

plan(multisession)
tf_results <- future_map_dfr(unique_files, ~ process_file(.x, target_terms))

tf_results <- tf_results %>%
  mutate(year = str_extract(file, "\\d{4}")) %>%
  filter(!is.na(year) & year %in% 1900:2100)

all_years <- str_extract(basename(txt_files), "\\d{4}")
all_years <- unique(all_years[!is.na(all_years)])
all_years_df <- data.frame(year = all_years, stringsAsFactors = FALSE)

yearly_counts <- tf_results %>%
  distinct(file, year) %>%
  group_by(year) %>%
  summarize(count = n(), .groups = "drop")

yearly_counts <- full_join(all_years_df, yearly_counts, by = "year") %>%
  mutate(count = ifelse(is.na(count), 0, count))

yearly_counts <- yearly_counts %>%
  mutate(year = as.factor(year))

print(ggplot(yearly_counts, aes(x = year, y = count, group = 1)) +
  #geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Number of Files with Related Terms per Year",
       x = "Year",
       y = "Count of Files") +
  theme_minimal())
write.csv(tf_results, "filtered_articles_metadata.csv", row.names = FALSE)
