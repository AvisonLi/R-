# TREND 2: THEMATIC ANALYSIS USING TF-IDF
# Builds on filtered articles from Trend 1

library(tidytext)
library(purrr)
library(tidyr)
library(stringr)
library(dplyr)# Group terms by theme and calculate total TF-IDF
theme_term_summary <- theme_results %>%
  group_by(theme, word) %>%
  summarise(
    total_tfidf = sum(tf_idf),
    total_terms = sum(n),
    .groups = "drop"
  ) %>%
  arrange(theme, desc(total_tfidf))

# Print the top terms for each theme
print("Top terms by theme:")
print(theme_term_summary)

# Save the term summary to a CSV for further inspection
write.csv(theme_term_summary, "theme_term_summary.csv", row.names = FALSE)

# Word cloud for each theme
theme_term_summary %>%
  group_by(theme) %>%
  top_n(15, total_tfidf) %>%
  ggplot(aes(label = word, size = total_tfidf, color = theme)) +
  geom_text_wordcloud() +
  facet_wrap(~theme) +
  scale_size_area(max_size = 12) +
  labs(title = "Key Terms by Theme") +
  theme_minimal()

# Load the theme analysis results
theme_analysis_results <- read.csv("theme_analysis_results.csv")

# Convert year to a factor for better visualization
theme_analysis_results$year <- as.factor(theme_analysis_results$year)

# 1. Graph: Total Terms by Theme Over the Years
ggplot(theme_analysis_results, aes(x = year, y = total_terms, color = theme, group = theme)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Total Terms by Theme Over the Years",
    subtitle = "Reasons, Impacts, and Solutions",
    x = "Year",
    y = "Total Terms"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# 2. Graph: Proportion of Focus on Themes Over the Years
theme_focus <- theme_analysis_results %>%
  group_by(year) %>%
  mutate(proportion = total_terms / sum(total_terms)) %>%
  ungroup()

ggplot(theme_focus, aes(x = year, y = proportion, fill = theme)) +
  geom_area(alpha = 0.7, position = "stack") +
  labs(
    title = "Proportion of Focus on Themes Over the Years",
    subtitle = "Reasons, Impacts, and Solutions",
    x = "Year",
    y = "Proportion of Focus"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
library(ggwordcloud)

# 1. Load Filtered Data from Trend1 -------------------------------
filtered_metadata <- read.csv("filtered_articles_metadata.csv")
relevant_files <- unique(filtered_metadata$file)

# 2. Define Theme Categories ---------------------------------------
themes <- list(
  reasons = c(
    "birth rate", "fertility", "migration", "life expectancy",
    "demographic shift", "urbanization", "longevity"
  ),
  impacts = c(
    "healthcare cost", "pension crisis", "labor shortage",
    "social security", "economic growth", "dependency ratio",
    "elderly care", "retirement age"
  ),
  solutions = c(
    "immigration policy", "automation", "healthcare reform",
    "pension reform", "productive aging", "silver economy"
  )
)

# 3. Calculate TF-IDF for Themes -----------------------------------
calculate_theme_tfidf <- function(file) {
  print(paste("Processing file:", file))
  
  content <- tolower(paste(readLines(file.path("filtered_articles", file)), collapse = " "))
  print("Original content:")
  print(substr(content, 1, 500))  # Print the first 500 characters for debugging
  
  # Preprocess for multi-word terms
  content <- content %>%
    str_replace_all(setNames(
      paste0(" ", gsub(" ", "_", unlist(themes)), " "),
      paste0(" ", unlist(themes), " ")
    ))
  print("Processed content:")
  print(substr(content, 1, 500))  # Print the first 500 characters for debugging
  
  # Create document-term matrix
  df <- tibble(text = content, doc_id = file) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(doc_id, word) %>%
    bind_tf_idf(word, doc_id, n)
  
  print("TF-IDF Data Frame:")
  print(head(df))  # Print the first few rows of the TF-IDF data frame
  
  # Classify words into themes
  df <- df %>%
    mutate(
      theme = case_when(
        word %in% gsub(" ", "_", themes$reasons) ~ "Reasons",
        word %in% gsub(" ", "_", themes$impacts) ~ "Impacts",
        word %in% gsub(" ", "_", themes$solutions) ~ "Solutions",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(theme))
  
  print("Classified Data Frame:")
  print(head(df))  # Print the first few rows of the classified data frame
  
  return(df)
}

# 4. Process All Files ---------------------------------------------
theme_results <- map_dfr(relevant_files, calculate_theme_tfidf)

# 5. Time-Based Theme Analysis -------------------------------------
theme_trends <- theme_results %>%
  left_join(filtered_metadata %>% select(file, year), by = c("doc_id" = "file")) %>%
  group_by(year, theme) %>%
  summarise(
    avg_tfidf = mean(tf_idf),
    total_terms = sum(n),
    .groups = "drop"
  )

# 6. Visualizations ------------------------------------------------
# Theme prevalence over time
ggplot(theme_trends, aes(x = year, y = avg_tfidf, color = theme)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Evolution of Aging Population Themes",
       subtitle = "Average TF-IDF Scores by Year",
       x = "Year", y = "Mean TF-IDF Score") +
  theme_minimal()

# Word cloud for each theme
theme_results %>%
  group_by(theme, word) %>%
  summarise(total_tfidf = sum(tf_idf), .groups = "drop") %>%
  group_by(theme) %>%
  top_n(15, total_tfidf) %>%
  ggplot(aes(label = word, size = total_tfidf, color = theme)) +
  geom_text_wordcloud() +
  facet_wrap(~theme) +
  scale_size_area(max_size = 12) +
  labs(title = "Key Terms by Theme") +
  theme_minimal()

# 7. Export Results ------------------------------------------------
write.csv(theme_trends, "theme_analysis_results.csv", row.names = FALSE)
print(ggplot(theme_analysis_results, aes(x = year, y = total_terms, color = theme, group = theme)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Total Terms by Theme Over the Years",
    subtitle = "Reasons, Impacts, and Solutions",
    x = "Year",
    y = "Total Terms"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_blank()
  ))
  print(ggplot(theme_analysis_results, aes(x = year, y = total_terms, color = theme, group = theme)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Total Terms by Theme Over the Years",
    subtitle = "Reasons, Impacts, and Solutions",
    x = "Year",
    y = "Total Terms"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_blank()
  ))
  print(ggplot(theme_analysis_results, aes(x = year, y = total_terms, color = theme, group = theme)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Total Terms by Theme Over the Years",
    subtitle = "Reasons, Impacts, and Solutions",
    x = "Year",
    y = "Total Terms"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_blank()
  ))
  print(ggplot(theme_analysis_results, aes(x = year, y = total_terms, color = theme, group = theme)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Total Terms by Theme Over the Years",
    subtitle = "Reasons, Impacts, and Solutions",
    x = "Year",
    y = "Total Terms"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_blank()
  ))