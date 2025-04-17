# TREND 2: THEMATIC ANALYSIS USING TF-IDF
# Builds on filtered articles from Trend 1

library(tidytext)
library(purrr)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)

# 1. Load Filtered Data from Trend1 -------------------------------
filtered_metadata <- read.csv("associated_files_for_trend2.csv")
relevant_files <- unique(filtered_metadata$file)  # 从 CSV 中提取文件名

# 2. Define Theme Categories ---------------------------------------
themes <- list(
  reasons = c(
    "birth rate", "fertility", "migration", "life expectancy",
    "demographic shift", "urbanization", "longevity",
    "improved healthcare", "medical advancements", "better living standards",
    "delayed parenthood", "family planning", "nutrition improvements"
  ),
  impacts = c(
    "healthcare cost", "pension crisis", "labor shortage",
    "social security", "economic growth", "dependency ratio",
    "elderly care", "retirement age", "shrinking workforce",
    "rising healthcare costs", "pension burden", "elderly dependency ratio",
    "economic slowdown", "intergenerational inequality", "pressure on healthcare systems",
    "age-related diseases", "social isolation of elderly", "changing family structure",
    "increased demand for eldercare services", "public spending pressures"
  ),
  solutions = c(
    "immigration policy", "automation", "healthcare reform",
    "pension reform", "productive aging", "silver economy",
    "raising retirement age", "encouraging higher fertility rates", "promoting healthy aging",
    "attracting skilled immigrants", "lifelong learning", "adult education",
    "flexible work for older adults", "investing in eldercare infrastructure",
    "family support policies", "intergenerational housing", "public awareness campaigns"
  )
)

# 3. Calculate Theme for Each Article -----------------------------
calculate_article_theme <- function(file) {
  content <- tolower(paste(readLines(file.path("filtered_articles", file)), collapse = " "))
  
  # Preprocess for multi-word terms
  content <- content %>%
    str_replace_all(setNames(
      paste0(" ", gsub(" ", "_", unlist(themes)), " "),
      paste0(" ", unlist(themes), " ")
    ))
  
  # Tokenize and classify terms into themes
  df <- tibble(text = content, doc_id = file) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    mutate(
      theme = case_when(
        word %in% gsub(" ", "_", themes$reasons) ~ "Reasons",
        word %in% gsub(" ", "_", themes$impacts) ~ "Impacts",
        word %in% gsub(" ", "_", themes$solutions) ~ "Solutions",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(theme)) %>%
    count(doc_id, theme, word, name = "keyword_count")  # 统计每个关键词的出现次数
  
  return(df)
}

# 4. Process All Files ---------------------------------------------
theme_results <- map_dfr(relevant_files, calculate_article_theme)

# 确保 doc_id 和 file 匹配
theme_results <- theme_results %>%
  mutate(doc_id = basename(doc_id))  # 提取文件名

# 调试：检查 theme_results 的内容
print("Debug: theme_results 内容")
print(head(theme_results))

# 合并 tfidf 列
debug_data <- theme_results %>%
  left_join(filtered_metadata %>% select(file, year, tfidf), by = c("doc_id" = "file"))

# 调试：检查合并后的数据
print("Debug: 合并后的 debug_data 内容")
print(head(debug_data))

# 确保 tfidf 列存在
if ("tfidf" %in% colnames(debug_data)) {
  # 在分组前过滤
  theme_article_counts <- debug_data %>%
    filter(tfidf > 0.0001) %>%
    group_by(year, theme, word) %>%
    summarise(
      keyword_count = sum(keyword_count),  # 汇总关键词计数
      tfidf = mean(tfidf),  # 计算平均 TF-IDF 值
      .groups = "drop"
    )

  # 确保年份为整数并逐年显示
  theme_article_counts <- theme_article_counts %>%
    mutate(year = as.integer(year))  # 将年份转换为整数

  # 检查数据
  print("Debug: theme_article_counts 数据")
  print(head(theme_article_counts))
 # 更新圖表樣式：黑色背景，白色字體
  custom_theme <- theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", face = "bold"),
    plot.subtitle = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = NA),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )
  # 绘制图表：按主题区分
  plot <- ggplot(theme_article_counts, aes(x = year, y = keyword_count, fill = theme)) +
    geom_bar(stat = "identity", position = "dodge") +  # 使用柱狀圖，並按主題分組
    scale_x_continuous(breaks = seq(min(theme_article_counts$year), max(theme_article_counts$year), by = 1)) +  # 确保年份逐年显示
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +  # y 轴为整数
    labs(
      title = "Number of Keywords by Theme Over the Years",
      subtitle = "Reasons, Impacts, and Solutions",
      x = "Year",
      y = "Keyword Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      legend.title = element_blank()  # 隐藏图例标题
    )

  # 显示图表
  print(plot)

  # 保存图表
  output_plot <- "theme_keywords_trend_bar.png"
  ggsave(output_plot, plot = plot, width = 10, height = 6, dpi = 300)

  # 打印确认信息
  print(paste("Bar chart saved to:", output_plot))

  # 调试：检查过滤后的数据
  print("Debug: 合并并过滤后的 theme_article_counts")
  print(head(theme_article_counts))

  # 保存结果到 CSV
  write.csv(theme_article_counts, "theme_article_counts_with_keywords.csv", row.names = FALSE)

  # 计算所有主题的总计数
  total_counts <- theme_article_counts %>%
    group_by(year) %>%
    summarise(
      total_keyword_count = sum(keyword_count),  # 汇总所有主题的关键词计数
      .groups = "drop"
    )

  # 绘制总计数图表
  total_plot <- ggplot(total_counts, aes(x = year, y = total_keyword_count)) +
    geom_line(color = "purple", size = 1) +
    geom_point(color = "darkviolet", size = 3) +
    scale_x_continuous(breaks = seq(min(total_counts$year), max(total_counts$year), by = 1)) +  # 确保年份逐年显示
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +  # y 轴为整数
    labs(
      title = "Total Keywords Count Over the Years",
      subtitle = "Combined Reasons, Impacts, and Solutions",
      x = "Year",
      y = "Total Keyword Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold")
    )

  # 显示总计数图表
  print(total_plot)

  # 保存总计数图表
  ggsave("total_keywords_trend.png", plot = total_plot, width = 10, height = 6, dpi = 300)

 

  # 分别绘制单独的主题图表 ---------------------------------------------

  # 绘制 Reasons 图表
  reasons_plot <- ggplot(theme_article_counts %>% filter(theme == "Reasons"), aes(x = year, y = keyword_count, fill = as.factor(year))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = seq(min(theme_article_counts$year), max(theme_article_counts$year), by = 1)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +
    scale_fill_discrete(name = "solution_year") +  # 更改图例标题
    labs(
      title = "Keywords Count for Reasons Over the Years",
      x = "Year",
      y = "Keyword Count"
    ) +
    custom_theme

  print(reasons_plot)
  ggsave("reasons_keywords_trend.png", plot = reasons_plot, width = 10, height = 6, dpi = 300)

  # 绘制 Impacts 图表
  impacts_plot <- ggplot(theme_article_counts %>% filter(theme == "Impacts"), aes(x = year, y = keyword_count, fill = as.factor(year))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = seq(min(theme_article_counts$year), max(theme_article_counts$year), by = 1)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +
    scale_fill_discrete(name = "solution_year") +  # 更改图例标题
    labs(
      title = "Keywords Count for Impacts Over the Years",
      x = "Year",
      y = "Keyword Count"
    ) +
    custom_theme

  print(impacts_plot)
  ggsave("impacts_keywords_trend.png", plot = impacts_plot, width = 10, height = 6, dpi = 300)

  # 绘制 Solutions 图表
  solutions_plot <- ggplot(theme_article_counts %>% filter(theme == "Solutions"), aes(x = year, y = keyword_count, fill = as.factor(year))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = seq(min(theme_article_counts$year), max(theme_article_counts$year), by = 1)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +
    scale_fill_discrete(name = "solution_year") +  # 更改图例标题
    labs(
      title = "Keywords Count for Solutions Over the Years",
      x = "Year",
      y = "Keyword Count"
    ) +
    custom_theme

  print(solutions_plot)
  ggsave("solutions_keywords_trend.png", plot = solutions_plot, width = 10, height = 6, dpi = 300)

  # 绘制 Reasons 图表
reasons_plot <- ggplot(theme_article_counts %>% filter(theme == "Reasons"), aes(x = reorder(word, -keyword_count), y = keyword_count, fill = word)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +
  labs(
    title = "Keyword Counts for Reasons",
    x = "Keywords",
    y = "Keyword Count"
  ) +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 旋转 X 轴标签以避免重叠

print(reasons_plot)
ggsave("reasons_keywords_bar_chart.png", plot = reasons_plot, width = 12, height = 6, dpi = 300)

# 绘制 Impacts 图表
impacts_plot <- ggplot(theme_article_counts %>% filter(theme == "Impacts"), aes(x = reorder(word, -keyword_count), y = keyword_count, fill = word)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +
  labs(
    title = "Keyword Counts for Impacts",
    x = "Keywords",
    y = "Keyword Count"
  ) +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 旋转 X 轴标签以避免重叠

print(impacts_plot)
ggsave("impacts_keywords_bar_chart.png", plot = impacts_plot, width = 12, height = 6, dpi = 300)

# 绘制 Solutions 图表
solutions_plot <- ggplot(theme_article_counts %>% filter(theme == "Solutions"), aes(x = reorder(word, -keyword_count), y = keyword_count, fill = word)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +
  labs(
    title = "Keyword Counts for Solutions",
    x = "Keywords",
    y = "Keyword Count"
  ) +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 旋转 X 轴标签以避免重叠

print(solutions_plot)
ggsave("solutions_keywords_bar_chart.png", plot = solutions_plot, width = 12, height = 6, dpi = 300)
} else {
  stop("Error: tfidf 列未正确合并，请检查 doc_id 和 file 的匹配！")
}