library(rvest)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
# Base URL
base_url <- "https://www.tbsnews.net/"

# Read homepage
homepage <- read_html(base_url)

# Extract <a> tags
a_nodes <- homepage %>% html_nodes("div.card-section a")

# Get href and aria-label
links_data <- data.frame(
  href = a_nodes %>% html_attr("href"),
  title = a_nodes %>% html_text(trim = TRUE), 
  stringsAsFactors = FALSE
)

# Filter: keep only valid hrefs and aria-labels (non-NA)
links_data <- links_data %>%
  filter(!is.na(href), !is.na(title)) %>%
  mutate(
    href = ifelse(grepl("^http", href), href, paste0(base_url, href))
  ) %>%
  distinct()

# Save to CSV
write_csv(links_data, "tbsnews_links_with_labels.csv")

cat("Saved", nrow(links_data), "links with aria-labels to tbsnews_links_with_labels.csv\n")
