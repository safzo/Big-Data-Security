#Milestone 3: Part 1
#Observations on Data Patterns

breach <- read.csv("breach_dataset.csv")

# Remove rows with missing values in "breach_size" column using for loop
for (i in 1:nrow(breach)) {
  if (is.na(breach$breach_size[i])) {
    breach <- breach[-i,]
  }
}

# Wrote cleaned data to a new CSV file
write.csv(breach, "breach_clean.csv", row.names = FALSE)

# Frequency of breaches
library(dplyr)

breach_frequency <- breach %>% 
  group_by(event_year, name) %>% 
  summarise(breach_count = n()) %>%
  ungroup()

# Severity for JPMorgan Chase
breach_severity_Chase <- breach %>%
  filter(ticker == "JPM") %>%
  group_by(breach_type) %>%
  mutate(records_affected = if_else(breach_size < 1000, "Low",
                                    if_else(breach_size >= 1000 & breach_size < 10000, "Medium", "High")),
         severity = case_when(breach_type == "CARD" ~ "Low",
                              breach_type == "HACK" & records_affected == "Low" ~ "Low",
                              breach_type == "HACK" & records_affected == "Medium" ~ "Medium",
                              breach_type == "HACK" & records_affected == "High" ~ "High",
                              breach_type == "INSD" & records_affected == "Low" ~ "Low",
                              breach_type == "INSD" & records_affected == "Medium" ~ "Medium",
                              breach_type == "INSD" & records_affected == "High" ~ "High",
                              breach_type == "PHYS" & records_affected == "Low" ~ "Low",
                              breach_type == "PHYS" & records_affected == "Medium" ~ "Medium",
                              breach_type == "PHYS" & records_affected == "High" ~ "High",
                              breach_type == "PORT" & records_affected == "Low" ~ "Low",
                              breach_type == "PORT" & records_affected == "Medium" ~ "Medium",
                              breach_type == "PORT" & records_affected == "High" ~ "High",
                              breach_type == "STAT" & records_affected == "Low" ~ "Low",
                              breach_type == "STAT" & records_affected == "Medium" ~ "Medium",
                              breach_type == "STAT" & records_affected == "High" ~ "High",
                              breach_type == "DISC" & records_affected == "Low" ~ "Low",
                              breach_type == "DISC" & records_affected == "Medium" ~ "Medium",
                              breach_type == "DISC" & records_affected == "High" ~ "High",
                              TRUE ~ "Unknown"))

write.csv(breach_severity_Chase, "breach_severity_Chase.csv", row.names = FALSE)

# Severity for competitors
breach_severity_competitor <- breach %>%
  filter(ticker %in% c("BAC", "C", "WFC", "COF", "PNC", "MS", "AXP")) %>%
  group_by(breach_type) %>%
  mutate(records_affected = if_else(breach_size < 1000, "Low",
                                    if_else(breach_size >= 1000 & breach_size < 10000, "Medium", "High")),
         severity = case_when(breach_type == "CARD" ~ "Low",
                              breach_type == "HACK" & records_affected == "Low" ~ "Low",
                              breach_type == "HACK" & records_affected == "Medium" ~ "Medium",
                              breach_type == "HACK" & records_affected == "High" ~ "High",
                              breach_type == "INSD" & records_affected == "Low" ~ "Low",
                              breach_type == "INSD" & records_affected == "Medium" ~ "Medium",
                              breach_type == "INSD" & records_affected == "High" ~ "High",
                              breach_type == "PHYS" & records_affected == "Low" ~ "Low",
                              breach_type == "PHYS" & records_affected == "Medium" ~ "Medium",
                              breach_type == "PHYS" & records_affected == "High" ~ "High",
                              breach_type == "PORT" & records_affected == "Low" ~ "Low",
                              breach_type == "PORT" & records_affected == "Medium" ~ "Medium",
                              breach_type == "PORT" & records_affected == "High" ~ "High",
                              breach_type == "STAT" & records_affected == "Low" ~ "Low",
                              breach_type == "STAT" & records_affected == "Medium" ~ "Medium",
                              breach_type == "STAT" & records_affected == "High" ~ "High",
                              breach_type == "DISC" & records_affected == "Low" ~ "Low",
                              breach_type == "DISC" & records_affected == "Medium" ~ "Medium",
                              breach_type == "DISC" & records_affected == "High" ~ "High",
                              TRUE ~ "Unknown"))

# Loaded required packages
library(stringr)

# Defined severity scores for each category
severity_scores <- c("Unknown" = 0, "Low" = 1, "Medium" = 2, "High" = 3)

# Defined function to calculate severity score for a single breach
calc_severity_score <- function(breach_size, breach_type) {
  # Defined rules for assigning severity scores to each category
  size_scores <- c("Unknown" = 0, "Low" = 1, "Medium" = 2, "High" = 3)
  type_scores <- c("CARD" = 1, "HACK" = 3, "INSD" = 3, "PHYS" = 2, "PORT" = 2, "STAT" = 2, "DISC" = 1, "UNKN" = 0)
  # Calculated score based on breach size and type
  size_score <- size_scores[cut(breach_size, breaks = c(0, 1000, 10000, Inf), labels = c("Low", "Medium", "High"))]
  type_score <- type_scores[breach_type]
  return(size_score + type_score)
}

# Filter data to only include JPM
jpm_breach <- filter(breach, ticker == "JPM")

# Group data by year and calculate total severity score for each year
jpm_severity <- jpm_breach %>% 
  group_by(event_year) %>% 
  summarise(total_severity_score = sum(severity_score))

# Plot severity score by year
ggplot(jpm_severity, aes(x = event_year, y = total_severity_score)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Severity Score of JPM by Year",
       x = "Year", y = "Total Severity Score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Filter data to only include Chase's competitors
competitors <- c("BAC", "C", "WFC", "COF", "PNC", "MS", "AXP")

# Filter data by selected tickers
competitor_breach <- breach %>%
  filter(ticker %in% competitors)

# Group data by year and calculate total severity score for each year and ticker
competitor_severity <- competitor_breach %>% 
  group_by(event_year, ticker) %>% 
  summarise(total_severity_score = sum(severity_score)) %>%
  # Add a new column with the label for each ticker
  mutate(label = case_when(
    ticker == "BAC" ~ "Bank of America",
    ticker == "C" ~ "Citigroup",
    ticker == "WFC" ~ "Wells Fargo",
    ticker == "COF" ~ "Capital One",
    ticker == "PNC" ~ "PNC Financial",
    ticker == "MS" ~ "Morgan Stanley",
    ticker == "AXP" ~ "American Express"
  ))

ggplot(competitor_severity, aes(x = event_year, y = total_severity_score)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = label), size = 3, angle = 45, position = position_dodge(width = 1)) +
  labs(title = "Severity Score by Year and Ticker",
       x = "Year", y = "Severity Score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

all <- c("JPM", "BAC", "C", "WFC", "COF", "PNC", "MS", "AXP")

# Filter data by selected tickers
all_breach <- breach %>%
  filter(ticker %in% all)

state_heat <- all_breach %>% 
  group_by(event_state, ticker) %>% 
  summarise(total_severity_score = sum(severity_score)) %>%
  # Add a new column with the label for each ticker
  mutate(label = case_when(
    ticker == "JPM" ~ "JP Morgan Chase & Co.",
    ticker == "BAC" ~ "Bank of America",
    ticker == "C" ~ "Citigroup",
    ticker == "WFC" ~ "Wells Fargo",
    ticker == "COF" ~ "Capital One",
    ticker == "PNC" ~ "PNC Financial",
    ticker == "MS" ~ "Morgan Stanley",
    ticker == "AXP" ~ "American Express"
  ))

library(ggplot2)

ggplot(state_heat, aes(x = event_state, y = label, fill = total_severity_score)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Severity Score by Event State for Selected Companies",
       x = "Event State", y = "Company Name", fill = "Total Severity Score")


