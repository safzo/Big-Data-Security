#Dataset 2
library(dplyr)
breach <- read.csv("accounting_finance_economics_data_breaches_dataset.csv")

name_table <- table(breach$name)

# Loop through each index in the table
for (i in seq_along(name_table)) {
  
  # Check if the count of the current name is greater than 1
  if (name_table[i] > 3) {
    
    # If the count is greater than 1, print the name
    print(names(name_table)[i])
  }
}
