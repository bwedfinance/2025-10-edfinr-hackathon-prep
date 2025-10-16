# load packages ------
library(tidyverse)
library(edfinr)

# download edfinr data ------
dist_us_full_sy12_to_sy22 <- get_finance_data(
  yr = "2012:2022",
  dataset_type = "full"
)

# if using positron and you have an anthropic api key, try out databot!
# start your prompt with: 
# Please use the `dist_us_full_sy12_to_sy22` dataframe to...
