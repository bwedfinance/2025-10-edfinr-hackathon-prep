# load packages ------
library(tidyverse)
library(scales)
library(viridis)
library(edfinr)

# download edfinr data ------
dist_us_full_sy12_to_sy22 <- get_finance_data(
  yr = "2012:2022",
  dataset_type = "full"
)

# select state data ---------
ky_sy12_to_sy22 <- dist_us_full_sy12_to_sy22 |> 
  filter(state == "KY") |> 
  mutate(exp_bene_pp = exp_emp_bene / enroll)

# plot bene spending pp over time by lea ----

# basic plot
ggplot() +
  geom_line(
    data = ky_sy12_to_sy22,
    aes(
      x = year,
      y = exp_bene_pp,
      group = ncesid
    )
  )

# tidy plot
ggplot() +
  geom_line(
    data = ky_sy12_to_sy22,
    alpha = .6,
    aes(
      x = year,
      y = exp_bene_pp,
      group = ncesid
    )
  ) +
  facet_wrap(~urbanicity) +
  scale_y_continuous(labels = label_dollar()) +
  theme_bw() +
  labs(
    x = "Year",
    y = " Per-Pupil Expdenditures on Employee Benefits",
    title = "Kentucky District Spending on Employee Benefits Incresaed from SY12 to SY22"
  )

# save plot -----
ggsave(
  "ky_comp_long.png",
  units = "in",
  width = 8,
  height = 6
)
