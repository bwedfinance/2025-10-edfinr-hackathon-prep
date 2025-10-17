# load packages ------
library(tidyverse)
library(scales)
library(viridis)
library(edfinr)

# download edfinr data ------

# running this function will pull the "skinny" df for all districts in sy22
dist_us_skinny_sy22 <- get_finance_data()

# setting the `dataset_type`` parameter to "full" will give you more expenditure data
dist_us_full_sy22 <- get_finance_data(dataset_type = "full")

# the `yr` parameter can give you data from a single year or multiple years
dist_us_skinny_sy12_to_sy22 <- get_finance_data(yr = "2012:2022")

# exploring data -----------

names(dist_us_full_sy22)

# subsetting data ----------

# use `filter()` will help you focus on a particular state
ky_full_sy22 <- dist_us_full_sy22 |> 
  filter(state == "KY") |> 
  # use `mutate` to convert total expenditures to per-puil
  mutate(
    exp_sal_pp = exp_emp_salary / enroll,
    exp_bene_pp = exp_emp_bene / enroll,
    exp_emp_comp_pp = exp_sal_pp + exp_bene_pp
   ) |> 
  select(
    ncesid, dist_name, enroll, rev_total_pp, rev_state_pp, rev_local_pp,
    mhi, mpv, ba_plus_pct, stpov_pct, urbanicity, exp_sal_pp, exp_bene_pp,
    exp_emp_comp_pp

  )

# you can also filter for a group of states 

ky_tn_in_oh <- dist_us_skinny_sy22 |> 
  filter(state %in% c("KY", "TN", "IN", "OH"))


# plotting data ------------

# start with a simple plot
ggplot() +
  geom_point(
    data = ky_full_sy22,
    aes(
      x = ba_plus_pct, 
      y = exp_emp_comp_pp,
      size = enroll,
      color = stpov_pct
    )
  ) 

# we can take that same plot and make it easier to interpret 
ggplot() +
  geom_point(
    data = ky_full_sy22,
    # adjusting `alpha` will make points more transparent 1 = totally opaque
    alpha = .9,
    aes(
      x = ba_plus_pct, 
      y = exp_emp_comp_pp,
      size = enroll,
      color = stpov_pct
    )
  ) +
  # this will allow points to vary by area instead of diameter - easier to interpret
  scale_size_area(
    max_size = 10, 
    # this gives us better looking labels
    labels = label_comma()
  ) + 
  # the viridis scale is good, especially for folks w/ color blindness
  # adding nice labels helps, too
  scale_color_viridis(labels = label_percent()) +
  # nice axis labels can help make it pretty
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  # the bw theme will give a clean background for your plot
  theme_bw() +
  # finally, let's add nice labels to plot elements
  labs(
    x = "% of adults with a BA+",
    y = "District spending on employee salary + benefits per-pupil",
    color = "Student poverty %",
    size = "Enrollment",
    title = "Kentucky District Employee Compensation Per-Pupil vs. Percent of Adults w/ BA+",
    subtitle = "School Year 2021-22",
    caption = "Data source: edfinr"
  )

# save plot -----
ggsave(
  "ky_ba_pct_vs_emp_comp.png",
  units = "in",
  width = 8,
  height = 6
)


# state comparison plots -------

# one varaible across several states
ggplot() +
  geom_density(
    data = ky_tn_in_oh,
    alpha = .6,
    aes(
      x = rev_state_pp,
      # use fill to differentiate between different states
      fill = state
    )
  ) +
  theme_bw()

# multiple variables across several states
ggplot() +
  geom_point(
    data = ky_tn_in_oh,
    alpha = .6,
    aes(
      x = ba_plus_pct,
      y = rev_local_pp,
      size = enroll,
      color = stpov_pct
    )
  ) +
  scale_color_viridis() +
  # use `facet_wrap()` to create small multiples by state
  facet_wrap(~state) +
  # see trends for x and y vars with `geom_smooth()`
  geom_smooth(
    data = ky_tn_in_oh,
    color = "orange",
    aes(
      x = ba_plus_pct,
      y = rev_local_pp
    )
  ) +
  theme_bw()
