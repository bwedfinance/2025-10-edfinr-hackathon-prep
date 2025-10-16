# this script will require some addtional packages
# Install once if needed:
pkgs <- c("gganimate", "gifski", "png")
to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(to_install)) install.packages(to_install)

# load packages ------
library(tidyverse)
library(scales)
library(viridis)
library(edfinr)
library(gganimate)

bw_colors <- c("#6D1E4A", # 1 plum
                "#007786", # 2 teal
                "#0D525A", # 3 dark green
                "#212B46", # 4 navy
                "#5A6675", # 5 grey
                "#F0DEC1") # 6 cream

# download edfinr data ------
dist_us_skinny_sy12_to_sy22 <- get_finance_data(
  yr = "2012:2022",
  dataset_type = "skinny"
)

# clean data  ---------
# filter for ky only
ky_sy12_to_sy22 <- dist_us_skinny_sy12_to_sy22 |> 
  filter(state == "KY") 

# create long version of state/local revenue
ky_rev_source <- ky_sy12_to_sy22 |> 
  select(year, dist_name, rev_local_pp, rev_state_pp, rev_fed_pp) |> 
  pivot_longer(
    cols = rev_local_pp:rev_fed_pp,
    names_to = "rev_source",
    values_to = "amt"
  ) |> 
  mutate(
    rev_source = str_replace_all(
      rev_source, 
      "rev_fed_pp",
      "Federal"
    )
  ) |> 
  mutate(
    rev_source = str_replace_all(
      rev_source, 
      "rev_state_pp",
      "State"
    )
  ) |> 
  mutate(
    rev_source = str_replace_all(
      rev_source, 
      "rev_local_pp",
      "Local"
    )
  )

# create plot to animate -------------
# save plot as variable "p"
p <- ggplot() +
  # this geom creates a smoother distribtion than histograms
  geom_density(
    data = ky_rev_source, 
    # `alpha` of .6 is fairly transparent
    alpha = .6,
    # we'll set the x axis to the per-pupil amounts
    # and the color fill to revenue source
    aes(
      x = amt,
      fill = rev_source
    )
  ) +
  # this helps cut off a few extreme outlier districts
  scale_x_continuous(
    limits = c(0, 15000),
    labels = label_dollar()
  ) +
  # this gets rid of the gap between the plot and the x-axis
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  # we'll get fancy and use bw branded colors
  scale_fill_manual(values = c(bw_colors[6], bw_colors[1], bw_colors[2])) +
  labs(
    title = "Kentucky District Revenue per Pupil by Source, SY12-SY22",
    # this will update the subtitle as the gif animates to show each year
    subtitle = "Year: {closest_state}",
    fill = "Revenue Source",
    x = "Revenue per Pupil",
    y = "Density"
  ) +
  theme_bw(base_size = 13)

# set animation specification -------
# this code sets the base of the animation for the gif
anim_spec <- p +
  # we want to animate the gif by year
  transition_manual(year) +
  labs(subtitle = "Year: {current_frame}")

# render gif
animated_plot <- animate(
  anim_spec,
  nframes = 10,    # total frames
  fps = 1,         # frames per second
  width = 900, height = 600, res = 120,   # size & resolution
  renderer = gifski_renderer(loop = TRUE)
)

# save gif ----------
anim_save("ky_revenue_distribution.gif", animation = animated_plot)
