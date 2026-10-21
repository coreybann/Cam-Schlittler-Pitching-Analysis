install.packages(c("baseballr", "tidyverse"))
library(baseballr)
library(tidyverse)
players <- baseballr::playerid_lookup(last_name = "Schlittler",first_name = "Cam")
print(players)
# Get Cam Schlittler's MLB ID
pitcher_id <- players$mlbam_id[1]

# Download 2026 regular season Statcast data
pitches <- baseballr::statcast_search_pitchers(
  start_date = "2026-03-01",
  end_date = "2026-09-30",
  pitcherid = pitcher_id)

# View the data
View(pitches)

nrow(pitches)
table(pitches$pitch_name)

# Calculate pitch usage percentages
pitch_usage <- pitches %>%
  filter(!is.na(pitch_name),
         pitch_name != "Pitch Out") %>%
  count(pitch_name) %>%
  mutate(usage_pct = n / sum(n) * 100)

# Create pitch usage chart
ggplot(pitch_usage,
       aes(x = reorder(pitch_name, usage_pct),
           y = usage_pct)) +
  geom_col(fill = "#003087") +
  coord_flip() +
  labs(
    title = "Cam Schlittler Pitch Usage",
    subtitle = "2026 Regular Season | New York Yankees",
    x = "Pitch Type",
    y = "Usage Percentage (%)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal()

ggsave(
  "Schlittler_2026_Pitch_Usage.png",
  width = 10,
  height = 6,
  dpi = 300
)

# Calculate average and maximum velocity
pitch_velocity <- pitches %>%
  filter(!is.na(pitch_name),
         pitch_name != "Pitch Out",
         !is.na(release_speed)) %>%
  group_by(pitch_name) %>%
  summarise(
    avg_velocity = mean(release_speed),
    max_velocity = max(release_speed),
    pitch_count = n(),
    .groups = "drop"
  )

# View the results
print(pitch_velocity)

# Create average pitch velocity chart
ggplot(pitch_velocity,
       aes(x = reorder(pitch_name, avg_velocity),
           y = avg_velocity)) +
  geom_col(fill = "#003087") +
  coord_flip() +
  geom_text(
    aes(label = paste0(round(avg_velocity, 1), " mph")),
    hjust = -0.1,
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, 110)
  ) +
  labs(
    title = "Cam Schlittler Average Pitch Velocity",
    subtitle = "2026 Regular Season | New York Yankees",
    x = "Pitch Type",
    y = "Average Velocity (MPH)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal()

# Calculate average pitch movement
pitch_movement <- pitches %>%
  filter(!is.na(pitch_name),
         pitch_name != "Pitch Out",
         !is.na(pfx_x),
         !is.na(pfx_z)) %>%
  group_by(pitch_name) %>%
  summarise(
    horizontal_break = mean(pfx_x) * 12,
    vertical_break = mean(pfx_z) * 12,
    pitch_count = n(),
    .groups = "drop"
  )

# View results
print(pitch_movement)

# Create pitch movement plot
ggplot(pitch_movement,
       aes(x = horizontal_break,
           y = vertical_break,
           color = pitch_name)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "gray60") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray60") +
  geom_point(size = 5) +
  geom_text(
    aes(label = pitch_name),
    vjust = -1,
    show.legend = FALSE
  ) +
  labs(
    title = "Cam Schlittler Pitch Movement Profile",
    subtitle = "2026 Regular Season | New York Yankees",
    x = "Horizontal Break (inches)",
    y = "Vertical Break (inches)",
    color = "Pitch Type",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  coord_equal() +
  theme_minimal()

# Calculate whiff rate by pitch type
pitch_whiffs <- pitches %>%
  filter(!is.na(pitch_name),
         pitch_name != "Pitch Out") %>%
  group_by(pitch_name) %>%
  summarise(
    total_pitches = n(),
    swings = sum(description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul",
      "foul_tip",
      "hit_into_play",
      "hit_into_play_no_out",
      "hit_into_play_score"
    )),
    whiffs = sum(description %in% c(
      "swinging_strike",
      "swinging_strike_blocked"
    )),
    .groups = "drop"
  ) %>%
  mutate(
    whiff_rate = round(whiffs / swings * 100, 1)
  )

# View results
print(pitch_whiffs)

ggplot(pitch_whiffs,
       aes(x = reorder(pitch_name, whiff_rate),
           y = whiff_rate)) +
  geom_col(fill = "#003087") +
  coord_flip() +
  geom_text(
    aes(label = paste0(whiff_rate, "%")),
    hjust = -0.2,
    size = 4
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Cam Schlittler Whiff Rate by Pitch Type",
    subtitle = "2026 Regular Season | New York Yankees",
    x = "Pitch Type",
    y = "Whiff Rate (%)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal()

# Calculate whiff rates by batter handedness
handedness_whiffs <- pitches %>%
  filter(!is.na(pitch_name),
         pitch_name != "Pitch Out",
         stand %in% c("L", "R")) %>%
  mutate(
    swing = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul",
      "foul_tip",
      "hit_into_play",
      "hit_into_play_no_out",
      "hit_into_play_score"
    ),
    whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked"
    )
  ) %>%
  group_by(pitch_name, stand) %>%
  summarise(
    total_pitches = n(),
    swings = sum(swing),
    whiffs = sum(whiff),
    .groups = "drop"
  ) %>%
  mutate(
    whiff_rate = if_else(
      swings > 0,
      round(whiffs / swings * 100, 1),
      NA_real_
    ),
    batter_side = if_else(
      stand == "L", "Left-Handed", "Right-Handed"
    )
  )

print(handedness_whiffs)
ggplot(handedness_whiffs,
       aes(x = pitch_name,
           y = whiff_rate,
           fill = batter_side)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Cam Schlittler Whiff Rate by Batter Handedness",
    subtitle = "2026 Regular Season | New York Yankees",
    x = "Pitch Type",
    y = "Whiff Rate (%)",
    fill = "Batter Handedness",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal()

# Create pitch location chart
pitches %>%
  filter(
    !is.na(plate_x),
    !is.na(plate_z),
    pitch_name %in% c(
      "4-Seam Fastball",
      "Cutter",
      "Sinker",
      "Curveball"
    )
  ) %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density_2d(
    aes(fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.7
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue"
  ) +
  facet_wrap(~pitch_name) +
  coord_fixed(
    xlim = c(-2, 2),
    ylim = c(0, 5)
  ) +
  labs(
    title = "Cam Schlittler Pitch Location Heatmaps",
    subtitle = "2026 Regular Season | New York Yankees",
    x = "Horizontal Location (feet)",
    y = "Vertical Location (feet)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal()

# Improved pitch location heatmaps
pitches %>%
  filter(
    !is.na(plate_x),
    !is.na(plate_z),
    pitch_name %in% c(
      "4-Seam Fastball",
      "Cutter",
      "Sinker",
      "Curveball"
    )
  ) %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density_2d(
    aes(fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.7
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue"
  ) +
  annotate(
    "rect",
    xmin = -0.83,
    xmax = 0.83,
    ymin = 1.5,
    ymax = 3.5,
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) +
  facet_wrap(~pitch_name) +
  coord_fixed(
    xlim = c(-2, 2),
    ylim = c(0, 5)
  ) +
  labs(
    title = "Cam Schlittler Pitch Location Heatmaps",
    subtitle = "2026 Regular Season | New York Yankees",
    x = "Horizontal Location (feet)",
    y = "Vertical Location (feet)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal()

ggsave(
  "Schlittler_2026_Pitch_Locations.png",
  width = 12,
  height = 8,
  dpi = 300
)

# Analyze pitch selection by count
count_usage <- pitches %>%
  filter(
    !is.na(pitch_name),
    pitch_name != "Pitch Out",
    !is.na(balls),
    !is.na(strikes)
  ) %>%
  mutate(
    count_situation = case_when(
      balls > strikes ~ "Behind",
      balls < strikes ~ "Ahead",
      TRUE ~ "Even"
    )
  ) %>%
  group_by(count_situation, pitch_name) %>%
  summarise(
    pitches_thrown = n(),
    .groups = "drop"
  ) %>%
  group_by(count_situation) %>%
  mutate(
    usage_pct = pitches_thrown /
      sum(pitches_thrown) * 100
  ) %>%
  ungroup()

print(count_usage)


ggplot(count_usage,
       aes(x = count_situation,
           y = usage_pct,
           fill = pitch_name)) +
  geom_col() +
  labs(
    title = "Cam Schlittler Pitch Selection by Count",
    subtitle = "2026 Regular Season | New York Yankees",
    x = "Count Situation",
    y = "Pitch Usage (%)",
    fill = "Pitch Type",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1)
  ) +
  theme_minimal()

# Check all Statcast pitch descriptions
pitches %>%
  count(description, sort = TRUE) %>%
  print(n = Inf)

ggsave(
  "Schlittler_2026_Count_Usage.png",
  width = 10,
  height = 7,
  dpi = 300
)

# Check all Statcast pitch descriptions
pitches %>%
  count(description, sort = TRUE) %>%
  print(n = Inf)

# Updated whiff rate calculations
pitch_whiffs <- pitches %>%
  filter(
    !is.na(pitch_name),
    pitch_name != "Pitch Out"
  ) %>%
  mutate(
    swing = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul",
      "foul_tip",
      "foul_bunt",
      "missed_bunt",
      "hit_into_play",
      "hit_into_play_no_out",
      "hit_into_play_score"
    ),
    whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "missed_bunt"
    )
  ) %>%
  group_by(pitch_name) %>%
  summarise(
    total_pitches = n(),
    swings = sum(swing),
    whiffs = sum(whiff),
    .groups = "drop"
  ) %>%
  mutate(
    whiff_rate = if_else(
      swings > 0,
      round(whiffs / swings * 100, 1),
      NA_real_
    )
  )

print(pitch_whiffs)
# Updated whiff rates by batter handedness
handedness_whiffs <- pitches %>%
  filter(
    !is.na(pitch_name),
    pitch_name != "Pitch Out",
    stand %in% c("L", "R")
  ) %>%
  mutate(
    swing = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul",
      "foul_tip",
      "foul_bunt",
      "missed_bunt",
      "hit_into_play",
      "hit_into_play_no_out",
      "hit_into_play_score"
    ),
    whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "missed_bunt"
    )
  ) %>%
  group_by(pitch_name, stand) %>%
  summarise(
    total_pitches = n(),
    swings = sum(swing),
    whiffs = sum(whiff),
    .groups = "drop"
  ) %>%
  mutate(
    whiff_rate = if_else(
      swings > 0,
      round(whiffs / swings * 100, 1),
      NA_real_
    ),
    batter_side = if_else(
      stand == "L",
      "Left-Handed",
      "Right-Handed"
    )
  )

print(handedness_whiffs)

# Display updated overall whiff rates
print(pitch_whiffs)

# Check total whiffs
sum(pitch_whiffs$whiffs)

# Check total swings
sum(pitch_whiffs$swings)

# Save Statcast dataset
saveRDS(
  pitches,
  "Schlittler_2026_Statcast_Data.rds"
)

# Save average velocity chart
velocity_chart <- ggplot(pitch_velocity,
                         aes(x = reorder(pitch_name, avg_velocity),
                             y = avg_velocity)) +
  geom_col(fill = "#003087") +
  coord_flip() +
  labs(
    title = "Cam Schlittler Average Pitch Velocity",
    x = "Pitch Type",
    y = "Average Velocity (MPH)"
  ) +
  theme_minimal()

ggsave(
  "Schlittler_2026_Velocity.png",
  plot = velocity_chart,
  width = 10,
  height = 7,
  dpi = 300
)

# Save pitch movement chart
ggplot(pitch_movement,
       aes(x = horizontal_break,
           y = vertical_break,
           color = pitch_name)) +
  geom_point(size = 5) +
  geom_text(aes(label = pitch_name),
            vjust = -1,
            show.legend = FALSE) +
  labs(
    title = "Cam Schlittler Pitch Movement",
    x = "Horizontal Break (inches)",
    y = "Vertical Break (inches)"
  ) +
  coord_equal() +
  theme_minimal()

ggsave("Schlittler_2026_Movement.png",
       width = 10, height = 7, dpi = 300)
# Save whiff rate chart
whiff_chart <- ggplot(pitch_whiffs,
                      aes(x = reorder(pitch_name, whiff_rate),
                          y = whiff_rate)) +
  geom_col(fill = "#003087") +
  coord_flip() +
  geom_text(
    aes(label = paste0(whiff_rate, "%")),
    hjust = -0.2,
    size = 4
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Cam Schlittler Whiff Rate by Pitch Type",
    subtitle = "2026 Regular Season | New York Yankees",
    x = "Pitch Type",
    y = "Whiff Rate (%)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal()

ggsave(
  "Schlittler_2026_Whiff_Rate.png",
  plot = whiff_chart,
  width = 10,
  height = 7,
  dpi = 300
)
# Save whiff rate by batter handedness
handedness_chart <- ggplot(handedness_whiffs,
                           aes(x = pitch_name,
                               y = whiff_rate,
                               fill = batter_side)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Cam Schlittler Whiff Rate by Batter Handedness",
    subtitle = "2026 Regular Season | New York Yankees",
    x = "Pitch Type",
    y = "Whiff Rate (%)",
    fill = "Batter Handedness",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal()

ggsave(
  "Schlittler_2026_Handedness.png",
  plot = handedness_chart,
  width = 10,
  height = 7,
  dpi = 300
)

# Save whiff rate by batter handedness
handedness_chart <- ggplot(handedness_whiffs,
                           aes(x = pitch_name,
                               y = whiff_rate,
                               fill = batter_side)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Cam Schlittler Whiff Rate by Batter Handedness",
    subtitle = "2026 Regular Season | New York Yankees",
    x = "Pitch Type",
    y = "Whiff Rate (%)",
    fill = "Batter Handedness",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal()

ggsave(
  "Schlittler_2026_Handedness.png",
  plot = handedness_chart,
  width = 10,
  height = 7,
  dpi = 300
)