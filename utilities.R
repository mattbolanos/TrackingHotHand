# --- Utilities --- #

# ---------------- #
# --- Packages --- #
# ---------------- #

library(reactable)
library(reactablefmtr)
library(nbastatR)
library(hexbin)
library(cowplot)
library(png)
library(scales)
library(extrafont)
library(prismatic)
library(grid)
library(ggtext)
library(htmltools)
library(tippy)

# Set margin def
margin <- ggplot2::margin



# ------------------ #
# --- Helper DFs --- #
# ------------------ #

# Load Court dimensions from (h/t Owen Phillips)
devtools::source_url("https://github.com/Henryjean/NBA-Court/blob/main/CourtDimensions.R?raw=TRUE")

# Adjust sourced court x-y dataframe
court_points <- court_points %>%
  mutate(
    dash = ifelse(dash, "dashed", "solid")
  )

# Court color way
light_court_theme <- list(
  light = list(
    lines = '#999999',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 1,
    hex_border_color = "#000000"
  )
)

# Columns for input
season_values <- paste0(seq(2015, 2020, 1), "-", seq(16, 21, 1))

shooter_values <- dbGetQuery(con, "SELECT DISTINCT shooter FROM streak_shots_nba") %>% 
  pull(shooter)

## Start up data for initial plot and tables
klay_plot <- read_csv("./start_up_data/plot_data.csv")
klay_table <- read_csv("./start_up_data/table_data.csv")
dist_table <- read_csv("./start_up_data/def_dist_table.csv") 

# Player headshots
players <- read_csv("./start_up_data/player_headshots.csv")

# Player base eFG%s for tracking data sample
base_efgs <- read_csv("./start_up_data/base_tracking_efg.csv")

# League averages
league_averages <- bind_rows(
  # By year
  dbGetQuery(
    con,
    "
    SELECT 
      CAST(SUM(
      (CASE WHEN type_shot = '3PT Field Goal' AND is_shot_made THEN 1 ELSE 0 END * .5) + 
      CASE WHEN is_shot_made THEN 1 ELSE 0 END) AS decimal(10,2)) / 
      CAST(SUM(CASE WHEN is_shot_attempted THEN 1 ELSE 0 END) AS decimal(10,2)) as league_pct,
      ROUND(
        CAST(
          SUM(
            CASE WHEN type_shot = '3PT Field Goal' AND is_shot_made THEN 3
            WHEN type_shot <> '3PT Field Goal' AND is_shot_made THEN 2 ELSE 0 END) AS decimal(10,2)
        ) / CAST(SUM(CASE WHEN is_shot_attempted THEN 1 ELSE 0 END) AS decimal(10,2)), 2) as league_pps,
      zone_range, 
      name_zone,
      slug_season,
      streak_col
    FROM 
      streak_shots_nba
    WHERE
      name_zone <> 'Back Court' AND 
      streak_col IS NOT NULL
    GROUP BY
      name_zone, zone_range, slug_season, streak_col
    "
  ),
  # 2015-2021
  dbGetQuery(
    con,
    "
    SELECT 
      CAST(SUM(
      (CASE WHEN type_shot = '3PT Field Goal' AND is_shot_made THEN 1 ELSE 0 END * .5) + 
      CASE WHEN is_shot_made THEN 1 ELSE 0 END) AS decimal(10,2)) / 
      CAST(SUM(CASE WHEN is_shot_attempted THEN 1 ELSE 0 END) AS decimal(10,2)) as league_pct,
      ROUND(
        CAST(
          SUM(
            CASE WHEN type_shot = '3PT Field Goal' AND is_shot_made THEN 3
            WHEN type_shot <> '3PT Field Goal' AND is_shot_made THEN 2 ELSE 0 END) AS decimal(10,2)
        ) / CAST(SUM(CASE WHEN is_shot_attempted THEN 1 ELSE 0 END) AS decimal(10,2)), 2) as league_pps,
      zone_range, 
      name_zone,
      streak_col
    FROM 
      streak_shots_nba 
    WHERE
      name_zone <> 'Back Court' AND 
      streak_col IS NOT NULL
    GROUP BY
      name_zone, zone_range, streak_col
    "
  ) %>% 
    mutate(slug_season = "2015-2021")
) 
