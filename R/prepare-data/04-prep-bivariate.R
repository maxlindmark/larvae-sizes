# Load libraries
library(tidyverse)
# library(tidylog)
# library(ggsidekick)
# library(ggstats)
# library(tinyVAST)
# library(fmesher)
# library(patchwork)
# library(viridis)
# theme_set(theme_sleek())

home <- here::here()

# Read data
b <- readr::read_csv(paste0(home, "/data/clean/larval_density.csv")) |>
  drop_na(no_m2)

# b |> 
#   summarise(no_m2 = sum(no_m2), .by = haul_id) |> 
#   filter(no_m2 == 0)
#length(unique(b$haul_id))

l <- readr::read_csv(paste0(home, "/data/clean/larval_size.csv"))
#length(unique(l$haul_id))

# Prepare for joining
l <- l |>
  rename(response = length_mm) |> 
  mutate(type = "length")

b <- b |>
  rename(response = no_m2) |> 
  mutate(type = "density")

d <- bind_rows(b,l) |>
  dplyr::select(-life_stage, -number, -temp_obs, -national_haul_id)

# Scale variables
d <- d |> 
  drop_na(chl) |> 
  mutate(
    type = as.factor(type),
    year_f = as.factor(year),
    species_f = as.factor(species),
    year_ct = year - median(year)
  ) |> 
  mutate(across(
    .cols = c("temp", "chl", "depth", "yday"),
    .fns = scale,
    .names = "{.col}_sc"
  )) |> 
  mutate(across(
    .cols = c("temp_sc", "chl_sc", "depth_sc", "yday_sc"),
    .fns = as.numeric
    )) |> 
  mutate(year = as.integer(year)) |> 
  as.data.frame()

# Read prediction grid
pred_grid <- readr::read_csv(paste0(home, "/data/clean/pred_grid.csv")) |>
  drop_na(temp) |>
  drop_na(chl) |>
  filter(year %in% unique(d$year)) |>
  mutate(
    temp_sc = (temp - mean(d$temp, na.rm = TRUE)) / sd(d$temp, na.rm = TRUE),
    depth_sc = (depth - mean(d$temp, na.rm = TRUE)) / sd(d$temp, na.rm = TRUE),
    chl_sc = (chl - mean(d$chl, na.rm = TRUE)) / sd(d$chl, na.rm = TRUE),
    year_f = as.factor(year),
    year_sc = 0,
    yday_sc = 0
  ) |>
  # FIXME
  mutate(keep = ifelse(lon < 10 & lat < 57.15, "N", "Y")) |>
  filter(keep == "Y") |>
  dplyr::select(-keep)

pred_grid_all <- bind_rows(pred_grid |> mutate(type = "length"),
                           pred_grid |> mutate(type = "density")) |> 
  mutate(type = as.factor(type)) |> 
  as.data.frame()


write_csv(d, paste0(home, "/data/clean/bivariate_size_density.csv"))
write_csv(pred_grid_all, paste0(home, "/data/clean/bivariate_pred_grid_all.csv"))
