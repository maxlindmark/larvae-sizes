library(RColorBrewer)
library(tidyverse)
library(patchwork)
library(tidylog)
library(brms)
library(viridis)
library(modelr)
library(tidybayes)
library(ggsidekick)
theme_set(theme_sleek())

home <- here::here()

kat <- readxl::read_excel(paste0(home, "/data/Calanus abundances Skagerrak Kattegatt.xlsx")) |> 
  dplyr::select(date1, Kattegatt) |> 
  rename(date = date1,
         density = Kattegatt) |> 
  mutate(area = "Kattegatt") |> 
  drop_na(density) |> 
  mutate(year = year(date),
         month = month(date),
         yday = yday(date),
         day = day(date),
         year_sc = as.numeric(scale(year)))

skag <- readxl::read_excel(paste0(home, "/data/Calanus abundances Skagerrak Kattegatt.xlsx")) |> 
  dplyr::select(date2, Skagerrak) |> 
  rename(date = date2,
         density = Skagerrak) |> 
  mutate(area = "Skagerrak") |> 
  mutate(year = year(date),
         month = month(date),
         yday = yday(date),
         day = day(date),
         yearf = as.factor(year),
         year_sc = as.numeric(scale(year)))
  
cal <- bind_rows(kat, skag)

# Plot all data
ggplot(cal, aes(date, density)) + 
  geom_line() + 
  facet_wrap(~area, ncol = 1, scales = "free")

# Seasonal average
cal |> 
  summarise(density = mean(density, na.rm = TRUE), .by = c(yday, area)) |> 
  ggplot(aes(yday, density)) + 
  geom_line() + 
  facet_wrap(~area, ncol = 1, scales = "free")

cal |> 
  summarise(density = mean(density, na.rm = TRUE), .by = c(month, area)) |> 
  ggplot(aes(month, density)) + 
  geom_line() + 
  facet_wrap(~area, ncol = 1, scales = "free")

cal |> 
  summarise(density = mean(density, na.rm = TRUE), .by = c(month, area)) |> 
  arrange(month, area) |> as.data.frame()

cal |> 
  summarise(n = n(), .by = c(yday, area)) |> 
  arrange(n) |> as.data.frame()

cal |> 
  summarise(n = n(), .by = c(yday, area)) |> 
  ggplot(aes(yday, n)) + 
  geom_col() + 
  facet_wrap(~area, ncol = 1, scales = "free")

cal |> 
  summarise(n = n(), .by = c(month, area)) |> 
  ggplot(aes(month, n)) + 
  geom_col() + 
  facet_wrap(~area, ncol = 1, scales = "free")

# Average over time
cal |> 
  summarise(density = mean(density, na.rm = TRUE), .by = c(year, area)) |> 
  ggplot(aes(year, density)) + 
  geom_line() + 
  facet_wrap(~area, ncol = 1, scales = "free")

# Fit basic model

hist(cal$density)
hist(log(cal$density))

m1 <- brm(density ~ s(year_sc),
          family = lognormal(),
          data = skag,
          control = list(adapt_delta = 0.95))

# add seasonal effects? start with month
m2 <- brm(density ~ s(year_sc) + s(month, bs = "cc"),
          family = lognormal(),
          knots = list(yday = c(0.5, 11.5)),
          control = list(adapt_delta = 0.995),
          data = skag)

# seasonal with yday instead
m3 <- brm(density ~ s(year_sc) + s(yday, bs = "cc"),
          family = lognormal(),
          knots = list(yday = c(0.5, 364.5)),
          control = list(adapt_delta = 0.995),
          data = skag)

## allow seasonality to change over time (using month-model)
m4 <- brm(density ~ s(year_sc) + s(month, bs = "cc", by = year),
          family = lognormal(),
          knots = list(month = c(0.5, 11.5)),
          control = list(adapt_delta = 0.95,
                         max_treedepth = 12),
          data = skag)

## allow seasonality to change over time (using yday-model)
m5 <- brm(density ~ s(year_sc) + s(yday, bs = "cc", by = year),
          family = lognormal(),
          knots = list(yday = c(0.5, 364.5)),
          control = list(adapt_delta = 0.95,
                         max_treedepth = 12),
          data = skag)


conditional_effects(m5)

loo(m1, m2, m3, m4, m5)

## Add e-pred here... on new data maybe add mean or raw data also
## 2 smooth sunrise plot
conditional_effects(m2)

hist(skag$density)

skag |> 
  data_grid(year = seq_range(year, n = 50),
            yday = seq_range(yday, n = 50)#,
            #month = seq_range(month, n = 50)
            ) |>
  mutate(year_sc = as.numeric(scale(year))) |> 
  add_epred_draws(m5) |> 
  ungroup() |> 
  summarise(median = median(.epred), .by = c(yday, year)) |> 
  ggplot() + 
  geom_raster(aes(yday, year, fill = median)) + 
  scale_fill_viridis(option = "mako") + 
  coord_cartesian(expand = 0)

# Temporal trends
skag_sum <- skag |> 
  summarise(density = median(density), .by = year)

p1 <- skag |> 
  data_grid(year = seq_range(year, n = 50)) |>
  mutate(year_sc = as.numeric(scale(year)),
         yday = 31) |> 
  add_epred_draws(m5) |> 
  ggplot() + 
  #geom_point(data = skag_sum, aes(year, density)) +
  stat_lineribbon(aes(year, .epred), alpha = 0.25, .width = c(0.9), size = 0.75,
                  fill = "steelblue") +
  stat_lineribbon(aes(year, .epred), alpha = 0.25, .width = c(0), size = 0.75,
                  fill = "steelblue")

p1




## Add in size index data
index <- read_csv(paste0(home, "/output/index.csv"))

index_dat <- index |> 
  filter(type == "weighted") |> 
  ungroup() |> 
  mutate(est2 = est - exp(mean(log(est))),
         .by = species) |> 
  mutate(year_sc = as.numeric(scale(year)),
         species_f = as.factor(species))

index_dat |> 
  ggplot(aes(year, est2, color = species)) +
  geom_point(alpha = 0.8) +
  theme(strip.text = element_text(face = "italic", size = 7.3)) +
  labs(x = "Year", y = "Length (mm)")

# Fit model
m <- brm(
  est2 ~ s(year_sc),
  family = student(),
  control = list(adapt_delta = 0.9),
  data = index_dat)

p2 <- index_dat |> 
  data_grid(year = seq_range(year, n = 50)) |>
  mutate(year_sc = as.numeric(scale(year)),
         yday = 31) |> 
  add_epred_draws(m) |> 
  ggplot() + 
  #geom_point(data = index_dat, aes(year, est2)) +
  stat_lineribbon(aes(year, .epred), alpha = 0.25, .width = c(0.9), size = 0.75,
                  fill = "steelblue") +
  stat_lineribbon(aes(year, .epred), alpha = 0.25, .width = c(0), size = 0.75,
                  fill = "steelblue") + 
  xlim(min(skag$year), 2022)

p1 / p2

# correlations between means

index_dat |> 
  summarise(mean = mean(est2), .by = year) |> 
  ggplot(aes(year, mean)) +
  geom_point(alpha = 0.8) +
  theme(strip.text = element_text(face = "italic", size = 7.3)) +
  labs(x = "Year", y = "Length (mm)")

size_mean <- index_dat |> 
  summarise(size = mean(est2), .by = year) 

calanus_mean <- skag |>
  mutate(density = as.numeric(scale(density))) |> 
  summarise(density = mean(density), .by = year) |> 
  left_join(size_mean, by = "year") |> 
  drop_na()
  
p3 <- ggplot(calanus_mean, aes(density, size)) + 
  geom_point()

p1 + p2
  



  
p1_data <- skag |> 
  data_grid(year = seq_range(year, n = 50)) |>
  mutate(year_sc = as.numeric(scale(year)),
         yday = 31) |> 
  add_epred_draws(m5)

p2_data <- index_dat |> 
  data_grid(year = seq_range(year, n = 50)) |>
  mutate(year_sc = as.numeric(scale(year)),
         yday = 31) |> 
  add_epred_draws(m)

pp_dat <- bind_rows(p1_data |> mutate(type = "density"),
                    p2_data |> mutate(type = "size"))

ggplot(pp_dat) + 
  #geom_point(data = index_dat, aes(year, est2)) +
  facet_wrap(~type, scales = "free_y", ncol = 1) +
  stat_lineribbon(aes(year, .epred), alpha = 0.3, .width = c(0.9), size = 0.75,
                  fill = "steelblue") +
  stat_lineribbon(aes(year, .epred), alpha = 0.3, .width = c(0), size = 0.75,
                  fill = "steelblue")

# FIX THIS PLOT







# Prepare the data for both plots
p1_data <- skag |> 
  data_grid(year = seq_range(year, n = 50)) |>
  mutate(year_sc = as.numeric(scale(year)),
         yday = 31) |> 
  add_epred_draws(m5)
p2_data <- index_dat |> 
  data_grid(year = seq_range(year, n = 50)) |>
  mutate(year_sc = as.numeric(scale(year)),
         yday = 31) |> 
  add_epred_draws(m)

# Get the min and max of each dataset to calculate the scaling factor
p1_min <- min(p1_data$.epred, na.rm = TRUE)
p1_max <- max(p1_data$.epred, na.rm = TRUE)
p2_min <- min(p2_data$.epred, na.rm = TRUE)
p2_max <- max(p2_data$.epred, na.rm = TRUE)

# Calculate the scaling factors for the second axis
scale_factor <- 110

# Function to rescale the second axis values
rescale <- function(x) x * scale_factor
rescale_inv <- function(x) x / scale_factor  # This is the inverse function

pal <- brewer.pal(n = 5, name = "Dark2")[c(1, 3)]

# Create the dual-axis plot
ggplot() +
  stat_lineribbon(data = p1_data, aes(year, .epred), alpha = 0.25, 
                  .width = c(0.9), size = 0.75, fill = pal[1]) +
  stat_lineribbon(data = p1_data, aes(year, .epred), alpha = 0.25, 
                  .width = c(0), size = 0.75, fill = pal[1]) +
  stat_lineribbon(data = p2_data, aes(year, rescale(.epred)), alpha = 0.3, 
                  .width = c(0.9), size = 0.75, fill = pal[2]) +
  stat_lineribbon(data = p2_data, aes(year, rescale(.epred)), alpha = 0.3, 
                  .width = c(0), size = 0.75, fill = pal[2]) +
  scale_y_continuous(
    name = "Calanus finmarchicus Density", # TODO
    sec.axis = sec_axis(~ rescale_inv(.), name = "Index Value")
  ) +
  labs(x = "Year") +
  theme(
    legend.position = "bottom",
    axis.title.y.left = element_text(color = pal[1], size = 10),
    axis.title.y.right = element_text(color = pal[2], size = 10)
  )

ggsave(paste0(home, "/figures/cor_zp.pdf"), width = 11, height = 11, units = "cm")
