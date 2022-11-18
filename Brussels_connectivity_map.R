# map of connectivity for data snapshot
library(here)
library(readr)
library(dplyr)
library(forcats)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(pruatlas)
library(scales)

ipairs33 <- read_csv(here("data", "ipairs33_v3_0_withC_2022Q3.csv"))
country_code <- read_csv2(here("data", "country_codes.csv")) %>%
  select(country_name, country_iso2_code)

# filter nuts3-nuts0 flight choice from intra-eu connectivity ----
ipair_capitals <- ipairs33 %>%
  filter(
    # short drive time
    driveThresh == "S",
    # short drive time
    Changes == 0,
    # only specific nuts3
    rfrom %in% c(
      # "BE100",
      "UKI31",
      "NL329",
      "BG412",
      "DE300",
      "BE100",
      "FR101",
      "IE061",
      "MT001",
      NULL)) %>% 
  dplyr::select(rfrom, rto, allFlights)

# calculate average flight choice for all nuts 3 ----
ipairs_nuts <- ipairs33 %>%
  filter(
    driveThresh=="S",
    Changes==0) %>%
  group_by(rto) %>%
  summarise(value = mean(allFlights)) %>%
  mutate(rfrom = "AVG_NUTS")

## add capitals name to nuts3 ----
df_capitals <- tribble(
   ~rfrom, ~capitals,
  "UKI31",  "London",
  "NL329",  "Amsterdam",
  "BE100",  "Brussels",
  "DE300",  "Berlin",
  "BG412",  "Sofya",
  "FR101",  "Paris",
  "IE061",  "Dublin",
  "MT001",  "Malta"
)

# calculate the difference between flight choices and average value ----
ipair <- ipair_capitals %>%
  left_join(ipairs_nuts, by = "rto") %>%
  mutate(diff = allFlights-value) %>%
  select(rfrom.x, rto, diff) %>%
  left_join(country_code, by = c("rto" = "country_iso2_code")) %>%
  arrange(desc(diff)) %>%
  mutate(
    country_name = case_when(
      rto == "NL" ~ "Netherlands",
      rto == "GB" ~ "United Kingdom",
      TRUE        ~ country_name),
    country_name = fct_reorder(country_name, diff)
  ) %>%
  left_join(df_capitals, by = c("rfrom.x" = "rfrom"))




# plot world map ----
world_map <- ne_countries(scale = 50, returnclass = 'sf')


# add flight choice data
data_for_map <- world_map %>% 
  left_join(ipair, by = c("iso_a2" = "rto")) %>%
  filter(capitals %in% c("London", "Brussels", "Sofya")) %>% 
  st_transform(crs = st_crs(3035))

# NOTE: black magic
bbox <- tribble(
  ~lon, ~lat,
    -7, 35,
    56, 63
  ) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(crs = st_crs(3035)) %>% 
  st_bbox() 


graticule <- st_graticule(crs = st_crs(3035))
colour_backgroud <- "#f5f5f2"
colour_sea       <- "#D8F4FF"
colour_land      <- "grey89"
colour_border    <- "#A9A9A9"
colour_graticule <- "#D3D3D3"
border_size      <- 0.2


# plot the map
data_for_map %>% 
  # filter(capitals == "Brussels") %>% # NOTE: for testing
  ggplot(group = capitals, fill = diff) +
  # fill the world with water...
  geom_sf(data = pruatlas::sphere_laea, fill = colour_sea) +
  # ... plot all the countries and fill with land...
  geom_sf(data = pruatlas::countries50m,
          fill   = colour_land,
          colour = colour_border,
          size = border_size) +
  # ... plot the graticule, just to know where is what ...
  geom_sf(data = graticule,    colour = colour_graticule) +
  # ... and now the real stuff, i.e. the choropleth  ...
  geom_sf(mapping = aes(fill = diff, group=as.factor(capitals))) +
  # ... zoom and clip on the area of interest ...
  coord_sf(xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)]) +
  scale_fill_viridis_b(
    option     = "A",
    direction  = -1,
    name       = "flights choice compared to European average",
    breaks     = c(-6, -2, 0, 2, 6, 10),
    trans      = pseudo_log_trans(sigma = 2),
    na.value   = "#eeeeee",
    guide      = "colourbar"
  ) +
  labs(
    title = "A well designed title",
    subtitle = "some details that are clearing the meaning of the plot"
    ) +
  theme_map() +
  theme(plot.title.position = "plot",
        legend.position="bottom",
        plot.margin = margin(
          # Top margin
          t = 0,
          # Top margin
          r = 1,
          # Top margin
          b = 0,
          # Left margin
          l = 1),
        legend.title = element_text(size=10),
        legend.key.height= unit(0.5, 'cm'),
        # Left margin
        legend.key.width= unit(0.5, 'cm')) + 
  facet_wrap(capitals ~ ., nrow = 1, ncol = 3)

