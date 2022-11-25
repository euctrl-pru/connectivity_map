# map of connectivity for data snapshot
library(here)
library(readr)
library(dplyr)
library(forcats)
library(udunits2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(pruatlas)
library(scales)

library(ggtext)


ipairs33 <- read_csv(here("data", "ipairs33_v3_0_withC_2022Q3.csv"))
country_code <- read_csv2(here("data", "country_codes.csv")) %>%
  select(country_name, country_iso2_code)
centroids <- read_csv(here("data","CtoC_330gcd_drive_interim.csv"))


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
  "UKI31",  "from London",
  "NL329",  "Amsterdam",
  "BE100",  "from Brussels",
  "DE300",  "Berlin",
  "BG412",  "from Sofia",
  "FR101",  "Paris",
  "IE061",  "Dublin",
  "MT001",  "Malta"
)

points <- df_capitals %>%
  left_join(centroids %>% 
              dplyr::select(NUTS_ID.x,
                            popLat.x,
                            popLon.x)%>%
              distinct(),
            by=c("rfrom"="NUTS_ID.x"))%>%
  filter(!is.na(popLon.x))%>%
  dplyr::select(rfrom,popLat.x,popLon.x)


points <-  st_as_sf(points, coords = c("popLon.x","popLat.x"), crs = 4326)%>%
  st_transform(crs = st_crs(3035))%>%
  st_coordinates(points)%>%
  cbind(points)%>%
  dplyr::select(-popLat.x,-popLon.x)


# calculate the difference between flight choices and average value ----
ipair <- ipair_capitals %>%
  left_join(ipairs_nuts, by = "rto") %>%
  mutate(diff = allFlights-value) %>%
  # select(rfrom.x, rto, diff) %>%
  left_join(country_code, by = c("rto" = "country_iso2_code")) %>%
  arrange(desc(diff)) %>%
  mutate(
    country_name = case_when(
      rto == "NL" ~ "Netherlands",
      rto == "GB" ~ "United Kingdom",
      TRUE        ~ country_name),
    country_name = fct_reorder(country_name, diff)
  ) %>%
  left_join(df_capitals, by = c("rfrom.x" = "rfrom"))%>%
  left_join(points,
            by=c("rfrom.x"="rfrom"))




# plot world map ----
world_map <- ne_countries(scale = 50, returnclass = 'sf')


# add flight choice data
data_for_map <- world_map %>% 
  left_join(ipair, by = c("iso_a2" = "rto")) %>%
  filter(capitals %in% c("from London", "from Brussels", "from Sofia")) %>% 
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
border_size      <- 0.1

# breaks <- c(-6, -2, 0, 2, 6, 10)
# breaks <- c(-4, -1, 1, 4, 10)
# breaks <- c(-6, -2, 0, 2, 6, 10, 20)
# breaks <- c(-6,-4, -2, 0, 2, 4, 10)#used for centered to zero plot
breaks <- c(0,1,2,3,4,5,10,30)

# plot the map
data_for_map %>% 
  # filter(capitals == "Brussels") %>% # NOTE: for testing
  ggplot(group = capitals, fill = allFlights) +
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
  geom_sf(mapping = aes(fill = allFlights, group=as.factor(capitals))) +
  #...plot capitals coordinates
  geom_point(aes(x=X,y=Y,group=as.factor(capitals)),colour="#ff5349",shape=8,size=1)+
  # ... zoom and clip on the area of interest ...
  coord_sf(xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)]) +
  # scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_fill_fermenter(
    name       = "Flights Choice (/Day)",
    # name       = NULL,
    guide=guide_colorsteps(title.position = "top"),
    n.breaks = length(breaks),
    type = "div",
    palette = "Blues",
    breaks = breaks,
    direction = 1
    ) +
  # scale_fill_viridis_b(
  #   option     = "A",
  #   direction  = -1,
  #   name       = "Flights Choice (/Day) ",
  #   breaks     = breaks,
  #   trans      = pseudo_log_trans(sigma = 2),
  #   na.value   = "#eeeeee",
  #   guide=guide_colorsteps(title.position = "top")
  # ) +
  labs(
    # title = "A well designed title",
    # subtitle = "flights choice compared to European average"
    ) +
  theme_map() +
  facet_grid(cols = vars(capitals)) +
  theme(
    plot.title.position = "plot",
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(
      # Top margin
      t = 1,
      # Top margin
      r = 2,
      # Top margin
      b = 1,
      # Left margin
      l = 2
    ),
    legend.text=element_text(size=8),
    legend.position = "top",
    # legend.position = c(0.337, 0.85),
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.background = element_rect(
      # fill = "grey89", 
      fill = "grey89", 
      colour = "grey89",
      size = 1
    ),
    legend.title = element_textbox(fill = "grey89", padding = ggplot2::margin(1, 1, 1, 1),size=8),
    strip.text.x = element_text(size = 15),
    NULL)

