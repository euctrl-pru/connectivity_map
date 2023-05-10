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
library(xlsx)

library(ggtext)


ipairs33 <- read_csv(here("data", "ipairs33_v0_0_2019Q1.csv"))
country_code <- read_csv2(here("data", "country_codes.csv")) %>%
  select(country_name, country_iso2_code)
centroids <- read_csv(here("data","CtoC_330gcd_drive_interim.csv"))

country_list <- c("BG","CY","EE","GR","LT","MT","PT","SI")
# filter nuts3-nuts0 flight choice from intra-eu connectivity ----
ipair_capitals <- ipairs33 %>%
  filter(rfrom %in% country_list)%>%
  dplyr::select(rfrom, rto, allFlights)

# calculate average flight choice for all nuts 3 ----
ipairs_nuts <- ipairs33 %>%
  group_by(rto) %>%
  summarise(value = mean(allFlights)) %>%
  mutate(rfrom = "AVG_NUTS")

# ## add capitals name to nuts3 ----
# df_capitals <- tribble(
#   ~rfrom, ~capitals,
#   "UKI31",  "London",
#   "NL329",  "Amsterdam",
#   "BE100",  "Brussels",
#   "DE300",  "Berlin",
#   "BG412",  "Sofia",
#   "FR101",  "Paris",
#   "IE061",  "Dublin",
#   "MT001",  "Malta",
#   "AT130",  "Vienna",
#   "CY000", "Nicosia",
#   "CZ010","Prague",
#   "DK011", "Copenhague",
#   "EE006", "Tallinn",
#   "ES300", "Madrid",
#   "FI1B1", "Helsinki",
#   "EL305", "Athens",
#   "HR401", "Zagreb",
#   "HU110", "Budapest",
#   "ITI43", "Rome",
#   "LT00A", "Vilnius",
#   "LU000", "Luxembourg",
#   "LV006", "Riga",
#   "PL127", "Warsaw",
#   "PT170", "Lisbon",
#   "RO321","Bucharest",
#   "SE110", "Stockholm",
#   "SI041","Ljubljana",
#   "SK010","Bratislava"
#   
#   
# )
# 
# points <- df_capitals %>%
#   left_join(centroids %>% 
#               dplyr::select(NUTS_ID.x,
#                             popLat.x,
#                             popLon.x)%>%
#               distinct(),
#             by=c("rfrom"="NUTS_ID.x"))%>%
#   filter(!is.na(popLon.x))%>%
#   dplyr::select(rfrom,popLat.x,popLon.x)
# 
# 
# points <-  st_as_sf(points, coords = c("popLon.x","popLat.x"), crs = 4326)%>%
#   st_transform(crs = st_crs(3035))%>%
#   st_coordinates(points)%>%
#   cbind(points)%>%
#   dplyr::select(-popLat.x,-popLon.x)


# calculate the difference between flight choices and average value ----
ipair <- ipair_capitals %>%
  left_join(ipairs_nuts, by = "rto") %>%
  mutate(diff = allFlights-value) %>%
  # select(rfrom.x, rto, diff) %>%
  left_join(country_code, by = c("rfrom.x" = "country_iso2_code")) %>%
  arrange(desc(diff))
# %>%
  # mutate(
  #   country_name = case_when(
  #     rto == "NL" ~ "Netherlands",
  #     rto == "GB" ~ "United Kingdom",
  #     TRUE        ~ country_name),
  #   country_name = fct_reorder(country_name, diff))
  # ) %>%
  # left_join(df_capitals, by = c("rfrom.x" = "rfrom"))%>%
  # left_join(points,
  #           by=c("rfrom.x"="rfrom"))




# plot world map ----
world_map <- ne_countries(scale = 50, returnclass = 'sf')


# add flight choice data
data_for_map <- world_map %>% 
  left_join(ipair, by = c("iso_a2" = "rto")) %>%
  filter(rfrom.x  %in% country_list) %>%
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
breaks <- c(-6,-4, -2, 0, 2, 4, 10)#used for centered to zero plot
# breaks <- c(0,1,2,3,4,5,10,30)

# plot the map
data_for_map %>% 
  # filter(capitals == "Brussels") %>% # NOTE: for testing
  ggplot(group = country_name, fill = diff) +
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
  geom_sf(mapping = aes(fill = diff, group=as.factor(country_name))) +
  #...plot capitals coordinates
  # geom_point(aes(x=X,y=Y,group=as.factor(iso_a2)),colour="#ff5349",shape=8,size=1)+
  # ... zoom and clip on the area of interest ...
  coord_sf(xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)]) +
  # scale_fill_distiller(type = "div", palette = "RdBu") +
  scale_fill_fermenter(
    name       = "flights choice compared to European average" ,
    # name       = NULL,
    guide=guide_colorsteps(title.position = "top"),
    n.breaks = length(breaks),
    type = "div",
    # palette = "Blues",
    palette="RdBu",
    breaks = breaks,
    direction = -1
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
    title = "Aviation connectivity from",
    # subtitle = "flights choice compared to European average"
    caption="Period: 03/03/19-09/03/19"
  ) +
  theme_map() +
  facet_grid(cols = vars(country_name)) +
  facet_wrap(~country_name,ncol=4)+
  theme(
    plot.title = element_text(size=18,hjust = 0.5,face = "bold"),
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
    legend.text=element_text(size=10),
    # legend.position = "top",
    legend.position = c(0.45, 0.9),
    legend.direction = "horizontal",
    # legend.justification = "center",
    legend.background = element_rect(
      fill = "transparent",
      # fill = "grey89", 
      colour = "grey89",
      # size = 1
      size=1,
    ),
    legend.title = element_textbox(fill = "grey89", padding = ggplot2::margin(1, 1, 1, 1),size=11),
    strip.text.x = element_text(size = 15),
    NULL)

#save the data in an excel worksheet
world_map %>% 
  st_drop_geometry()%>%
  right_join(ipair, by = c("iso_a2" = "rto")) %>% 
  rename(rto = iso_a2)%>%
  dplyr::select(capitals,rto,diff,allFlights)%>%
  write_csv("../../FlightsChoice_from_capitals.xls")

