#import libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggbreak) 

#import files
ipairs33 <- readr::read_csv("../output/2022_10_11/ipairs33_v3_0_withC_2022Q3.csv")
country_code <- readr::read_csv2("../raw_data/country_codes.csv")

#filter nuts3-nuts0 flight choice from intra-eu connectivity fiñe
ipair_capitals <- ipairs33 %>%
  filter(driveThresh=="S",#filter short drive time
         Changes==0,#consider only direct flights
         rfrom %in%
           # c("BE100"
           c("UKI31","NL329","BG412","DE300","BE100","FR101","IE061","MT001"
           ))%>%#filter only specific nuts3
  dplyr::select(rfrom,rto,allFlights)##select only from nuts3 to nuts0 flight choices


#calculate average flight choice for all nuts 3
ipairs_nuts <- ipairs33 %>%
  filter(driveThresh=="S",
         Changes==0)%>%
  group_by(rto)%>%
  summarise(value = mean(allFlights))%>%
  mutate(rfrom="AVG_NUTS")



##add capitals name to nuts3
capitals <- c("London","Amsterdam","Brussels","Berlin","Sofya","Paris","Dublin","Malta")
rfrom <- c("UKI31","NL329","BE100","DE300","BG412","FR101","IE061","MT001")

df_capitals <- data.frame(capitals,rfrom)

#calculate the difference between flight choices and average value
ipair <- ipair_capitals %>%
  left_join(ipairs_nuts,by= "rto")%>%
  mutate(diff = allFlights-value)%>%
  dplyr::select(rfrom.x,rto,diff)%>%
  left_join(country_code %>%
              dplyr::select(country_name,country_iso2_code),
            by=c("rto"="country_iso2_code"))%>%
  arrange(desc(diff))%>%
  mutate(country_name = fct_reorder(country_name,diff),
         country_name = case_when(country_name == "Netherlands (the)"~ "Netherlands",
                                  country_name == "United Kingdom of Great Britain and Northern Ireland (the)"~ "United Kingdom",
                                  TRUE ~ as.character(country_name)))%>%
  left_join(df_capitals,by=c("rfrom.x"="rfrom"))






#plot world map
world_map <- ne_countries(scale = 50, returnclass = 'sf')
#select european union countries
european_union <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                    "Czech Rep.","Denmark","Estonia","Finland","France",
                    "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                    "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                    "Portugal","Romania","Slovakia","Slovenia","Spain",
                    "Sweden","United Kingdom")
#filter out non european countries from the map
european_union_map <- 
  world_map %>% 
  filter(continent == "Europe"|geounit %in% c("Cyprus","Turkey"))
#crop the mapo to only visualize europe
bbox_europe <- st_bbox(c(xmin = -10, ymin = 20, xmax = 50, ymax = 80), crs = st_crs(european_union_map))
european_union_map_cropped <- european_union_map
#add flight choice data
map <- 
  european_union_map_cropped %>% 
  left_join(ipair, by = c("iso_a2" = "rto"))%>%
  filter(capitals %in% c("London","Brussels","Sofya"))
#plot the map
ggplot(data = map,group=capitals,fill=(diff)) +
  geom_sf(european_union_map,mapping=aes(fill=NA))+
  geom_sf(mapping = aes(fill = (diff),group=as.factor(capitals))) +
  coord_sf(xlim = c(-13,35), ylim = c(33,73), expand = FALSE)+
  # scale_fill_gradient(name = "Difference in #flights", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50") +
  scale_fill_viridis_b(option = "A",
                       direction = -1,
                       name = "#flights choice compared to European average",
                       breaks = c(-6,-2,0,2,6,10),
                       trans = scales::pseudo_log_trans(sigma = 2),
                       na.value = "#eeeeee",
                       guide = "colourbar"
  ) +
  theme_minimal()+
  theme(plot.title.position = "plot",
        legend.position="bottom",
        plot.margin = margin(t = 0,  # Top margin
                             r = 1,  # Right margin
                             b = 0,  # Bottom margin
                             l = 1),
        legend.title = element_text(size=10),
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(0.5, 'cm'))+ # Left margin
  
  facet_wrap(capitals ~ .,nrow = 1,ncol = 4)

