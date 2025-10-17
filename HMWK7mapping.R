library(dplyr)
library(zipcodeR)
library(sf)
library(MASS)
library(viridis)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggplot2)

#Loading my data, filtering for my urban and rural lakes and making it easier on myself by making my urban lake ID's vectors

creeltable <- read.csv("creeltable.csv")

urban_lakes <- c("MD", "ML", "KE", "WA", "WI", "YA", "SW", "FI",
                 "UM", "EBP", "CEP", "CUP", "HUBP", "MM", 
                 "LSSP", "KLP", "RSP", "SSMP")

rural_lakes <- c("AQ", "AR", "BH", "BK", "BOT", "BY", "CA", "DS", "DY", "ER",
                 "FD", "HT", "ID", "IV", "JS", "LC", "LE", "LH", "LL", "LR",
                 "LT", "LV", "MS", "NH", "OB", "PK", "PN", "PT", "SA", "SE",
                 "SM", "SV", "TO", "UG", "WB", "WC", "WN", "WS", "BS", "BV",
                 "NT", "LJ", "TR", "BA", "EA", "BO", "BL", "BM", "LG", "ST", "HH")

#This makes an additional column called "site_group" that makes it easier to filter and plot later by site type.
creeltable <- creeltable %>%
  mutate(site_group = case_when(
    lakeID %in% urban_lakes ~ "urban",
    lakeID %in% rural_lakes ~ "rural",
    TRUE ~ NA_character_
  ))

creeltableurban <- creeltable %>%
  filter(site_group == "urban")

creeltablerural <- creeltable %>%
  filter(site_group == "rural")

#removing any NA's in the homezip data, making another df so I don't mess with the raw rural data
ruralmap <- creeltablerural %>%
  filter(!is.na(homeZip)) %>%
  mutate(homeZip = as.character(homeZip))

zip_locs <- reverse_zipcode(ruralmap$homeZip) %>%
  filter(!is.na(lat)) %>%
  rename(home_lat = lat, home_lon = lng)

ruralmap_geo <- ruralmap %>%
  left_join(zip_locs, by = c("homeZip" = "zipcode")) %>%
  filter(!is.na(home_lat) & !is.na(home_lon))

#loading in my lake data, which is alread in lat long
rural_lakes_sf <- read.csv("lakeTable.csv") %>%
  filter(lakeID %in% rural_lakes) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#this code sets a density threshold, so showing 95% densest area
kde_rural <- with(ruralmap_geo, kde2d(home_lon, home_lat, n = 900))
kde_df_rural <- expand.grid(lon = kde_rural$x, lat = kde_rural$y)
kde_df_rural$density <- as.vector(kde_rural$z)

# Filter for densest 95%
density_threshold <- quantile(kde_df_rural$density, probs = 0.05)
kde_df_rural_95 <- kde_df_rural %>%
  filter(density >= density_threshold)

# Base map of WI and neighboring states taken from the rnaturalearth package

states <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name %in% c("Wisconsin", "Illinois", "Minnesota", "Michigan"))

#map limits
xlims <- c(-93.5, -83.5)
ylims <- c(41.2, 47.5)

#finally we can make our map. I think its because I am essentially layering to maps or grids on top of eachother, but I cannot for the life of me figure out how to get the main figure white and not grey. I asked chat GPT to help and it was less than helpful in resolving that, this includes some of the code that I integrated from it

ruraldensitymap <- ggplot() +
  geom_sf(data = states, fill = "white", color = "black", size = 0.4) +
  geom_tile(
    data = kde_df_rural_95,
    aes(x = lon, y = lat, fill = density),
    alpha = 0.6
  ) +
  geom_sf(data = rural_lakes_sf, color = "blue", size = 0.3) +
  scale_fill_viridis(option = "magma", name = "Angler Density") +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
  labs(title = "Rural Angler Density Map") +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5)
  )


#Now I can save my map
ggsave(
  "rural_density_map.png",
  ruraldensitymap,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

#since that wrecked my spirit, here is an easy study site map of my urban sites.

urban_lakes_sf <- read.csv("lakeTable.csv") %>%
  filter(lakeID %in% urban_lakes) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

wisconsin <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "Wisconsin")

#alas here is the map
urban_sites_map <- ggplot() +
  geom_sf(data = wisconsin, fill = "white", color = "black", size = 0.4) +
  geom_sf(data = urban_lakes_sf, color = "red", size = 1) +
  labs(title = "Urban Angler Creel Study Sites") +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5)
  )

# Display the map
urban_sites_map