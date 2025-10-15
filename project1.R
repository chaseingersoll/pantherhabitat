# install packages
install.packages ("sf")
install.packages ("dplyr")
install.packages ("tmap")
install.packages ("ggplot2")
install.packages ("terra")
# libraries 
library(sf) 
library(dplyr)
library(tmap) 
library(ggplot2) 
library(terra) # better for large geodatabases
# import data
# 1. County boundaries counties
counties <- st_read("data/cntbnd_sep15.shp")
View(counties)
# 2. Florida Panther Focus Area panther 
pantherhabitat<- st_read("data/Florida_Panther_Focus_Area.shp")
View(pantherhabitat)
# 3. Land Cover
landcover <- st_read("data/CLC_v3_8_Poly.gdb")
View(landcover)
# check CRS
st_crs(counties)
st_crs(pantherhabitat)
st_crs(landcover)
# reproject to match (the CRS of the counties)
landcover <- st_transform(landcover, st_crs(counties))
pantherhabitat <- st_transform(pantherhabitat, st_crs(counties))
#clip
landcover_panther <- st_intersection(landcover, pantherhabitat)
#intersect 
county_habitat <- st_intersection(counties, pantherhabitat)
#calculate area
county_habitat$area_km2 <- st_area(county_habitat) / 1e6
#summarize total habitat
habitat_summary <- county_habitat %>%
  group_by(NAME) %>% 
  summarise(total_habitat_km2 = sum(area_km2, na.rm = TRUE)) %>%
  arrange(desc(total_habitat_km2))

#Visualizations

#choropleth
tm_shape(counties) +
  tm_polygons(col = "grey90") +
  tm_shape(habitat_summary) +
  tm_polygons(
    fill = "total_habitat_km2",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = "brewer.greens"
    ),
    fill.legend = tm_legend(title = "Habitat (km²)")  # Legend defined inside layer
  ) +
  tm_title("Florida Panther Habitat by County") +
  tm_scalebar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "top"), type = "8star")

tm_shape(habitat_summary) +
  tm_polygons(fill= "total_habitat_km2",
              fill.scale = tm_scale_intervals(style = "quantile", 
                                               values = "brewer.greens"))
              
# Save the map as PDF
tmap_save(map, filename = "plot/florida_panther_habitat_map.pdf")
#install
library(units)

# Bar Chart
top10 <- habitat_summary %>% top_n(10, total_habitat_km2)

ggplot(top10, aes(x = reorder(NAME, total_habitat_km2),
                  y = total_habitat_km2)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 10 Counties by Panther Habitat Area",
       x = "County", y = "Habitat area (km^2)")

#pie chart
#summarize area 
# Landcover area in km²
landcover_panther$area_km2 <- st_area(landcover_panther) / 1e6
landcover_summary <- landcover_panther %>%
  group_by(NAME_SITE) %>%
  summarise(total_area = sum(area_km2, na.rm = TRUE))
#Group detailed land cover into broad types
landcover_panther <- landcover_panther %>%
  mutate(grouped_class = case_when(
    NAME_SITE %in% c("Salt Marsh", "Slough", "Scrub Mangrove", "Wet Flatwoods", "Shrub Bog", "Slough Marsh", "Submergent Aquatic Vegetation") ~ "Wetlands",
    NAME_SITE %in% c("Urban Open Land", "Urban Open Forested", "Residential, Low Density",
                     "Residential, Med. Density - 2-5 Dwelling Units/AC", "Residential, High Density > 5 Dwelling Units/AC",
                     "Utilities", "Transportation", "Stormwater Treatment Areas") ~ "Urban/Infrastructure",
    NAME_SITE %in% c("Row Crops", "Sod Farms", "Orchards/Groves", "Specialty Farms", "Vineyard and Nurseries", "Other Agriculture") ~ "Agriculture",
    NAME_SITE %in% c("Pine - Mesic Oak", "Mixed Hardwood-Coniferous", "Mixed Wetland Hardwoods",
                     "Scrubby Flatwoods", "Mesic Hammock", "Upland Hardwood Forest", "Mixed Hardwood-Coniferous Swamps") ~ "Forests",
    TRUE ~ "Other"
  ))
#Summarize using area_km2 column
landcover_grouped_summary <- landcover_panther %>%
  group_by(grouped_class) %>%
  summarise(total_area = sum(as.numeric(area_km2), na.rm = TRUE)) %>%
  arrange(desc(total_area)) %>%
  mutate(
    pct = total_area / sum(total_area) * 100,
    label = paste0(grouped_class, "\n", round(pct, 1), "%")
  )
# Plot the pie chart
ggplot(landcover_grouped_summary, aes(x = "", y = total_area, fill = grouped_class)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Grouped Land Cover Composition within Panther Habitat") +
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
#improve
landcover_grouped_summary <- landcover_grouped_summary %>%
  mutate(label = ifelse(pct > 5, paste0(grouped_class, "\n", round(pct, 1), "%"), ""))

# Group + summarize (reuse your grouped and area_km2 logic)
landcover_grouped_summary <- landcover_panther %>%
  group_by(grouped_class) %>%
  summarise(total_area = sum(as.numeric(area_km2), na.rm = TRUE)) %>%
  arrange(desc(total_area)) %>%
  mutate(
    pct = total_area / sum(total_area) * 100,
    label = ifelse(pct > 5, paste0(grouped_class, "\n", round(pct, 1), "%"), "")  # Show only if > 5%
  )
# Plot with filtered labels
ggplot(landcover_grouped_summary, aes(x = "", y = total_area, fill = grouped_class)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(title = "Grouped Land Cover Composition within Panther Habitat") +
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


# Summarize land cover groups and label those >3%
landcover_grouped_summary <- landcover_panther %>%
  group_by(grouped_class) %>%
  summarise(total_area = sum(as.numeric(area_km2), na.rm = TRUE)) %>%
  arrange(desc(total_area)) %>%
  mutate(
    pct = total_area / sum(total_area) * 100,
    label = ifelse(pct > 3, paste0(grouped_class, "\n", round(pct, 1), "%"), "")
  )

# Plot — only one ggplot call
ggplot(landcover_grouped_summary, aes(x = "", y = total_area, fill = grouped_class)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(title = "Grouped Land Cover Composition within Panther Habitat") +
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

View(landcover_grouped_summary)

other_types <- landcover_panther %>%
  filter(grouped_class == "Other") %>%
  distinct(NAME_SITE) %>%
  arrange(NAME_SITE)

print(other_types)

# Reclassify land cover types into broader groups
landcover_panther <- landcover_panther %>%
  mutate(grouped_class = case_when(
    NAME_SITE %in% c("Salt Marsh", "Slough", "Scrub Mangrove", "Wet Flatwoods", "Shrub Bog",
                     "Slough Marsh", "Submergent Aquatic Vegetation", "Freshwater Marsh") ~ "Wetlands",
    
    NAME_SITE %in% c("Aquacultural Ponds", "Artificial Impoundment/Reservoir", "Artificial/Farm Pond",
                     "Basin Marsh", "Basin Swamp", "Blackwater Stream", "Canal", "Clastic Upland Lake",
                     "Cultural - Estuarine", "Cultural - Lacustrine", "Cultural - Palustrine", "Cultural - Riverine",
                     "Cultural - Terrestrial", "Depression Marsh", "Ditch/Artificial Intermittent Stream", "Dome Swamp",
                     "Estuarine", "Estuarine Ditch/Channel", "Floodplain Marsh", "Floodplain Swamp", "Glades Marsh",
                     "Impounded Marsh", "Isolated Freshwater Marsh", "Isolated Freshwater Swamp", "Lacustrine", "Littoral",
                     "Marshes", "Natural Lakes and Ponds", "Natural Rivers and Streams", "River Floodplain Lake/Swamp Lake",
                     "Riverine", "Sawgrass", "South Florida Bayhead", "Spoil Area", "Strand Swamp", "Tidal Flat", "Wet Prairie") ~ "Aquatic/Wetlands",
    
    NAME_SITE %in% c("Urban Open Land", "Urban Open Forested", "Residential, Low Density",
                     "Residential, Med. Density - 2-5 Dwelling Units/AC", "Residential, High Density > 5 Dwelling Units/AC",
                     "Utilities", "Transportation", "Stormwater Treatment Areas",
                     "Ballfields", "Cemeteries", "Commercial and Services", "Communication",
                     "Community rec. facilities", "Golf courses", "High Intensity Urban", "Highway Rights of Way",
                     "Industrial", "Industrial Cooling Pond", "Institutional", "Parks and Zoos", "Rural Structures",
                     "Sewage Treatment Pond", "Solar Farms", "Strip Mines", "Vegetative Berm", "Urban Open Pine") ~ "Urban/Developed",
    
    NAME_SITE %in% c("Row Crops", "Sod Farms", "Orchards/Groves", "Specialty Farms", "Vineyard and Nurseries",
                     "Other Agriculture", "Citrus", "Fallow Cropland", "Fallow Orchards", "Feeding Operations",
                     "Field Crops", "Improved Pasture", "Irrigated Field Crops", "Irrigated Row Crops", "Pecan",
                     "Tree Nurseries", "Unimproved/Woodland Pasture") ~ "Agriculture/Pasture",
    
    NAME_SITE %in% c("Australian Pine", "Bay Swamp", "Baygall", "Brazilian Pepper", "Cabbage Palm",
                     "Cabbage Palm Flatwoods", "Cabbage Palm Hammock", "Cypress", "Cypress/Hardwood Swamps",
                     "Cypress/Pine/Cabbage Palm", "Cypress/Tupelo (including mixed Cypress/Tupelo)", "Hardwood Plantations",
                     "Hydric Hammock", "Hydric Pine Flatwoods", "Hydric Pine Savanna", "Live Oak", "Mangrove Swamp",
                     "Melaleuca", "Mesic Flatwoods", "Mixed Scrub-Shrub Wetland", "Oak - Cabbage Palm Forests",
                     "Oak Scrub", "Other Coniferous Wetlands", "Other Hardwood Wetlands", "Palmetto Prairie",
                     "Pine Rockland", "Prairie Hydric Hammock", "Prairie Mesic Hammock", "Rockland Hammock",
                     "Scrub", "Successional Hardwood Forest", "Tupelo", "Wet Coniferous Plantations", "Xeric Hammock") ~ "Forests/Woodlands",
    
    NAME_SITE %in% c("Bare Soil", "Bare Soil/Clear Cut", "Dry Prairie", "Grass", "Mowed Grass", "Non-vegetated Wetland",
                     "Prairie Hydric Hammock", "Prairie Mesic Hammock", "Rural Open", "Rural Open Forested",
                     "Rural Open Pine", "Shrub and Brushland", "Scrub") ~ "Open Land/Shrubland",
    
    TRUE ~ "Other"
  ))

# Summarize total area by new grouped_class
landcover_grouped_summary <- landcover_panther %>%
  group_by(grouped_class) %>%
  summarise(total_area = sum(as.numeric(area_km2), na.rm = TRUE)) %>%
  mutate(pct = total_area / sum(total_area) * 100) %>%
  arrange(desc(pct)) %>%
  # Create labels with percentages rounded to 1 decimal place
  mutate(label = paste0(grouped_class, "\n", round(pct, 1), "%"))

# pie chart change
landcover_grouped_summary$grouped_class <- factor(landcover_grouped_summary$grouped_class,
                                                  levels = landcover_grouped_summary$grouped_class)

# Pie chart
ggplot(landcover_grouped_summary, aes(x = "", y = total_area, fill = grouped_class)) +
  geom_col(width = 1, color = "white") +  # white border for slices
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4) +
  labs(title = "Land Cover Composition within Panther Habitat") +
  theme_void() +
  theme(legend.position = "none")  # no legend, labels have info
#fix pie chart again
install.packages("ggrepel")  # if not installed
library(ggrepel)

ggplot(landcover_grouped_summary, aes(x = 1, y = total_area, fill = grouped_class)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  geom_label_repel(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    show.legend = FALSE,
    nudge_x = 1.5,  # pushes labels outside
    segment.color = "grey50",
    direction = "y",
    box.padding = 0.5,
    point.padding = 0.5,
    size = 4
  ) +
  labs(title = "Land Cover Composition within Panther Habitat", fill = "Land Cover") +
  theme_void() +
  theme(legend.position = "right")

#again
library(ggrepel)

# Calculate the y positions (cumulative midpoint of each slice)
landcover_grouped_summary <- landcover_grouped_summary %>%
  arrange(desc(pct)) %>%
  mutate(
    ymax = cumsum(total_area),
    ymin = c(0, head(ymax, n = -1)),
    label_y = (ymax + ymin) / 2
  )

ggplot(landcover_grouped_summary, aes(x = 1, y = total_area, fill = grouped_class)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  geom_label_repel(
    aes(y = label_y, label = label),
    x = 1.3,           
#redo
# Calculate the y positions (optional, not needed without labels)
landcover_grouped_summary <- landcover_grouped_summary %>%
  arrange(desc(pct)) %>%
  mutate(
    ymax = cumsum(total_area),
    ymin = c(0, head(ymax, n = -1)),
    label_y = (ymax + ymin) / 2
  )
#Final Pie chart without labels!
ggplot(landcover_grouped_summary, aes(x = 1, y = total_area, fill = grouped_class)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Land Cover Composition within Panther Habitat", fill = "Land Cover") +
  theme_void() +
  theme(legend.position = "right")

