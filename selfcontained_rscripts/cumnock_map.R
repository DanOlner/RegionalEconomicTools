# =============================================================================
# Cumnock 60km Radius Map
# Creates a map showing Cumnock, Ayrshire with a 60km radius buffer
# =============================================================================

# 1. Load required libraries
library(tmaptools)  # For geocoding via OpenStreetMap Nominatim API
library(sf)         # For simple features / spatial operations
library(tmap)       # For thematic mapping with tile layers

# 2. Geocode Cumnock using OpenStreetMap Nominatim API
message("Geocoding Cumnock, Ayrshire...")
location <- geocode_OSM("Cumnock, Ayrshire, Scotland", as.data.frame = TRUE)
print(paste("Coordinates found:", location$lon, location$lat))

# 3. Create sf point object for Cumnock
cumnock_point <- st_point(c(location$lon, location$lat))
cumnock_sf <- st_sf(
  name = "Cumnock",
  geometry = st_sfc(cumnock_point, crs = 4326)
)

# 4. Create 60km buffer around Cumnock
# Transform to British National Grid (EPSG:27700) for accurate meter-based buffer
cumnock_projected <- st_transform(cumnock_sf, 27700)
buffer_60km <- st_buffer(cumnock_projected, dist = 60000)  # 60km = 60000 meters

# Transform buffer back to WGS84 for mapping with tiles
buffer_60km_wgs84 <- st_transform(buffer_60km, 4326)

# 5. Create expanded bounding box for map view (show surrounding area)
bbox <- st_bbox(buffer_60km_wgs84)
x_range <- bbox["xmax"] - bbox["xmin"]
y_range <- bbox["ymax"] - bbox["ymin"]
expansion <- 0.15  # 15% extra view around buffer

expanded_bbox <- c(
  bbox["xmin"] - x_range * expansion,
  bbox["ymin"] - y_range * expansion,
  bbox["xmax"] + x_range * expansion,
  bbox["ymax"] + y_range * expansion
)

# 6. Set tmap to view mode for tile layer support
tmap_mode("view")

# 7. Create the map (using tmap v4 syntax)
cumnock_map <- tm_basemap("OpenStreetMap") +
  # Draw the 60km buffer with 50% transparency
  tm_shape(buffer_60km_wgs84) +
  tm_polygons(
    fill = "steelblue",
    fill_alpha = 0.5,
    col = "darkblue",
    lwd = 2
  ) +
  # Mark Cumnock location
  tm_shape(cumnock_sf) +
  tm_dots(fill = "red", size = 0.5) +
  # Set the view to show expanded area
  tm_view(
    set_bounds = expanded_bbox,
    set_view = c(lon = location$lon, lat = location$lat, zoom = 8)
  )

# 8. Display the map
print(cumnock_map)

# 9. Save map to JPEG
message("Saving map to cumnock_60km_radius.jpg...")
tmap_save(
  cumnock_map,
  filename = "cumnock_60km_radius.jpg",
  width = 1200,
  height = 1000,
  dpi = 150
)

message("Map saved successfully!")
