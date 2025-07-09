## ----message = FALSE, warning = FALSE, results = 'hide'--------------------------------------------------------
pkgs <- c("sf", "gstat", "mapview", "nngeo", "rnaturalearth", "dplyr", "areal",
          "nomisr", "osmdata", "OpenStreetMap", "tidyr", "texreg", "downlit", "xml2") 
lapply(pkgs, require, character.only = TRUE)



## ----message = FALSE, warning = FALSE, results = 'hide'--------------------------------------------------------
pkgs <- c("tmap", "tmaptools", "viridisLite", 
          "ggplot2", "ggthemes", "rmapshaper", "cowplot") 
lapply(pkgs, require, character.only = TRUE)



## --------------------------------------------------------------------------------------------------------------
sessionInfo()



## --------------------------------------------------------------------------------------------------------------
load("_data/msoa_spatial.RData")
load("_data/ulez_spatial.RData")
load("_data/pollution_spatial.RData")
load("_data/pubs_spatial.RData")


## --------------------------------------------------------------------------------------------------------------
st_crs(msoa.spdf) == st_crs(pol.spdf)
st_crs(msoa.spdf) == st_crs(pubs.spdf)
st_crs(msoa.spdf) == st_crs(ulez.spdf)


## --------------------------------------------------------------------------------------------------------------
# MSOA in different crs --> transform
pol.spdf <- st_transform(pol.spdf, crs = st_crs(msoa.spdf))
pubs.spdf <- st_transform(pubs.spdf, crs = st_crs(msoa.spdf))
ulez.spdf <- st_transform(ulez.spdf, crs = st_crs(msoa.spdf))


# Check if all geometries are valid, and make valid if needed
msoa.spdf <- st_make_valid(msoa.spdf)



## --------------------------------------------------------------------------------------------------------------

# Subset to pollution estimates in London
pol_sub.spdf <- pol.spdf[msoa.spdf, ] # or:
pol_sub.spdf <- st_filter(pol.spdf, msoa.spdf)
mapview(pol_sub.spdf)



## --------------------------------------------------------------------------------------------------------------
# Subset pubs to pubs not in the ulez area
sub2.spdf <- pubs.spdf[ulez.spdf, , op = st_disjoint] # or:
sub2.spdf <- st_filter(pubs.spdf, ulez.spdf, .predicate = st_disjoint)
mapview(sub2.spdf)


## --------------------------------------------------------------------------------------------------------------
msoa.spdf$ulez <- 0

# intersecting lsoas
within <- msoa.spdf[ulez.spdf,]

# use their ids to create binary indicator 
msoa.spdf$ulez[which(msoa.spdf$MSOA11CD %in% within$MSOA11CD)] <- 1
table(msoa.spdf$ulez)


## --------------------------------------------------------------------------------------------------------------
# Assign MSOA to each point
pubs_msoa.join <- st_join(pubs.spdf, msoa.spdf, join = st_within)

# Count N by MSOA code (drop geometry to speed up)
pubs_msoa.join <- dplyr::count(st_drop_geometry(pubs_msoa.join),
                               MSOA11CD = pubs_msoa.join$MSOA11CD,
                               name = "pubs_count")
sum(pubs_msoa.join$pubs_count)

# Merge and replace NAs with zero (no matches, no pubs)
msoa.spdf <- merge(msoa.spdf, pubs_msoa.join,
                   by = "MSOA11CD", all.x = TRUE)
msoa.spdf$pubs_count[is.na(msoa.spdf$pubs_count)] <- 0



## --------------------------------------------------------------------------------------------------------------
# Use geometric centroid of each MSOA
cent.sp <- st_centroid(msoa.spdf[, "MSOA11CD"])

# Get K nearest neighbour with distance
knb.dist <- st_nn(cent.sp, 
                  pubs.spdf,
                  k = 1,             # number of nearest neighbours
                  returnDist = TRUE, # we also want the distance
                  progress = FALSE)
msoa.spdf$dist_pubs <- unlist(knb.dist$dist)
summary(msoa.spdf$dist_pubs)



## --------------------------------------------------------------------------------------------------------------
# Create buffer (1km radius)
cent.buf <- st_buffer(cent.sp, 
                      dist = 1000) # dist in meters
mapview(cent.buf)



### New version (using areal package)
# We use area-weighted interpolation from the areal package
int.spdf <- aw_interpolate(
  cent.buf,                         # interpolate to
  tid = "MSOA11CD",                 # id of target
  source = pol.spdf,                # the source to be interpolated from
  sid = "ukgridcode",               # source id
  weight = "sum",                   # function for interpolation
  output = "sf",                    # output object
  intensive = "no22011"             # variables to be interpolated
)

# # ### Old version ("by hand")
# # Add area of each buffer (in this constant)
# cent.buf$area <- as.numeric(st_area(cent.buf))
# 
# # Calculate intersection of pollution grid and buffer
# int.df <- st_intersection(cent.buf, pol.spdf)
# int.df$int_area <- as.numeric(st_area(int.df)) # area of intersection
# 
# # And we use the percent overlap areas as the weights to calculate a weighted mean.
# 
# # Area of intersection as share of buffer
# int.df$area_per <- int.df$int_area / int.df$area
# 
# # Aggregate as weighted mean
# int.df <- st_drop_geometry(int.df)
# int.df$no2_weighted <- int.df$no22011 * int.df$area_per
# int.df <- aggregate(list(no2 = int.df[, "no2_weighted"]), 
#                     by = list(MSOA11CD = int.df$MSOA11CD),
#                     sum)


## --------------------------------------------------------------------------------------------------------------
# Back to non-spatial df
int.df <- st_drop_geometry(int.spdf)
names(int.df)[2] <- "no2"

# Merge back to spatial data.frame
msoa.spdf <- merge(msoa.spdf, int.df, by = "MSOA11CD", all.x = TRUE)

mapview(msoa.spdf, zcol = "no2")



## --------------------------------------------------------------------------------------------------------------
# Define ethnic group shares
msoa.spdf$per_mixed <- msoa.spdf$KS201EW_200 / msoa.spdf$KS201EW0001 * 100
msoa.spdf$per_asian <- msoa.spdf$KS201EW_300 / msoa.spdf$KS201EW0001 * 100
msoa.spdf$per_black <- msoa.spdf$KS201EW_400 / msoa.spdf$KS201EW0001 * 100
msoa.spdf$per_other <- msoa.spdf$KS201EW_500 / msoa.spdf$KS201EW0001 * 100

# Define tenure
msoa.spdf$per_owner <- msoa.spdf$KS402EW_100 / msoa.spdf$KS402EW0001 * 100
msoa.spdf$per_social <- msoa.spdf$KS402EW_200 / msoa.spdf$KS402EW0001 * 100

# Non British passport
msoa.spdf$per_nonUK <- (msoa.spdf$KS205EW0001 - msoa.spdf$KS205EW0003)/ msoa.spdf$KS205EW0001 * 100
msoa.spdf$per_nonEU <- (msoa.spdf$KS205EW0001 - msoa.spdf$KS205EW0003 -
                          msoa.spdf$KS205EW0004 - msoa.spdf$KS205EW0005  - 
                          msoa.spdf$KS205EW0006)/ msoa.spdf$KS205EW0001 * 100
msoa.spdf$per_nonUK_EU <- (msoa.spdf$KS205EW0005  + msoa.spdf$KS205EW0006)/ msoa.spdf$KS205EW0001 * 100


# Run regression
mod1.lm <- lm(no2 ~ per_mixed + per_asian + per_black + per_other +
                per_owner + per_social + pubs_count + POPDEN + ulez,
              data = msoa.spdf)

# summary
screenreg(list(mod1.lm), digits = 3)



## ----house-prices, cache=TRUE----------------------------------------------------------------------------------
# Download
hp.link <- "https://data.london.gov.uk/download/average-house-prices/bdf8eee7-41e1-4d24-90ce-93fe5cf040ae/land-registry-house-prices-MSOA.csv"
hp.df <- read.csv(hp.link)
hp.df <- hp.df[which(hp.df$Measure == "Median" &
                       grepl("2011", hp.df$Year)), ]
table(hp.df$Year)

# Aggregate across 2011 values
hp.df$med_house_price <- as.numeric(hp.df$Value)
hp.df <- aggregate(hp.df[, "med_house_price", drop = FALSE],
                   by = list(MSOA11CD = hp.df$Code),
                   FUN = function(x) mean(x, na.rm = TRUE))

# Merge spdf and housing prices
msoa.spdf <- merge(msoa.spdf, hp.df,
                   by = "MSOA11CD",
                   all.x = TRUE, all.y = FALSE)
hist(log(msoa.spdf$med_house_price))


## --------------------------------------------------------------------------------------------------------------
# Save
save(msoa.spdf, file = "_data/msoa2_spatial.RData")


## --------------------------------------------------------------------------------------------------------------

# Define colours
cols <- viridis(n = 7, direction = 1, option = "C")

mp1 <- tm_shape(msoa.spdf) +
  tm_polygons(
    fill = "no2",                      # variable for fill colouring
    fill_alpha = 1,                    # transparency 
    fill.scale = tm_scale_intervals(
      style = "fisher",                # algorithm to def cut points
      n = 7,                           # Number of requested cut points
      values = cols                    # colouring
    ),
    fill.legend = tm_legend(
      title = "NO2",
      hist = FALSE
    )
  ) +
  tm_borders(
    col = "white",
    lwd = 0.5,
    fill_alpha = 0.5
  )


mp1



## --------------------------------------------------------------------------------------------------------------

# Define colours
cols <- viridis(n = 7, direction = 1, option = "C")

mp1 <- tm_shape(msoa.spdf) +
  tm_polygons(
    fill = "no2",
    fill_alpha = 1,
    fill.scale = tm_scale_intervals(
      style = "fisher",
      n = 7,
      values = cols
    ),
    fill.legend = tm_legend(
      title = "NO2",
      hist = FALSE
    )
  ) +
  tm_borders(
    col = "white",
    lwd = 0.5,
    fill_alpha = 0.5
  ) +
  tm_shape(ulez.spdf) +
  tm_borders(
    col = "red",
    lwd = 1,
    fill_alpha = 1
  )


mp1



## --------------------------------------------------------------------------------------------------------------

# Define colours
cols <- viridis(n = 7, direction = 1, option = "C")

mp1 <- tm_shape(msoa.spdf) +
  tm_polygons(
    fill = "no2",
    fill_alpha = 1,
    fill.scale = tm_scale_intervals(
      style = "fisher",
      n = 7,
      values = cols
    ),
    fill.legend = tm_legend(
      title = expression('in'~mu*'g'/m^{3}),
      hist = FALSE
    )
  ) +
  tm_borders(
    col = "white",
    lwd = 0.5,
    fill_alpha = 0.5
  ) +
  tm_shape(ulez.spdf) +
  tm_borders(
    col = "red",
    lwd = 1,
    fill_alpha = 1
  ) +
  tm_layout(
    frame = FALSE,
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.position = c("right", "bottom"),
    legend.outside = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.8
  ) +
  tm_title_out(
    text = "NO2",
    position = tm_pos_out("center", "top",      # top cell
                          pos.h = "center"),    # position in cell
    size = 1.6
  )

mp1




## ----warning=FALSE---------------------------------------------------------------------------------------------
# Save old projection
crs_orig <- st_crs(msoa.spdf)

# Change projection
ulez.spdf <- st_transform(ulez.spdf, 4326)
msoa.spdf <- st_transform(msoa.spdf, 4326)

# Get OSM data for background
osm_tmp <- read_osm(st_bbox(msoa.spdf), ext = 1.1, type = "osm-german") 

# Define colours
cols <- viridis(n = 7, direction = 1, option = "C")

mp1 <- tm_shape(osm_tmp) + 
  tm_rgb() +
  
  tm_shape(msoa.spdf) + 
  tm_polygons(
    fill = "no2",
    fill_alpha = 0.8,
    fill.scale = tm_scale_intervals(
      style = "fisher",
      n = 7,
      values = cols
    ),
    fill.legend = tm_legend(
      title = expression('in'~mu*'g'/m^{3}),
      hist = FALSE
    )
  ) +
  tm_shape(ulez.spdf) +
  tm_borders(
    col = "red",
    lwd = 1,
    fill_alpha = 1
  ) +
  tm_layout(
    frame = FALSE,
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.position = c("right", "bottom"),
    legend.outside = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.8
  ) +
  tm_title_out(
    text = "NO2",
    position = tm_pos_out("center", "top", pos.h = "center"),
    size = 1.6
  )

mp1




## --------------------------------------------------------------------------------------------------------------
# Define colours
cols1 <- viridis(n = 7, direction = 1, option = "C")

# Define colours
cols2 <- viridis(n = 7, direction = 1, option = "D")

# First map: NO2
mp1 <- tm_shape(osm_tmp) + 
  tm_rgb() +
  tm_shape(msoa.spdf) + 
  tm_polygons(
    fill = "no2",
    fill_alpha = 0.8,
    fill.scale = tm_scale_intervals(
      style = "fisher",
      n = 7,
      values = cols1
    ),
    fill.legend = tm_legend(
      title = expression('in'~mu*'g'/m^{3}),
      hist = FALSE
    )
  ) +
  tm_shape(ulez.spdf) +
  tm_borders(
    col = "red",
    lwd = 1,
    fill_alpha = 1
  ) +
  tm_layout(
    frame = FALSE,
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.position = c("right", "bottom"),
    legend.outside = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.8
  ) +
  tm_title_out(
    text = "NO2",
    position = tm_pos_out("center", "top", pos.h = "center"),
    size = 1.4
  )

# Second map: % Black
mp2 <- tm_shape(osm_tmp) + 
  tm_rgb() +
  tm_shape(msoa.spdf) + 
  tm_polygons(
    fill = "per_black",
    fill_alpha = 0.8,
    fill.scale = tm_scale_intervals(
      style = "fisher",
      n = 7,
      values = cols2
    ),
    fill.legend = tm_legend(
      title = "% black",
      hist = FALSE
    )
  ) +
  tm_shape(ulez.spdf) +
  tm_borders(
    col = "red",
    lwd = 1,
    fill_alpha = 1
  ) +
  tm_layout(
    frame = FALSE,
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.position = c("right", "bottom"),
    legend.outside = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.8
  ) +
  tm_title_out(
    text = "Ethnic Black inhabitants",
    position = tm_pos_out("center", "top", pos.h = "center"),
    size = 1.4
  )

# Arrange side by side
tmap_arrange(mp1, mp2, ncol = 2, nrow = 1)


## --------------------------------------------------------------------------------------------------------------
png(file = paste("London.png", sep = ""), width = 14, height = 7, units = "in", 
    res = 100, bg = "white")
par(mar=c(0,0,3,0))
par(mfrow=c(1,1),oma=c(0,0,0,0))
tmap_arrange(mp1, mp2, ncol = 2, nrow = 1)
dev.off()


## --------------------------------------------------------------------------------------------------------------
gp <- ggplot(msoa.spdf)+
    geom_sf(aes(fill = no2))+
    scale_fill_viridis_c(option = "B")+
    coord_sf(datum = NA)+
    theme_map()+
    theme(legend.position = c(.9, .6))
gp


## --------------------------------------------------------------------------------------------------------------
# Get some larger scale boundaries
borough.spdf <- st_read(dsn = paste0("_data", "/statistical-gis-boundaries-london/ESRI"),
                     layer = "London_Borough_Excluding_MHW" # Note: no file ending
                     )

# transform to only inner lines
borough_inner <- ms_innerlines(borough.spdf)

# Plot with inner lines
gp <- ggplot(msoa.spdf)+
    geom_sf(aes(fill = no2), color = NA)+
    scale_fill_viridis_c(option = "A")+
    geom_sf(data = borough_inner, color = "gray92")+
    geom_sf(data = ulez.spdf, color = "red", fill = NA)+
    coord_sf(datum = NA)+
    theme_map()+
    labs(fill = "NO2")+
    theme(legend.position = c(.9, .6))
gp


## --------------------------------------------------------------------------------------------------------------
sub4.spdf <- msoa.spdf[ulez.spdf, ]


## --------------------------------------------------------------------------------------------------------------
gp <- ggplot(msoa.spdf)+
    geom_sf(aes(fill = per_asian))+
    scale_fill_viridis_c(option = "E")+
    coord_sf(datum = NA)+
    theme_map()+
    theme(legend.position = c(.9, .6))
gp


## --------------------------------------------------------------------------------------------------------------
### Distance to city center
# Define centre
centre <- st_as_sf(data.frame(lon = -0.128120855701165, 
                              lat = 51.50725909644806),
                   coords = c("lon", "lat"), 
                   crs = 4326)
# Reproject
centre <- st_transform(centre, crs = st_crs(msoa.spdf))
# Calculate distance
msoa.spdf$dist_centre <- as.numeric(st_distance(msoa.spdf, centre)) / 1000
# hist(msoa.spdf$dist_centre)


## --------------------------------------------------------------------------------------------------------------

# Define colours
cols <- viridis(n = 10, direction = 1, option = "B")
cols2 <- viridis(n = 10, direction = 1, option = "E")


library(tmap)

# Map 1: Distance - Fisher
mp1 <- tm_shape(msoa.spdf) + 
  tm_polygons(
    fill = "dist_centre",
    fill_alpha = 1,
    fill.scale = tm_scale_intervals(
      style = "fisher",
      n = 10,
      values = cols
    ),
    fill.legend = tm_legend(
      title = "Distance",
      hist = FALSE
    )
  ) +
  tm_borders(col = "white", lwd = 0.5, fill_alpha = 0.5) +
  tm_layout(
    frame = FALSE,
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.position = c("right", "bottom"),
    legend.outside = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.8
  ) +
  tm_title_out(
    text = "Dist centre",
    position = tm_pos_out("center", "top", pos.h = "center"),
    size = 1.6
  )

# Map 2: Distance - Quantile
mp2 <- tm_shape(msoa.spdf) + 
  tm_polygons(
    fill = "dist_centre",
    fill_alpha = 1,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 10,
      values = cols
    ),
    fill.legend = tm_legend(
      title = "Distance",
      hist = FALSE
    )
  ) +
  tm_borders(col = "white", lwd = 0.5, fill_alpha = 0.5) +
  tm_layout(
    frame = FALSE,
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.position = c("right", "bottom"),
    legend.outside = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.8
  ) +
  tm_title_out(
    text = "Dist centre",
    position = tm_pos_out("center", "top", pos.h = "center"),
    size = 1.6
  )

# Map 3: Pubs - Fisher
mp3 <- tm_shape(msoa.spdf) + 
  tm_polygons(
    fill = "pubs_count",
    fill_alpha = 1,
    fill.scale = tm_scale_intervals(
      style = "fisher",
      n = 10,
      values = cols
    ),
    fill.legend = tm_legend(
      title = "Count",
      hist = FALSE
    )
  ) +
  tm_borders(col = "white", lwd = 0.5, fill_alpha = 0.5) +
  tm_layout(
    frame = FALSE,
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.position = c("right", "bottom"),
    legend.outside = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.8
  ) +
  tm_title_out(
    text = "Pubs",
    position = tm_pos_out("center", "top", pos.h = "center"),
    size = 1.6
  )

# Map 4: Pubs - Quantile
mp4 <- tm_shape(msoa.spdf) + 
  tm_polygons(
    fill = "pubs_count",
    fill_alpha = 1,
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 10,
      values = cols
    ),
    fill.legend = tm_legend(
      title = "Count",
      hist = FALSE
    )
  ) +
  tm_borders(col = "white", lwd = 0.5, fill_alpha = 0.5) +
  tm_layout(
    frame = FALSE,
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.position = c("right", "bottom"),
    legend.outside = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.8
  ) +
  tm_title_out(
    text = "Pubs",
    position = tm_pos_out("center", "top", pos.h = "center"),
    size = 1.6
  )



tmap_arrange(mp1, mp2, mp3, mp4, ncol = 2, nrow = 2)

