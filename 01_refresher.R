## ----message = FALSE, warning = FALSE, results = 'hide'--------------------------------------------------------
pkgs <- c("sf", "gstat", "mapview", "nngeo", "rnaturalearth", "dplyr",
          "nomisr", "osmdata", "tidyr", "texreg") 
lapply(pkgs, require, character.only = TRUE)



## --------------------------------------------------------------------------------------------------------------
sessionInfo()



## --------------------------------------------------------------------------------------------------------------
pks <- c("dplyr",
"gstat",
"mapview",
"nngeo",
"nomisr",
"osmdata",
"rnaturalearth",
"sf",
"spatialreg",
"spdep",
"texreg",
"tidyr",
"tmap",
"viridisLite")


## --------------------------------------------------------------------------------------------------------------
# Coordinate pairs of two locations
coords1 <- c(51.752595, -1.262801)
coords2 <- c(51.753237, -1.253904)
coords <- rbind(coords1, coords2)

# Conventional data frame
nuffield.df <- data.frame(name = c("Nuffield College", "Radcliffe Camera"),
                          address = c("New Road", "Radcliffe Sq"),
                          lat = coords[,1], lon = coords[,2])

head(nuffield.df)

# Combine to spatial data frame
nuffield.spdf <- st_as_sf(nuffield.df, 
                          coords = c("lon", "lat"), # Order is important
                          crs = 4326) # EPSG number of CRS

# Map
mapview(nuffield.spdf, zcol = "name")



## --------------------------------------------------------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
st_crs(world)

# Extract a country and plot in current CRS (WGS84)
ger.spdf <- world[world$name == "Germany", ]
plot(st_geometry(ger.spdf))

# Now, let's transform Germany into a CRS optimized for Iceland
ger_rep.spdf <- st_transform(ger.spdf, crs = 5325)
plot(st_geometry(ger_rep.spdf))



## ----cache=TRUE, eval=FALSE------------------------------------------------------------------------------------
# # Create subdir (all data withh be stored in "_data")
# dn <- "_data"
# ifelse(dir.exists(dn), "Exists", dir.create(dn))
# 
# # Download zip file and unzip
# tmpf <- tempfile()
# boundary.link <- "https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip"
# download.file(boundary.link, tmpf)
# unzip(zipfile = tmpf, exdir = paste0(dn))
# unlink(tmpf)


## --------------------------------------------------------------------------------------------------------------
dn <- "_data"
# This is a shapefile
# We only need the MSOA layer for now
msoa.spdf <- st_read(dsn = paste0(dn, "/statistical-gis-boundaries-london/ESRI"),
                     layer = "MSOA_2011_London_gen_MHW" # Note: no file ending
                     )



## ----cache=FALSE-----------------------------------------------------------------------------------------------
head(msoa.spdf)



## ----cache=TRUE, eval=FALSE------------------------------------------------------------------------------------
# # Download file
# ulez.link <- "https://data.london.gov.uk/download/ultra_low_emissions_zone/936d71d8-c5fc-40ad-a392-6bec86413b48/CentralUltraLowEmissionZone.geojson"
# download.file(ulez.link, paste0(dn, "/ulez.json"))


## ----cache=TRUE, eval=TRUE-------------------------------------------------------------------------------------
# Read geo.json
st_layers(paste0(dn, "/ulez.json"))
ulez.spdf <- st_read(dsn = paste0(dn, "/ulez.json")) # here dsn is simply the file
head(ulez.spdf)


## ----mapview---------------------------------------------------------------------------------------------------
mapview(msoa.spdf, zcol = "POPDEN")


## ----cache=TRUE, eval=FALSE------------------------------------------------------------------------------------
# ### For larger request, register and set key
# # Sys.setenv(NOMIS_API_KEY = "XXX")
# # nomis_api_key(check_env = TRUE)
# 
# x <- nomis_data_info()
# 
# # Get London ids
# london_ids <- msoa.spdf$MSOA11CD
# 
# ### Get key statistics ids
# # select requires tables (https://www.nomisweb.co.uk/sources/census_2011_ks)
# # Let's get KS201EW (ethnic group), KS205EW (passport held), and KS402EW (housing tenure)
# 
# # Get internal ids
# stats <- c("KS201EW", "KS402EW", "KS205EW")
# oo <- which(grepl(paste(stats, collapse = "|"), x$name.value))
# ksids <- x$id[oo]
# ksids # This are the internal ids
# 
# 
# ### look at meta information
# q <- nomis_overview(ksids[1])
# head(q)
# a <- nomis_get_metadata(id = ksids[1], concept = "GEOGRAPHY", type = "type")
# a # TYPE297 is MSOA level
# 
# b <- nomis_get_metadata(id = ksids[1], concept = "MEASURES", type = "TYPE297")
# b # 20100 is the measure of absolute numbers
# 
# 
# ### Query data in loop over the required statistics
# for(i in ksids){
# 
#   # Determin if data is divided by sex or urban-rural
#   nd <- nomis_get_metadata(id = i)
#   if("RURAL_URBAN" %in% nd$conceptref){
#     UR <- TRUE
#   }else{
#     UR <- FALSE
#   }
#   if("C_SEX" %in% nd$conceptref){
#     SEX <- TRUE
#   }else{
#     SEX <- FALSE
#   }
# 
#   # make data request
#   if(UR == TRUE){
#     if(SEX == TRUE){
#       tmp_en <- nomis_get_data(id = i, time = "2011",
#                                geography = london_ids, # replace with "TYPE297" for all MSOAs
#                                measures = 20100, RURAL_URBAN = 0, C_SEX = 0)
#     }else{
#       tmp_en <- nomis_get_data(id = i, time = "2011",
#                                geography = london_ids, # replace with "TYPE297" for all MSOAs
#                                measures = 20100, RURAL_URBAN = 0)
#     }
#   }else{
#     if(SEX == TRUE){
#       tmp_en <- nomis_get_data(id = i, time = "2011",
#                                geography = london_ids, # replace with "TYPE297" for all MSOAs
#                                measures = 20100, C_SEX = 0)
#     }else{
#       tmp_en <- nomis_get_data(id = i, time = "2011",
#                                geography = london_ids, # replace with "TYPE297" for all MSOAs
#                                measures = 20100)
#     }
# 
#   }
# 
#   # Append (in case of different regions)
#   ks_tmp <- tmp_en
# 
#   # Make lower case names
#   names(ks_tmp) <- tolower(names(ks_tmp))
#   names(ks_tmp)[names(ks_tmp) == "geography_code"] <- "msoa11"
#   names(ks_tmp)[names(ks_tmp) == "geography_name"] <- "name"
# 
#   # replace weird cell codes
#   onlynum <- which(grepl("^[[:digit:]]+$", ks_tmp$cell_code))
#   if(length(onlynum) != 0){
#     code <- substr(ks_tmp$cell_code[-onlynum][1], 1, 7)
#     if(is.na(code)){
#       code <- i
#     }
#     ks_tmp$cell_code[onlynum] <- paste0(code, "_", ks_tmp$cell_code[onlynum])
#   }
# 
#   # save codebook
#   ks_cb <- unique(ks_tmp[, c("date", "cell_type", "cell", "cell_code", "cell_name")])
# 
#   ### Reshape
#   ks_res <- tidyr::pivot_wider(ks_tmp, id_cols = c("msoa11", "name"),
#                                names_from = "cell_code",
#                                values_from = "obs_value")
# 
#   ### Merge
#   if(i == ksids[1]){
#     census_keystat.df <- ks_res
#     census_keystat_cb.df <- ks_cb
#   }else{
#     census_keystat.df <- merge(census_keystat.df, ks_res, by = c("msoa11", "name"), all = TRUE)
#     census_keystat_cb.df <- rbind(census_keystat_cb.df, ks_cb)
#   }
# 
# }
# 
# 
# # Descriptions are saved in the codebook
# save(census_keystat.df, file = "_data/Census_ckeystat.RData")
# save(census_keystat_cb.df, file = "_data/Census_codebook.RData")


## ----eval=TRUE-------------------------------------------------------------------------------------------------
load("_data/Census_ckeystat.RData")
msoa.spdf <- merge(msoa.spdf, census_keystat.df,
                   by.x = "MSOA11CD", by.y = "msoa11", all.x = TRUE)


## ----eval=FALSE------------------------------------------------------------------------------------------------
# 
# msoa.spdf$per_white <- msoa.spdf$KS201EW_100 / msoa.spdf$KS201EW0001 * 100
# msoa.spdf$per_mixed <- msoa.spdf$KS201EW_200 / msoa.spdf$KS201EW0001 * 100
# msoa.spdf$per_asian <- msoa.spdf$KS201EW_300 / msoa.spdf$KS201EW0001 * 100
# msoa.spdf$per_black <- msoa.spdf$KS201EW_400 / msoa.spdf$KS201EW0001 * 100
# msoa.spdf$per_other <- msoa.spdf$KS201EW_500 / msoa.spdf$KS201EW0001 * 100
# 
# mapview(msoa.spdf, zcol = "per_white")
# 


## ----cache=TRUE, eval=FALSE------------------------------------------------------------------------------------
# # Download
# pol.link <- "https://uk-air.defra.gov.uk/datastore/pcm/mapno22011.csv"
# download.file(pol.link, paste0(dn, "/mapno22011.csv"))


## --------------------------------------------------------------------------------------------------------------
pol.df <- read.csv(paste0(dn, "/mapno22011.csv"), skip = 5, header = T, sep = ",",
                      stringsAsFactors = F, na.strings = "MISSING")

head(pol.df)


## --------------------------------------------------------------------------------------------------------------
# Build spatial object
pol.spdf <- st_as_sf(pol.df, coords = c("x", "y"),
                    crs = 27700)

# we transform the point coordinates into a regular grid with "diameter" 500m
pol.spdf <- st_buffer(pol.spdf, dist = 500, nQuadSegs  = 1,
                      endCapStyle = 'SQUARE')

# Plot NO2
plot(pol.spdf[, "no22011"], border = NA)


## --------------------------------------------------------------------------------------------------------------
# bounding box of where we want to query data
q <- opq(bbox = st_bbox(st_transform(msoa.spdf, 4326)))


## ----eval=FALSE------------------------------------------------------------------------------------------------
# # First build the query of location of pubs in London
# osmq <- add_osm_feature(q, key = "amenity", value = "pub")
# 
# # And then query the data
# pubs.osm <- osmdata_sf(osmq)


## ----eval=FALSE------------------------------------------------------------------------------------------------
# # Make unique points / polygons
# pubs.osm <- unique_osmdata(pubs.osm)
# 
# # Get points and polygons (there are barley any pubs as polygons, so we ignore them)
# pubs.points <- pubs.osm$osm_points
# pubs.polys <- pubs.osm$osm_multipolygons
# 
# # # Drop OSM file
# # rm(pubs.osm); gc()
# 
# # Reduce to point object only
# pubs.spdf <- pubs.points
# 
# # Reduce to a few variables
# pubs.spdf <- pubs.spdf[, c("osm_id", "name", "addr:postcode", "diet:vegan")]


## --------------------------------------------------------------------------------------------------------------
# Reload (as I don't run the above here)
load("_data/osm_d.RData")


## --------------------------------------------------------------------------------------------------------------
mapview(st_geometry(pubs.spdf))


## --------------------------------------------------------------------------------------------------------------
save(msoa.spdf, file = "_data/msoa_spatial.RData")
save(ulez.spdf, file = "_data/ulez_spatial.RData")
save(pol.spdf, file = "_data/pollution_spatial.RData")
save(pubs.spdf, file = "_data/pubs_spatial.RData")

