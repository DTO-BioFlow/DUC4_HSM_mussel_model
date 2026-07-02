# ---------------------------------------------------------------------------
# Legacy / unused functions, kept for reference only.
#
# None of these are called by the active pipeline (VSC_CB2_HSM_18.R sources
# only functions_WS.R and functions_S3.R). They reference packages that are
# not installed by the Dockerfile (dplyr, stringr, FNN, geosphere,
# emodnetwfs) and some hardcode a former contributor's local file paths.
#
# Do NOT upload this file to the S3 "scripts/" prefix — the container's
# entrypoint.sh syncs and would otherwise pull it in at runtime for no
# benefit. It is not sourced by VSC_CB2_HSM_18.R.
# ---------------------------------------------------------------------------

# function merge by closest date and chosen distance lat-long --------------------------------------------------------
join_dfs <- function( df1,
                      df2,
                      max_dist){
# merge by dplyr
joined_dfs <- df1 %>%
  left_join(df2, by = c("date"))

#subset to only get same dates
joined_dfs <- subset(joined_dfs, date == date & date == date)
joined_dfs <- joined_dfs[!is.na(joined_dfs$long.x),]
joined_dfs <- joined_dfs[!is.na(joined_dfs$long.y),]

# close distance selection
# finding distance locations
coord_df1 <- joined_dfs[, c("lat.x", "long.x")]
coord_df1$long1 <- coord_df1$long.x * sin(mean(coord_df1$lat.x, na.rm = TRUE)/180*pi)
coord_df2 <- joined_dfs[, c("lat.y", "long.y")]
coord_df2$long1 <- coord_df2$long.y * sin(mean(coord_df2$lat.y, na.rm = TRUE)/180*pi)

matches <- knnx.index(coord_df1, coord_df2, k = 1)
matches

dist <- distHaversine(joined_dfs[, c("long.x", "lat.x")], joined_dfs[, c("long.y", "lat.y")])

# remove observation where distance is too big
short_dist <- which(dist < max_dist)
joined_dfs2 <- joined_dfs[short_dist ,]
joined_dfs2$dist <- dist[short_dist]

return(joined_dfs2)
}


food_for_HSM_terra <- function(folder) { # create this functions to load BPNS maps

  for (i in 1:12) {
    BPNS_sst <- terra::rast(paste0(folder,"BPNS_",i,"_1.tif"))
    BPNS_sss <- terra::rast(paste0(folder,"BPNS_",i,"_2.tif"))
    BPNS_chl <- terra::rast(paste0(folder,"BPNS_",i,"_3.tif"))
    BPNS_oxy <- terra::rast(paste0(folder,"BPNS_",i,"_4.tif"))
    BPNS_orbvel <- terra::rast(paste0(folder,"BPNS_",i,"_5.tif"))
    #BPNS_depth <- terra::rast(paste0(folder,"BPNS_",i,"_6.tif"))
    BPNS_sedrate <- terra::rast(paste0(folder,"BPNS_",i,"_7.tif"))
    crs(BPNS_sedrate) <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"
    BPNS_substrate <- terra::rast(paste0(folder,"BPNS_",i,"_8.tif"))
    crs(BPNS_substrate) <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"
    BPNS_currentvel <- terra::rast(paste0(folder,"BPNS_",i,"_9.tif"))


    BPNS_1 <- list(BPNS_sst,BPNS_sss,BPNS_oxy,BPNS_substrate,BPNS_sedrate,BPNS_currentvel,BPNS_orbvel,BPNS_chl)

    names(BPNS_1) <- c('temp',
                       'sal',
                       'oxy',
                       'substrate',
                       'sedrate',
                       'currentvel',
                       'orbvel',
                       'chl')
    BPNS[[i]] <-  BPNS_1
  }

  return(BPNS)
}

food_for_HSM_wc <- function(folder_wc) { # create this functions to load BPNS maps

  for (i in 1:12) {
    BPNS_sss <- raster(paste0(folder_wc,"BPNS input layers max/BPNS_",i,"_2.tif"))
    BPNS_chl <- raster(paste0(folder_wc,"BPNS input layers min/BPNS_",i,"_3.tif"))
    BPNS_oxy <- raster(paste0(folder_wc,"BPNS input layers min/BPNS_",i,"_4.tif"))
    BPNS_orbvel <- raster(paste0(folder_wc,"BPNS input layers max/BPNS_",i,"_5.tif"))
    BPNS_sedrate <- raster(paste0(folder_wc,"BPNS input layers max/BPNS_",i,"_7.tif"))
    crs(BPNS_sedrate) <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"
    BPNS_substrate <- raster(paste0(folder_wc,"BPNS input layers max/BPNS_",i,"_8.tif"))
    crs(BPNS_substrate) <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"
    BPNS_currentvel <- raster(paste0(folder_wc,"BPNS input layers max/BPNS_",i,"_9.tif"))

    if (i %in% c(1,2,3,10,11,12)){
      BPNS_sst<- raster(paste0(folder_wc,"BPNS input layers min/BPNS_",i,"_1.tif"))
    } else {
      BPNS_sst<- raster(paste0(folder_wc,"BPNS input layers max/BPNS_",i,"_1.tif"))
    }

    BPNS_1 <- addLayer(BPNS_sst,BPNS_sss,BPNS_oxy,BPNS_substrate,BPNS_sedrate,BPNS_currentvel,BPNS_orbvel,BPNS_chl)


    names(BPNS_1) <- c('temp',
                       'sal',
                       'oxy',
                       'substrate',
                       'sedrate',
                       'currentvel',
                       'orbvel',
                       'chl')
    BPNS[[i]] <-  BPNS_1

  }

  return(BPNS)
}

food_for_HSM_wc_WS <- function(folder, lookup, scenario) { # create this functions to load BPNS maps
  BPNS <- list()
  for (i in 1:12) {
    BPNS_1 <- stack()
    for(par in parameters) {
      min_max_median <- lookup %>%
        filter(month == i, str_detect(parameter, par)) %>%
        dplyr::select(all_of(scenario)) %>%
        pull()
      layer_name <- lookup %>%
        filter(month == i, str_detect(parameter, par)) %>%
        dplyr::select(parameter) %>%
        pull()

      lookup_par <- tibble(n = c(1,2,3,4,5,7,8,9,10),
                           l = c("temp","sal","chl","oxy","orbvel","sed","sub","cur","shear"))

      layer_n <- lookup_par$n[which(str_detect(lookup_par$l, par))]

      layer <- paste0(folder,"BPNS input layers ", min_max_median, "/BPNS_", i, "_", layer_n, ".tif")
      r <- raster(layer)
      names(r) <- layer_name

      crs(r) <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"

      BPNS_1 <- stack(BPNS_1,r)
    }

    BPNS[[i]] <-  BPNS_1

  }

  return(BPNS)
}

hsm_calc <- function(df,j) {
  library(raster)
  library(FuzzyR)

  hsm <- calc(df[[j]], function(x) evalfis(x, fuzzy_model[[j]]))

  return(hsm)
}

########################################################################

hsm_calc_year <- function(df,j) {
  library(raster)
  library(FuzzyR)

  # hsm <- calc(df[[j]], function(x) evalfis(x, fuzzy_model_year))
  hsm <- calc(df[[j]], function(x) evalfis_cpp(matrix(x, nrow = 1), fuzzy_model_year))

  return(hsm)
}

####
# using raster and for loop
hsm_calc_year_fuzzyR <- function(df, j) {
  # df: list of 12 raster stacks
  # j: month index
  rstack <- df[[j]]                 # the multi-layer raster (stack/brick)
  n <- ncell(rstack)                # number of cells
  # Extract all layer values: matrix with n rows (cells) and p columns (parameters)
  vals <- getValues(rstack)         # returns matrix if multiple layers
  out <- numeric(n)

  for (k in 1:n) {
    x <- vals[k, ]                  # vector of the cell's parameter values
    out[k] <- evalfis(x, fuzzy_model_year)
    # if(k %% 5000 == 0){
      # print(k)
    # }
  }

  hsm <- raster(rstack)             # template
  hsm <- setValues(hsm, out)        # assign suitability values
  return(hsm)
}

###

##########################################

hsm_calc_validation <- function(df,j) {
  library(raster)
  library(FuzzyR)

  hsm <- calc(df, function(x) evalfis(x, fuzzy_model[[j]]))

  return(hsm)
}

sensitivity_param <- function(df,param) {
  df2 <- df

  for (j in 1:12) {
  df2[[j]][[param]] <- df2[[j]][[param]] * 100
  }

  return(df2)
}


#########################################
##                Data                 ##
#########################################
# Biotic, abiotic & hydrodynamic data (EUROBIS, EMODnet, ...)
# function load all data from computer into R --------------------------------------------------------
get_data_from_my_computer <- function(){
  # OBIS for Mytilus edulis --------------------------------------------------------
  # load data
  OBIS <- read.csv("C:/Users/ward.standaert/Desktop/Mussels/Data/OBIS/OccurrenceNS.csv")

  #select needed columns
  mussel_OBIS_1 <- OBIS %>%
    dplyr::select("decimallongitude",
                  "decimallatitude",
                  "minimumdepthinmeters",
                  "maximumdepthinmeters",
                  "shoredistance",
                  "bathymetry",
                  "sst",
                  "sss",
                  'genus',
                  "eventdate",
                  "year",
                  "date_year",
                  "month",
                  "day"
    )

  # rename columns
  colnames(mussel_OBIS_1) <- c("long",
                               "lat",
                               "depthmin",
                               "depthmax",
                               "shoredistance",
                               "bathymetry",
                               "sst",
                               'sss',
                               "genus",
                               "date",
                               "date_year",
                               "year",
                               "month",
                               "day")

  # set date in correct format
  mussel_OBIS_1$date2 <- mussel_OBIS_1$date
  mussel_OBIS_1$date <- as.Date(substr(mussel_OBIS_1$date,1,10))

  # subset mussel_OBIS to subtidal (depth >= 4m & < 100m) ----------------------
  df <- mussel_OBIS_1 %>%
    subset(bathymetry > 0)
  # create extra column for depth
  df$depth <- NA

  # which depth should be chosen => (1) average min-max, (2) min (when no max), (3) max (when no min), (4) bathymetry (when no min or max)
  min <- which(!is.na(df$depthmin) & is.na(df$depthmax))
  max <- which(!is.na(df$depthmax) & is.na(df$depthmin))
  bath <- which(is.na(df$depth))
  min_max <- which(!is.na(df$depthmin) & !is.na(df$depthmax))

  df$depth[min] <- df$depthmin[min]
  df$depth[max] <- df$depthmax[max]
  df$depth[min_max] <- (df$depthmin[min_max] + df$depthmax[min_max])/2
  df$depth[bath] <- df$bathymetry[bath]

  mussel_OBIS <- df %>%
    subset(depth >= 4 & depth < 100)


  # EMODnet Chemistry: environmental data --------------------------------------------------------
  # load data
  chem <- read.csv("C:/Users/ward.standaert/Desktop/Mussels/Data/EMODnet/environment emodnet chemistry.csv", header= T)

  # select needed columns
  chem_env <- chem %>%
    dplyr::select(yyyy.mm.ddThh.mm.ss.sss,
                  Longitude..degrees_east.,
                  Latitude..degrees_north.,
                  Depth..m.,
                  Water.depth..m.,
                  Water.body.salinity..per.mille.,
                  Water.body.dissolved.oxygen.concentration..umol.l.,
                  Water.body.chlorophyll.a..mg.m.3.,
                  Water.body.dissolved.inorganic.carbon..umol.l.,
                  Water.body.pH..pH.units.,
                  ITS.90.water.temperature..degrees.C.)

  # rename columns
  colnames(chem_env) <- c('date',
                          "long",
                          "lat",
                          "depth",
                          "depth2",
                          "sal",
                          "oxy",
                          "chla",
                          "carb",
                          "pH",
                          "temp")

  # set date in correct format
  chem_env$date2 <- chem_env$date
  chem_env$date <- as.Date(chem_env$date, format = "%Y-%m-%dT%H:%M:%S")

  # correct the long coordinates to -180 - 180
  chem_env$long[which(chem_env$long > 180)] <- chem_env$long[which(chem_env$long > 180)] - 360


  # EMODnet information to download data via Rpackage (doesn't work for physics)--------------------------------------------------------
  #emodnet_wfs()
  # https://github.com/emodnet/emodnetwfs
  # https://www.emodnet-biology.eu/blog/emodnetwfs-access-emodnet-web-feature-service-data-through-r
  # https://emodnet.github.io/EMODnetWFS/articles/ecql_filtering.html


  # EMODnet Geology - sedimentation rate --------------------------------------------------------
  wfs_geo <- emodnet_init_wfs_client(service = "geology_seabed_substrate_maps")
  layers <- c("sedimentation_rates")

  geo_sed_rate <- emodnet_get_layers(wfs = wfs_geo, layers = layers)

  geo_sed_rate_df <- as.data.frame(geo_sed_rate)


  geo_sed_rate_df <- geo_sed_rate_df %>%
    dplyr::select(sedimentation_rates.sea_area,
                  sedimentation_rates.depth,
                  sedimentation_rates.sampling_date,
                  sedimentation_rates.sedimentation_rate,
                  sedimentation_rates.latitude,
                  sedimentation_rates.longitude,
                  sedimentation_rates.substrate)


  geo_sed_rate_df$sedimentation_rates.sampling_date <- paste0(substr(geo_sed_rate_df$sedimentation_rates.sampling_date,1,4),"-",
                                                              substr(geo_sed_rate_df$sedimentation_rates.sampling_date,5,6),"-",
                                                              substr(geo_sed_rate_df$sedimentation_rates.sampling_date,7,8))

  geo_sed_rate_df$sedimentation_rates.sampling_date <- as.Date(geo_sed_rate_df$sedimentation_rates.sampling_date, format = "%Y-%d-%m")
  geo_sed_rate_df <- geo_sed_rate_df[which(!is.na(geo_sed_rate_df$sedimentation_rates.sampling_date)),]

  colnames(geo_sed_rate_df) <- c("area", "depth", "date", "sedimentation_rate", "lat", "long", "substrate" )
  geo_sed_rate_df$date2 <- geo_sed_rate_df$date


  # EMODnet Geology - substrate --------------------------------------------------------
  wfs_geo2 <- emodnet_init_wfs_client(service = "geology_seabed_substrate_maps")
  layers <- c("seabed_substrate_1m")

  geo_substr <- emodnet_get_layers(wfs = wfs_geo2, layers = layers)

  geo_substr_df <- as.data.frame(geo_substr)


  geo_substr_df <- geo_substr_df %>%
    dplyr::select(seabed_substrate_1m.scale,
                  seabed_substrate_1m.original_scale,
                  seabed_substrate_1m.original_grain_size,
                  seabed_substrate_1m.folk_7cl_txt ,
                  seabed_substrate_1m.folk_5cl_txt,
                  seabed_substrate_1m.folk_16cl_txt,
                  seabed_substrate_1m.geom,
                  seabed_substrate_1m.country)

  colnames(geo_substr_df) <- c("scale", "original_scale", "original_grain_size", "folk_7cl_txt", "folk_5cl_txt", "folk_16cl_txt", "geom", "country" )

  # EMODnet Physics - wave height -------------------------------

  wave_height_df <- read.csv("C:/Users/ward.standaert/Desktop/Mussels/Data/EMODnet/wave height.csv", header= T)
  wave_height_df <- wave_height_df %>%
    subset(VTDH_in_m != -99999.990000)

  colnames(wave_height_df) <- c("date", "lat", "long", "wave_heigth")
  wave_height_df$date <- as.Date(wave_height_df$date)


  wave_height_grid <- raster("C:/Users/ward.standaert/Desktop/Mussels/Data/EMODnet/Data/hs100.tif")
  wave_height_grid_df <-na.omit(as.data.frame(wave_height_grid, xy = TRUE))
  colnames(wave_height_grid_df) <- c("long", "lat", "wave height")

  # EMODnet Physics - horizontal current speed -------------------------------
  #hcsp <- read.csv("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Coastbusters 2.0/Data/EMODnet/horizontal current speed.csv", header= T)[-1,]

  # rename columns
  #colnames(hcsp) <- c('date',
  #                    "long",
  #                    "lat",
  #                    "hcsp")

  # set date in correct format
  #hcsp$date2 <- hcsp$date
  #hcsp$date <- as.Date(hcsp$date, format = "%Y-%m-%dT%H:%M:%S")

  # long and lat as numeric
  #hcsp$long <- as.numeric(hcsp$long)
  #hcsp$lat <- as.numeric(hcsp$lat)

  # correct the long coordinates to -180 - 180
  #hcsp$long[which(hcsp$long > 180)] <- hcsp$long[which(hcsp$long > 180)] - 360

  # EMODnet Physics - horizontal current speed -------------------------------
  # only need to load hcsp_15_22_joined as already proceessed to save time.
  #hcsp <- read.csv("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Coastbusters 2.0/Data/EMODnet/horizontal current speed (adjusted).csv", header= T)
  current_sp <- read.csv("C:/Users/ward.standaert/Desktop/Mussels/Data/EMODnet/hcsp_15_22_joined.csv", header= T, sep = ";")

  current_sp$date <- as.Date(current_sp$date, format = "%d/%m/%Y")
  current_sp <- subset(current_sp,hcsp < 100)

  ##############################################################################################################
  #   STEPS TAKEN TO MANIPULATE DATA
  # rename columns
  #colnames(hcsp) <- c('date',
  #                    "lat",
  #                    "long",
  #                    "hcsp")

  # set date in correct format
  #hcsp$date2 <- hcsp$date
  #hcsp$date <- as.Date(hcsp$date, format = "%Y-%m-%dT%H:%M:%S")

  # long and lat as numeric
  #hcsp$long <- as.numeric(hcsp$long)
  #hcsp$lat <- as.numeric(hcsp$lat)

  # remove latitude > 90 & latitude < -90
  #bad_lat <- c(which(hcsp$lat > 90),which(hcsp$lat < -90))
  #hcsp <- hcsp[-bad_lat,]

  # correct the long coordinates to -180 - 180
  #hcsp$long[which(hcsp$long > 180)] <- hcsp$long[which(hcsp$long > 180)] - 360

  # remove "NaN" value for hcsp + hcsp as numeric
  #hcsp <- hcsp[which(hcsp$hcsp != "NaN"),]
  #hcsp$hcsp <- as.numeric(hcsp$hcsp)

  #write.csv(hcsp,"C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Coastbusters 2.0/Data/EMODnet/horizontal current speed (adjusted).csv", row.names = FALSE)


  # return data -------------------------------
  data_list <- list(mussel_OBIS, chem_env, geo_sed_rate_df, geo_substr_df, current_sp, wave_height_grid)
  names(data_list) <- c("mussel_OBIS", "chem_env", "geo_sed_rate_df", "geo_substr_df", "current_sp", "wave_height_grid")

  print("All data retrieved")

  return(data_list)
}

create_subsets_mussels_vs_envir <- function(){
  # create subset per environmental parameter
  # create subtidal (depth >= 2m) mussel occurrence df (with date, long & lat) to combine with other dfs ----------------------
  mussel_occurrence <- mussel_OBIS %>%
    subset(!is.na(date)) %>%
    dplyr::select(date,
                  month,
                  long,
                  lat) %>%
    na.omit()

  # Depth ----------------------
  depth <- mussel_OBIS %>%
    subset(!is.na(depth)) %>%
    dplyr::select(date,
                  month,
                  long,
                  lat,
                  depth) %>%
    na.omit()

  # Temperature ----------------------
  temp <- mussel_OBIS %>%
    subset(!is.na(sst)) %>%
    dplyr::select(date,
                  month,
                  long,
                  lat,
                  sst) %>%
    na.omit()

  # Salinity ----------------------
  sal <- mussel_OBIS %>%
    subset(!is.na(sss)) %>%
    dplyr::select(date,
                  month,
                  long,
                  lat,
                  sss) %>%
    na.omit()

  # NOT RELEVANT FOR BPNS - shore distance ---------------------- to calculate shore distance: https://rdrr.io/github/MikkoVihtakari/PlotSvalbard/man/dist2land.html
  #shoredist <- mussel_OBIS %>%
  #  subset(!is.na(shoredistance) & shoredistance > 0) %>%
  #  dplyr::select(date,
  #         month,
  #         long,
  #         lat,
  #         shoredistance) %>%
  #  na.omit()

  # chlorophyll ----------------------
  chl_1 <- chem_env %>%
    subset(!is.na(chla)) %>%
    dplyr::select(date,
                  long,
                  lat,
                  chla) %>%
    na.omit()

  # merge mussel occurrence df with parameter df based on date and coordinates
  PP <- join_dfs(mussel_occurrence, chl_1, 2000) #df1, df2, max distance between obs (in meters)
  PP <- na.omit(PP)


  # dissolved oxygen ----------------------
  oxy_1 <- chem_env %>%
    subset(!is.na(oxy)) %>%
    dplyr::select(date,
                  long,
                  lat,
                  oxy) %>%
    na.omit()

  # merge mussel occurrence df with parameter df based on date and coordinates
  oxy <- join_dfs(mussel_occurrence, oxy_1, 2000) #df1, df2, max distance between obs (in meters)
  oxy <- na.omit(oxy)

  # sedimentation rate ----------------------
  sed_1 <- geo_sed_rate_df %>%
    subset(!is.na(sedimentation_rate)) %>%
    dplyr::select(date,
                  long,
                  lat,
                  sedimentation_rate) %>%
    na.omit()


  # merge mussel occurrence df with parameter df based on date and coordinates
  sed <- join_dfs(mussel_occurrence, sed_1, 2000) #df1, df2, max distance between obs (in meters)
  sed <- na.omit(sed)



  # wave height ----------------------
  # merge mussel occurrence df with parameter df based on date and coordinates
  mussel_occurrence_00_05 <- mussel_occurrence %>%
    subset(date > "1999-12-31" & date < "2006-01-01")

  mussel_00_05_coord <- as.matrix(mussel_occurrence_00_05[,c("long","lat")])
  wave_height_coord <- data.frame("long" = mussel_occurrence_00_05$long, "lat" = mussel_occurrence_00_05$lat)

  wave_height_coord$wave_height <- extract(wave_height_grid,mussel_00_05_coord)

  wave_height_coord <- na.omit(wave_height_coord)
  wave_height1 <- mutate(wave_height_coord, xy = paste(long,"-", lat))
  wave_height2 <- wave_height1 %>%
    group_by(xy) %>%
    summarise(long = mean(long),
              lat = mean(lat),
              wave_height = mean(wave_height))
  wave_height <- as.data.frame(wave_height2[,c("long","lat","wave_height")])

  # the first df I tried didn't have matches with mussel occurence (df for 2021-2022)
  #wave_height <- join_dfs(mussel_occurrence, wave_height_df, 2000) #df1, df2, max distance between obs (in meters)
  #wave_height <- na.omit(wave_height)

  # horizontal current speed ---------------------- # ( already subset taken, just need to load hcsp_15_22_joined to save time. Otherwise see code below with #)
  current_sp <- current_sp

  #hcsp <- hcsp[which(hcsp$date %in% mussel_occurrence$date),]
  #hcsp <- join_dfs(mussel_occurrence, hcsp, 2000)
  #hcsp <- na.omit(hcsp)
  #hcsp$long <- as.numeric(hcsp$long)
  #hcsp$lat <- as.numeric(hcsp$lat)
  #hcsp$hcsp <- as.numeric(hcsp$hcsp)

  #str(hcsp)

  #hcsp_70_99 <- hcsp %>%
  #  subset(date > "1969-01-01" & date < "2000-01-01") %>%
  #  subset(lat > -90 & lat < 90) %>%
  #  subset(long > -181 & long < 361) %>%
  #  subset(hcsp != -9999.99)

  #hcsp_00_04 <- hcsp %>%
  #  subset(date > "1999-12-31" & date < "2005-01-01") %>%
  #  subset(lat > -90 & lat < 90) %>%
  #  subset(long > -181 & long < 361) %>%
  #  subset(hcsp != -9999.99)

  #hcsp_04_09 <- hcsp %>%
  #  subset(date > "2004-12-31" & date < "2010-01-01") %>%
  #  subset(lat > -90 & lat < 90) %>%
  #  subset(long > -181 & long < 361) %>%
  #  subset(hcsp != -9999.99)

  #hcsp_10_14 <- hcsp %>%
  #  subset(date > "2009-12-31" & date < "2015-01-01") %>%
  #  subset(lat > -90 & lat < 90) %>%
  #  subset(long > -181 & long < 361) %>%
  #  subset(hcsp != -9999.99)

  #hcsp_15_17 <- hcsp %>%
  #  subset(date > "2014-12-31" & date < "2017-01-01") %>%
  #  subset(lat > -90 & lat < 90) %>%
  #  subset(long > -181 & long < 361) %>%
  #  subset(hcsp != -9999.99)
  #hcsp_17_20 <- hcsp %>%
  #  subset(date > "2016-12-31" & date < "2020-01-01") %>%
  #  subset(lat > -90 & lat < 90) %>%
  #  subset(long > -181 & long < 361) %>%
  #  subset(hcsp != -9999.99)
  #hcsp_20_22 <- hcsp %>%
  #  subset(date > "2019-12-31" & date < "2023-01-01") %>%
  #  subset(lat > -90 & lat < 90) %>%
  #  subset(long > -181 & long < 361) %>%
  #  subset(hcsp != -9999.99)

  #hcsp_70_99_joined <- join_dfs(mussel_occurrence, hcsp_70_99, 2000)

  #hcsp_00_04_joined <- join_dfs(mussel_occurrence, hcsp_00_04, 2000)

  #hcsp_04_09_joined <- join_dfs(mussel_occurrence, hcsp_04_09, 2000)

  #hcsp_10_14_joined <- join_dfs(mussel_occurrence, hcsp_10_14, 2000)

  # test las one, other all empty

  #hcsp_15_17_joined <- join_dfs(mussel_occurrence, hcsp_15_17, 2000)
  #hcsp_17_20_joined <- join_dfs(mussel_occurrence, hcsp_17_20, 2000)
  #hcsp_20_22_joined <- join_dfs(mussel_occurrence, hcsp_20_22, 2000)

  #hcsp_joined <- rbind(hcsp_15_17_joined, hcsp_17_20_joined,hcsp_20_22_joined)

  # return all subsets ----------------------
  subsets_list <- list(mussel_occurrence, temp, sal, depth, PP, oxy, wave_height, current_sp, sed)
  names(subsets_list) <- c("mussel_occurrence", "temp", "sal", "depth", "PP", "oxy", "wave_height", "current_sp", "sed")

  print("All data processed and ready to create response curves")

  return(subsets_list)

}
