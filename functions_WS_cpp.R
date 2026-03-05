#########################################
##              Functions              ##
#########################################
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


fun9999 <- function(x) { x[is.na(x)] <- -9999; return(x)} # function to change NA to -9999 to work in fuzzy logic

food_for_HSM <- function(folder) { # create this functions to load BPNS maps
  
  for (i in 1:12) {
    BPNS_sst <- raster(paste0(folder,"BPNS_",i,"_1.tif"))
    BPNS_sss <- raster(paste0(folder,"BPNS_",i,"_2.tif"))
    BPNS_chl <- raster(paste0(folder,"BPNS_",i,"_3.tif"))
    BPNS_oxy <- raster(paste0(folder,"BPNS_",i,"_4.tif"))
    BPNS_orbvel <- raster(paste0(folder,"BPNS_",i,"_5.tif"))
    #BPNS_depth <- raster(paste0(folder,"BPNS_",i,"_6.tif"))
    BPNS_sedrate <- raster(paste0(folder,"BPNS_",i,"_7.tif"))
    crs(BPNS_sedrate) <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"
    BPNS_substrate <- raster(paste0(folder,"BPNS_",i,"_8.tif"))
    crs(BPNS_substrate) <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"
    BPNS_currentvel <- raster(paste0(folder,"BPNS_",i,"_9.tif"))
    BPNS_shear <- raster(paste0(folder,"BPNS_",i,"_10.tif"))
    crs(BPNS_shear) <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"
    
    BPNS_1 <- addLayer(BPNS_sst,BPNS_sss,BPNS_oxy,BPNS_substrate,BPNS_sedrate,BPNS_currentvel,BPNS_orbvel,BPNS_chl,BPNS_shear)
    
    names(BPNS_1) <- c('temp',
                       'sal',
                       'oxy',
                       'substrate',
                       'sedrate',
                       'currentvel',
                       'orbvel',
                       'chl',
                       'shear')
    BPNS[[i]] <-  BPNS_1
  }
  
  return(BPNS)
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

hsm_calc_year_cpp <- function(df, j, out_disc = 201) {
  # df: list of 12 raster stacks
  # j: month index
  rstack <- df[[j]]                 # the multi-layer raster (stack/brick)
  n <- ncell(rstack)                # number of cells
  # Extract all layer values: matrix with n rows (cells) and p columns (parameters)
  vals <- getValues(rstack)         # returns matrix if multiple layers
  out <- numeric(n)
  
  for (k in 1:n) {
    x <- vals[k, ]                  # vector of the cell's parameter values
    out[k] <- evalfis_cpp(matrix(x, nrow = 1), fuzzy_model_year, out_disc)
    # if(k %% 5000 == 0){
    #   print(k)
    # }
  }
  
  hsm <- raster(rstack)             # template
  hsm <- setValues(hsm, out)        # assign suitability values
  return(hsm)
}

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

#########################################
##            Fuzzy logic              ##
#########################################
# Fuzzy R
# Create fuzzy logic model
build_fuzzy_logic_model <- function(params, spec_rules) {
  # create list to store monthly fis
  fis_list <- NULL
  
  # to list by month
  month <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  
  # loop to get monthly fis and fuzzy logic and plots
  for (i in c(1:12)){
    # Create a fis (Fuzzy inference system)
    musselbed <- NULL # start fresh
    
    musselbed <- newfis(
      paste0('musselbed_',month[i]),
      fisType = "mamdani", #sugeno uses average weight
      mfType = "t1",
      andMethod = "prod",
      orMethod = "max",
      impMethod = "min",
      aggMethod = "max",
      defuzzMethod = "centroid"
    )
  
    #######################
    # Add input variables #
    #######################
    # 1.  Temperature -------------------------------------------------------------------------------------------------------------------------
    if ("temp" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "temperature",
      c(-10:40),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'optimal', 'trapmf', rc_list[[i]]$sst$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'low', 'trapmf', c(-10,-10,rc_list[[i]]$sst$q[1],rc_list[[i]]$sst$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'high', 'trapmf', c(rc_list[[i]]$sst$q[3],rc_list[[i]]$sst$q[4],40,40))
    }
  
    
    # 2.  Salinity -------------------------------------------------------------------------------------------------------------------------
    if ("sal" %in% params){
      musselbed <- addvar(
      musselbed,
      'input', #input or output
      "salinity",
      c(0:45),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'optimal', 'trapmf', rc_list[[i]]$sss$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'low', 'trapmf', c(0,0,rc_list[[i]]$sss$q[1],rc_list[[i]]$sss$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'high', 'trapmf', c(rc_list[[i]]$sss$q[3],rc_list[[i]]$sss$q[4],45,45))
    }
    
    # 3.  Dissolved Oxygen concentration ---> NOT ENOUGH DATA for monthly -------------------------------------------------------------------------------------------------------------------------
    if ("oxy" %in% params){
      musselbed <- addvar(
      musselbed,
      'input', #input or output
      "Oxy",
      c(0:50),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'optimal', 'trapmf', rc_list[[i]]$oxy$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'low', 'trapmf', c(0,0,rc_list[[i]]$oxy$q[1],rc_list[[i]]$oxy$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'much', 'trapmf', c(rc_list[[i]]$oxy$q[3],rc_list[[i]]$oxy$q[4],50,50))
    }
    
    # 4.  Substrate -------------------------------------------------------------------------------------------------------------------------
    if ("sub" %in% params){
      musselbed <- addvar(
      musselbed,
      'input', #input or output
      "substrate",
      c(0:200), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'optimal', 'trimf', rc_list[[i]]$substrate$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'low', 'trapmf', c(0,0,rc_list[[i]]$substrate$q[1],rc_list[[i]]$substrate$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'high', 'trapmf', c(rc_list[[i]]$substrate$q[2],rc_list[[i]]$substrate$q[3],200,200))
    }
    
    # 5.  Sedimentation rate -------------------------------------------------------------------------------------------------------------------------
    if ("sed" %in% params){
      musselbed <- addvar(
      musselbed,
      'input', #input or output
      "sedimentation",
      c(-2:2), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'optimal', 'trimf', rc_list[[i]]$sedimentation$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'low', 'trapmf', c(-2,-2,rc_list[[i]]$sedimentation$q[1],rc_list[[i]]$sedimentation$q[2])) # to adjust
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'high', 'trapmf', c(rc_list[[i]]$sedimentation$q[2],rc_list[[i]]$sedimentation$q[3],2,2)) # to adjust
    }
    
    # 6.  Current speed -------------------------------------------------------------------------------------------------------------------------
    if ("cur" %in% params){
      musselbed <- addvar(
      musselbed,
      'input', #input or output
      "current speed",
      c(0:5), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'optimal', 'trapmf', rc_list[[i]]$current_speed$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'low', 'trapmf', c(0,0,rc_list[[i]]$current_speed$q[1],rc_list[[i]]$current_speed$q[2])) # to adjust
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'high', 'trapmf', c(rc_list[[i]]$current_speed$q[3],rc_list[[i]]$current_speed$q[4],5,5)) # to adjust
    }
    
    # 7.  Orbital velocity -------------------------------------------------------------------------------------------------------------------------
    if ("orb" %in% params){
      musselbed <- addvar(
      musselbed,
      'input', #input or output
      "orbital velocity",
      c(0:5), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'optimal', 'trimf', rc_list[[i]]$orb_vel$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'low', 'trapmf', c(0,0,rc_list[[i]]$orb_vel$q[1],rc_list[[i]]$orb_vel$q[2])) # to adjust
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'high', 'trapmf', c(rc_list[[i]]$orb_vel$q[2],rc_list[[i]]$orb_vel$q[3],5,5)) # to adjust
    }
    
    # 8. Primary Production (PP) -------------------------------------------------------------------------------------------------------------------------
    if ("chl" %in% params){
      musselbed <- addvar(
      musselbed,
      'input', #input or output
      "PP",
      c(0:60),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'optimal', 'trapmf', rc_list[[i]]$PP$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'low', 'trapmf', c(0,0,rc_list[[i]]$PP$q[1],rc_list[[i]]$PP$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'high', 'trapmf', c(rc_list[[i]]$PP$q[3],rc_list[[i]]$PP$q[4],60,60))
    }
    
    # 9.  Shear stress -------------------------------------------------------------------------------------------------------------------------
    if ("shear" %in% params){
      musselbed <- addvar(
        musselbed,
        'input', #input or output
        "shear stress",
        c(0:1), # to adjust
        method = NULL,
        params = NULL,
        firing.method = "tnorm.min.max"
      )
      # Add membership function (mf)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'optimal', 'trimf', rc_list[[i]]$shear$q) # N/m² : from Smile Consult in German Bight
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'low', 'trapmf', c(0,0,rc_list[[i]]$shear$q[1],rc_list[[i]]$shear$q[2])) # to adjust
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'high', 'trapmf', c(rc_list[[i]]$shear$q[2],rc_list[[i]]$shear$q[3],5,5)) # to adjust
    }
    
    
    ########################
    # Add output variables #
    ########################
    # 1.  Suitability -------------------------------------------------------------------------------------------------------------------------
    musselbed <- addvar(
      musselbed,
      'output', #input or output
      "Suitability",
      c(0:100),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'output', 1, 'optimal', 'trapmf', c(80,85,100,100))
    musselbed <- addmf(musselbed, 'output', 1, 'good', 'trapmf', c(65,70,80,85))
    musselbed <- addmf(musselbed, 'output', 1, 'okay', 'trapmf', c(25,50,65,70))
    musselbed <- addmf(musselbed, 'output', 1, 'bad', 'trapmf', c(0,0,25,50))
    
    ###################
    # Add fuzzy rules #
    ###################
    # load monthly fuzzy rules -------------------------------------------------------------------------------------------------------------------------
    rulelist <- expand.grid(c(rep(list(c(1:3)), length(params)))) #use combinations to have all possible scenario's
    rulelist <- as.data.frame(sapply(rulelist, function(x) as.numeric(x)))
    rulelist$response <- NA # add response column
    rulelist$weight <- NA # add response column
    rulelist$and_or <- NA # add AND/OR column
    
    rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) <  ((length(params)/100)*50))] <- 4
    rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*50) & rowSums(rulelist[,c(1:(length(params)-1))] == 1) < ((length(params)/100)*70))] <- 3
    rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*70) & rowSums(rulelist[,c(1:(length(params)-1))] == 1) < ((length(params)/100)*90))] <- 2
    rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*90))] <- 1
    rulelist$weight <- 0.5 # weight for rule
    rulelist$and_or <- 1 # provide AND/OR column
    
    # add extra specific rules
    if (!is.null(spec_rules[[i]])){
    # remove unnecessary rules
     for (j in 1:length(spec_rules[[i]])){
      p <- which(spec_rules[[i]][[j]][1:length(params)] != 0) 
      rulelist <- rulelist[-which(rulelist[,p] == spec_rules[[i]][[j]][p]),]
     }   
    extra_rules <-t(as.data.frame(specif_rules_month[[i]]))
    colnames(extra_rules) <-  colnames(rulelist)
    rulelist2 <- rbind(rulelist,extra_rules)
    } else {
      rulelist2 <- rulelist
    }


    
    musselbed <- addrule(musselbed, as.matrix(rulelist2)) # add rules to fis
    
    ###########################################
    # Store monthly fuzzy logic model in list #
    ###########################################
    # store monthly in list -------------------------------------------------------------------------------------------------------------------------
    fis_list[[month[i]]] <- musselbed
  }
  return(fis_list)
}

build_fuzzy_logic_model_starfish <- function(params, spec_rules) {
  # create list to store monthly fis
  fis_list <- NULL
  
  # to list by month
  month <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  
  # loop to get monthly fis and fuzzy logic and plots
  for (i in c(1:12)){
    # Create a fis (Fuzzy inference system)
    musselbed <- NULL # start fresh
    
    musselbed <- newfis(
      paste0('musselbed_',month[i]),
      fisType = "mamdani", #sugeno uses average weight
      mfType = "t1",
      andMethod = "prod",
      orMethod = "max",
      impMethod = "min",
      aggMethod = "max",
      defuzzMethod = "centroid"
    )
    
    #######################
    # Add input variables #
    #######################
    # 1.  Temperature -------------------------------------------------------------------------------------------------------------------------
    if ("temp" %in% params){
      musselbed <- addvar(
        musselbed,
        'input', #input or output
        "temperature",
        c(-10:40),
        method = NULL,
        params = NULL,
        firing.method = "tnorm.min.max"
      )
      # Add membership function (mf)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'optimal', 'trapmf', rc_list[[i]]$sst)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'low', 'trapmf', c(-10,-10,rc_list[[i]]$sst[1],rc_list[[i]]$sst[2]))
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'high', 'trapmf', c(rc_list[[i]]$sst[3],rc_list[[i]]$sst[4],40,40))
    }
    
    
    # 2.  Salinity -------------------------------------------------------------------------------------------------------------------------
    if ("sal" %in% params){
      musselbed <- addvar(
        musselbed,
        'input', #input or output
        "salinity",
        c(0:45),
        method = NULL,
        params = NULL,
        firing.method = "tnorm.min.max"
      )
      # Add membership function (mf)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'optimal', 'trapmf', c(20,22.8,26,31)) # troost 2022 and rubel 2020
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'low', 'trapmf', c(0,0,20,22.8))
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'high', 'trapmf', c(26,31,45,45))
    }
    
    # 3.  Dissolved Oxygen concentration ---> NOT ENOUGH DATA for monthly -------------------------------------------------------------------------------------------------------------------------
    if ("oxy" %in% params){
      musselbed <- addvar(
        musselbed,
        'input', #input or output
        "Oxy",
        c(0:50),
        method = NULL,
        params = NULL,
        firing.method = "tnorm.min.max"
      )
      # Add membership function (mf)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'optimal', 'trapmf', rc_list[[i]]$oxy)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'low', 'trapmf', c(0,0,rc_list[[i]]$oxy[1],rc_list[[i]]$oxy[2]))
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'much', 'trapmf', c(rc_list[[i]]$oxy[3],rc_list[[i]]$oxy[4],50,50))
    }
    
    # 4.  Substrate -------------------------------------------------------------------------------------------------------------------------
    if ("sub" %in% params){
      musselbed <- addvar(
        musselbed,
        'input', #input or output
        "substrate",
        c(0:200), # to adjust
        method = NULL,
        params = NULL,
        firing.method = "tnorm.min.max"
      )
      # Add membership function (mf)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'optimal', 'trapmf', rc_list[[i]]$substrate)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'low', 'trapmf', c(0,0,rc_list[[i]]$substrate[1],rc_list[[i]]$substrate[2]))
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'high', 'trapmf', c(rc_list[[i]]$substrate[3],rc_list[[i]]$substrate[4],200,200))
    }
    
    # 5.  Sedimentation rate -------------------------------------------------------------------------------------------------------------------------
    if ("sed" %in% params){
      musselbed <- addvar(
        musselbed,
        'input', #input or output
        "sedimentation",
        c(-2:2), # to adjust
        method = NULL,
        params = NULL,
        firing.method = "tnorm.min.max"
      )
      # Add membership function (mf)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'optimal', 'trapmf', rc_list[[i]]$sedimentation)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'low', 'trapmf', c(-2,0,rc_list[[i]]$sedimentation[1],rc_list[[i]]$sedimentation[2])) # to adjust
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'high', 'trapmf', c(rc_list[[i]]$sedimentation[3],rc_list[[i]]$sedimentation[4],2,2)) # to adjust
    }
    
    # 6.  Shear stress -------------------------------------------------------------------------------------------------------------------------
    if ("shear" %in% params){
      musselbed <- addvar(
        musselbed,
        'input', #input or output
        "shear stress",
        c(0:1), # to adjust
        method = NULL,
        params = NULL,
        firing.method = "tnorm.min.max"
      )
      # Add membership function (mf)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'optimal', 'trapmf', rc_list[[i]]$shear) # N/m² : from Smile Consult in German Bight
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'low', 'trapmf', c(0,0,rc_list[[i]]$shear[1],rc_list[[i]]$shear[2])) # to adjust
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'high', 'trapmf', c(rc_list[[i]]$shear[3],rc_list[[i]]$shear[4],5,5)) # to adjust
    }
    # 7.  Current speed -------------------------------------------------------------------------------------------------------------------------
    if ("cur" %in% params){
      musselbed <- addvar(
        musselbed,
        'input', #input or output
        "current speed",
        c(0:5), # to adjust
        method = NULL,
        params = NULL,
        firing.method = "tnorm.min.max"
      )
      # Add membership function (mf)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'optimal', 'trapmf', rc_list[[i]]$current_speed)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'low', 'trapmf', c(0,0,rc_list[[i]]$current_speed[1],rc_list[[i]]$current_speed[2])) # to adjust
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'high', 'trapmf', c(rc_list[[i]]$current_speed[3],rc_list[[i]]$current_speed[4],5,5)) # to adjust
    }
    
    # 8.  Orbital velocity -------------------------------------------------------------------------------------------------------------------------
    if ("orb" %in% params){
      musselbed <- addvar(
        musselbed,
        'input', #input or output
        "orbital velocity",
        c(0:5), # to adjust
        method = NULL,
        params = NULL,
        firing.method = "tnorm.min.max"
      )
      # Add membership function (mf)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'optimal', 'trapmf', rc_list[[i]]$orb_vel)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'low', 'trapmf', c(0,0,rc_list[[i]]$orb_vel[1],rc_list[[i]]$orb_vel[2])) # to adjust
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'high', 'trapmf', c(rc_list[[i]]$orb_vel[3],rc_list[[i]]$orb_vel[4],5,5)) # to adjust
    }
    
    # 9. Primary Production (PP) -------------------------------------------------------------------------------------------------------------------------
    if ("chl" %in% params){
      musselbed <- addvar(
        musselbed,
        'input', #input or output
        "PP",
        c(0:60),
        method = NULL,
        params = NULL,
        firing.method = "tnorm.min.max"
      )
      # Add membership function (mf)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'optimal', 'trapmf', rc_list[[i]]$PP)
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'low', 'trapmf', c(0,0,rc_list[[i]]$PP[1],rc_list[[i]]$PP[2]))
      musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'high', 'trapmf', c(rc_list[[i]]$PP[3],rc_list[[i]]$PP[4],60,60))
    }
    
    
    
    ########################
    # Add output variables #
    ########################
    # 1.  Suitability -------------------------------------------------------------------------------------------------------------------------
    musselbed <- addvar(
      musselbed,
      'output', #input or output
      "Suitability",
      c(0:100),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'output', 1, 'optimal', 'trapmf', c(80,85,100,100))
    musselbed <- addmf(musselbed, 'output', 1, 'good', 'trapmf', c(65,70,80,85))
    musselbed <- addmf(musselbed, 'output', 1, 'okay', 'trapmf', c(25,50,65,70))
    musselbed <- addmf(musselbed, 'output', 1, 'bad', 'trapmf', c(0,0,25,50))
    
    ###################
    # Add fuzzy rules #
    ###################
    # load monthly fuzzy rules -------------------------------------------------------------------------------------------------------------------------
    rulelist <- expand.grid(c(rep(list(c(1:3)), length(params)))) #use combinations to have all possible scenario's
    rulelist <- as.data.frame(sapply(rulelist, function(x) as.numeric(x)))
    rulelist$response <- NA # add response column
    rulelist$weight <- NA # add response column
    rulelist$and_or <- NA # add AND/OR column
    
    rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) <  ((length(params)/100)*50))] <- 4
    rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*50) & rowSums(rulelist[,c(1:(length(params)-1))] == 1) < ((length(params)/100)*70))] <- 3
    rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*70) & rowSums(rulelist[,c(1:(length(params)-1))] == 1) < ((length(params)/100)*90))] <- 2
    rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*90))] <- 1
    rulelist$weight <- 0.5 # weight for rule
    rulelist$and_or <- 1 # provide AND/OR column
    
    # add extra specific rules
    if (!is.null(specif_rules_month[[i]])){
      # remove unnecessary rules
      for (j in 1:length(spec_rules[[i]])){
        p <- which(spec_rules[[i]][[j]][1:length(params)] != 0) 
        rulelist <- rulelist[-which(rulelist[,p] == spec_rules[[i]][[j]][p]),]
      }   
      extra_rules <-t(as.data.frame(specif_rules_month[[i]]))
      colnames(extra_rules) <-  colnames(rulelist)
      rulelist2 <- rbind(rulelist,extra_rules)
    } else {
      rulelist2 <- rulelist
    }
    
    
    
    musselbed <- addrule(musselbed, as.matrix(rulelist2)) # add rules to fis
    
    ###########################################
    # Store monthly fuzzy logic model in list #
    ###########################################
    # store monthly in list -------------------------------------------------------------------------------------------------------------------------
    fis_list[[month[i]]] <- musselbed
  }
  return(fis_list)
}

build_fuzzy_logic_model_yearrc <- function(params, spec_rules) {
  # create list to store monthly fis
  fis_list <- NULL
  
  # to list by month
  month <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  
  # Create a fis (Fuzzy inference system)
  musselbed <- NULL # start fresh
  
  musselbed <- newfis(
    'musselbed_',
    fisType = "mamdani", #sugeno uses average weight
    mfType = "t1",
    andMethod = "prod",
    orMethod = "max",
    impMethod = "min",
    aggMethod = "max",
    defuzzMethod = "centroid"
  )
  
  #######################
  # Add input variables #
  #######################
  # 1.  Temperature -------------------------------------------------------------------------------------------------------------------------
  if ("temp" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "temperature",
      c(-10:40),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'optimal', 'trapmf', rc_list$sst$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'low', 'trapmf', c(-10,-10,rc_list$sst$q[1],rc_list$sst$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'high', 'trapmf', c(rc_list$sst$q[3],rc_list$sst$q[4],40,40))
  }
  
  
  # 2.  Salinity -------------------------------------------------------------------------------------------------------------------------
  if ("sal" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "salinity",
      c(0:45),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'optimal', 'trapmf', rc_list$sss$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'low', 'trapmf', c(0,0,rc_list$sss$q[1],rc_list$sss$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'high', 'trapmf', c(rc_list$sss$q[3],rc_list$sss$q[4],45,45))
  }
  
  # 3.  Dissolved Oxygen concentration ---> NOT ENOUGH DATA for monthly -------------------------------------------------------------------------------------------------------------------------
  if ("oxy" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "Oxy",
      c(0:50),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'optimal', 'trapmf', rc_list$oxy$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'low', 'trapmf', c(0,0,rc_list$oxy$q[1],rc_list$oxy$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'high', 'trapmf', c(rc_list$oxy$q[3],rc_list$oxy$q[4],50,50))
  }
  
  # 4.  Substrate -------------------------------------------------------------------------------------------------------------------------
  if ("sub" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "substrate",
      c(0:200), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'optimal', 'trimf', rc_list$substrate$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'low', 'trapmf', c(0,0,rc_list$substrate$q[1],rc_list$substrate$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'high', 'trapmf', c(rc_list$substrate$q[2],rc_list$substrate$q[3],200,200))
  }
  
  # 5.  Sedimentation rate -------------------------------------------------------------------------------------------------------------------------
  if ("sed" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "sedimentation",
      c(-2:2), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'optimal', 'trimf', rc_list$sedimentation$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'low', 'trapmf', c(-2,-2,rc_list$sedimentation$q[1],rc_list$sedimentation$q[2])) # to adjust
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'high', 'trapmf', c(rc_list$sedimentation$q[2],rc_list$sedimentation$q[3],2,2)) # to adjust
  }
  
  # 6.  Current speed -------------------------------------------------------------------------------------------------------------------------
  if ("cur" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "current speed",
      c(0:5), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'optimal', 'trapmf', rc_list$current_speed$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'low', 'trapmf', c(0,0,rc_list$current_speed$q[1],rc_list$current_speed$q[2])) # to adjust
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'high', 'trapmf', c(rc_list$current_speed$q[3],rc_list$current_speed$q[4],5,5)) # to adjust
  }
  
  # 7.  Orbital velocity -------------------------------------------------------------------------------------------------------------------------
  if ("orb" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "orbital velocity",
      c(0:5), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'optimal', 'trimf', rc_list$orb_vel$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'low', 'trapmf', c(0,0,rc_list$orb_vel$q[1],rc_list$orb_vel$q[2])) # to adjust
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'high', 'trapmf', c(rc_list$orb_vel$q[2],rc_list$orb_vel$q[3],5,5)) # to adjust
  }
  
  # 8. Primary Production (PP) -------------------------------------------------------------------------------------------------------------------------
  if ("chl" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "PP",
      c(0:60),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'optimal', 'trapmf', rc_list$PP$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'low', 'trapmf', c(0,0,rc_list$PP$q[1],rc_list$PP$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'high', 'trapmf', c(rc_list$PP$q[3],rc_list$PP$q[4],60,60))
  }
  
  
  # 9.  Shear stress -------------------------------------------------------------------------------------------------------------------------
  if ("shear" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "shear stress",
      c(0:5), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'optimal', 'trimf', rc_list$shear$q) # N/m² : from Smile Consult in German Bight
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'low', 'trapmf', c(0,0,rc_list$shear$q[1],rc_list$shear$q[2])) # to adjust
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'high', 'trapmf', c(rc_list$shear$q[2],rc_list$shear$q[3],5,5)) # to adjust
  }
  
  
  ########################
  # Add output variables #
  ########################
  # 1.  Suitability -------------------------------------------------------------------------------------------------------------------------
  musselbed <- addvar(
    musselbed,
    'output', #input or output
    "Suitability",
    c(0:100),
    method = NULL,
    params = NULL,
    firing.method = "tnorm.min.max"
  )
  # Add membership function (mf)
  musselbed <- addmf(musselbed, 'output', 1, 'optimal', 'trapmf', c(80,85,100,100))
  musselbed <- addmf(musselbed, 'output', 1, 'good', 'trapmf', c(65,70,80,85))
  musselbed <- addmf(musselbed, 'output', 1, 'okay', 'trapmf', c(25,50,65,70))
  musselbed <- addmf(musselbed, 'output', 1, 'bad', 'trapmf', c(0,0,25,50))
  
  ###################
  # Add fuzzy rules #
  ###################
  # load monthly fuzzy rules -------------------------------------------------------------------------------------------------------------------------
  rulelist <- expand.grid(c(rep(list(c(1:3)), length(params)))) #use combinations to have all possible scenario's
  rulelist <- as.data.frame(sapply(rulelist, function(x) as.numeric(x)))
  rulelist$response <- NA # add response column
  rulelist$weight <- NA # add response column
  rulelist$and_or <- NA # add AND/OR column
  
  rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) <  ((length(params)/100)*50))] <- 4
  rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*50) & rowSums(rulelist[,c(1:(length(params)-1))] == 1) < ((length(params)/100)*70))] <- 3
  rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*70) & rowSums(rulelist[,c(1:(length(params)-1))] == 1) < ((length(params)/100)*90))] <- 2
  rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*90))] <- 1
  rulelist$weight <- 0.5 # weight for rule
  rulelist$and_or <- 1 # provide AND/OR column
  
  if (!is.null(spec_rules)){
    # remove unnecessary rules
    for (j in 1:length(spec_rules)){
      p <- which(spec_rules[[j]][1:length(params)] != 0) 
      rulelist <- rulelist[-which(rulelist[,p] == spec_rules[[j]][p]),]
    }   
    extra_rules <-t(as.data.frame(specif_rules_month))
    colnames(extra_rules) <-  colnames(rulelist)
    rulelist2 <- rbind(rulelist,extra_rules)
  } else {
    rulelist2 <- rulelist
  }
  musselbed <- addrule(musselbed, as.matrix(rulelist2)) # add rules to fis
  
  fis_list <- musselbed
  return(fis_list)
}


# install.packages("Rcpp")  # if needed
library(Rcpp)

cppFunction('
#include <Rcpp.h>
using namespace Rcpp;

// --- membership function evaluator ---
double mf_eval(double x, const std::string & type, const NumericVector & p) {
  if (type == "trimf") {
    double a = p[0], b = p[1], c = p[2];
    if (x <= a || x >= c) return 0.0;
    if (x == b) return 1.0;
    if (x < b) return (x - a) / (b - a);
    return (c - x) / (c - b);
  }
  else if (type == "trapmf") {
    double a = p[0], b = p[1], c = p[2], d = p[3];
    if (x <= a || x >= d) return 0.0;
    if (x >= b && x <= c) return 1.0;
    if (x < b) return (x - a) / (b - a);
    return (d - x) / (d - c);
  }
  else if (type == "gaussmf") {
    double sigma = p[0], c0 = p[1];
    if (sigma <= 0) return 0.0;
    double arg = (x - c0) / sigma;
    return std::exp(-0.5 * arg * arg);
  }
  else if (type == "gbellmf") {
    double a = p[0], b = p[1], c0 = p[2];
    if (a == 0) return 0.0;
    double arg = std::abs((x - c0) / a);
    return 1.0 / (1.0 + std::pow(arg, 2.0 * b));
  }
  else {
    stop("Unsupported MF type: " + type);
  }
}

// [[Rcpp::export]]
NumericVector evalfis_cpp(NumericMatrix input, List fis, int out_disc = 201) {
  int n = input.nrow();
  if (n == 0) return NumericVector(0);
  int nin = input.ncol();

  // parse FIS top-level settings
  std::string andMethod = as<std::string>(fis["andMethod"]);
  std::string impMethod = as<std::string>(fis["impMethod"]);
  std::string aggMethod = as<std::string>(fis["aggMethod"]);
  std::string defuzzMethod = as<std::string>(fis["defuzzMethod"]);
  if (defuzzMethod.size() == 0) defuzzMethod = as<std::string>(fis["defuzzMethod"]); // fallback

  List inputs = fis["input"];
  List outputs = fis["output"];
  NumericMatrix rules = as<NumericMatrix>(fis["rule"]);
  int nrules = rules.nrow();
  int ncols_rule = rules.ncol();

  // Determine rule column layout:
  // assume antecedents in cols 0..nin-1, consequent at col nin (1-based in R),
  // optional weight at col nin+1, optional connection at nin+2
  int consequent_col = nin;      // corresponds to R column nin+1
  int weight_col = (ncols_rule > nin+1) ? nin+1 : -1;
  // (connection column ignored: we use fis$andMethod globally)

  // --- parse input MFs into C++ structures to avoid repeated R lookups ---
  int nin_vars = inputs.size();
  if (nin_vars != nin) {
    // structure mismatch
    stop("Number of input columns does not match fis$input length");
  }

  // For each input variable, store MF types and parameters
  std::vector< std::vector<std::string> > in_mf_types(nin);
  std::vector< std::vector< NumericVector > > in_mf_params(nin);

  for (int i = 0; i < nin; i++) {
    List invar = inputs[i];
    List mf_list = invar["mf"];
    int nmf = mf_list.size();
    in_mf_types[i].reserve(nmf);
    in_mf_params[i].reserve(nmf);
    for (int m = 0; m < nmf; m++) {
      List mf = mf_list[m];
      std::string t = as<std::string>(mf["type"]);
      NumericVector params = as<NumericVector>(mf["params"]);
      in_mf_types[i].push_back(t);
      in_mf_params[i].push_back(params);
    }
  }

  // --- parse output MFs (assume single output variable as your structure shows) ---
  List outvar = outputs[0];
  NumericVector out_range = as<NumericVector>(outvar["range"]);
  double out_min = out_range[0];
  double out_max = out_range[1];
  List out_mf_list = outvar["mf"];
  int noutMF = out_mf_list.size();
  std::vector<std::string> out_mf_types(noutMF);
  std::vector<NumericVector> out_mf_params(noutMF);
  for (int m = 0; m < noutMF; m++) {
    List mf = out_mf_list[m];
    out_mf_types[m] = as<std::string>(mf["type"]);
    out_mf_params[m] = as<NumericVector>(mf["params"]);
  }

  // precompute discretization for centroid
  if (out_disc < 5) out_disc = 5;
  std::vector<double> xs(out_disc);
  double step = (out_max - out_min) / double(out_disc - 1);
  for (int i = 0; i < out_disc; i++) xs[i] = out_min + i * step;

  NumericVector result(n, NA_REAL);

  // For each observation / raster cell
  for (int irow = 0; irow < n; irow++) {
    // aggregated output MF degrees (after agg across rules) -- init 0
    std::vector<double> agg_out_mf(noutMF, 0.0);

    // iterate rules
    for (int r = 0; r < nrules; r++) {
      double firing;
      if (andMethod == "prod") firing = 1.0;
      else firing = 1.0; // will use min reduction below if not "prod"

      bool skip_rule = false;
      // antecedents: columns 0..nin-1 (R->C indexing: column index j)
      for (int j = 0; j < nin; j++) {
        double rule_val = rules(r, j);      // numeric MF index; often 1-based; 0 may mean "dont care"
        int mf_index = int(rule_val) - 1;   // convert to 0-based
        if (mf_index < 0) {
          // 0 in FuzzyR typically means "dont care" (no constraint on this var)
          continue;
        }
        // evaluate MF of input(irow, j) at this MF
        double x = input(irow, j);
        std::string mf_type = in_mf_types[j][mf_index];
        NumericVector mf_params = in_mf_params[j][mf_index];
        double mu = mf_eval(x, mf_type, mf_params);

        if (andMethod == "prod") {
          firing *= mu;
        } else { // default to "min" t-norm semantics
          firing = std::min(firing, mu);
        }
        if (firing <= 0.0) { // short-circuit
          skip_rule = true;
          break;
        }
      } // end antecedent loop

      if (skip_rule || firing <= 0.0) continue;

      // Get consequent (assume single-output FIS)
      if (consequent_col >= ncols_rule) continue;
      int out_idx = int(rules(r, consequent_col)) - 1; // 0-based
      if (out_idx < 0 || out_idx >= noutMF) continue;

      // optional weight
      double weight = 1.0;
      if (weight_col >= 0 && weight_col < ncols_rule) {
        weight = rules(r, weight_col);
      }
      double fired_val = firing * weight;

      // Implication method: for Mamdani with "min", we store the firing to be combined
      // into output MF via aggregation (aggMethod). Here we accumulate per MF degree using max
      if (aggMethod == "max") {
        agg_out_mf[out_idx] = std::max(agg_out_mf[out_idx], fired_val);
      } else {
        // other aggregation methods could be added
        agg_out_mf[out_idx] = std::max(agg_out_mf[out_idx], fired_val);
      }
    } // end rules loop

    // DEFUZZIFICATION: centroid over discretized universe
    double numerator = 0.0;
    double denominator = 0.0;
    for (int xi = 0; xi < out_disc; xi++) {
      double xv = xs[xi];
      // build aggregated MF at xv: for each output MF, compute mf(xv) then apply implication and aggregation
      double mu_point = 0.0;
      for (int m = 0; m < noutMF; m++) {
        double mfv = mf_eval(xv, out_mf_types[m], out_mf_params[m]);
        // implication: Mamdani min between rule firing degree for that MF and mfv
        double implied = std::min(agg_out_mf[m], mfv);
        // aggregation over rules / consequents: max
        mu_point = std::max(mu_point, implied);
      }
      if (mu_point > 0.0) {
        numerator += xv * mu_point;
        denominator += mu_point;
      }
    }

    if (denominator == 0.0) {
      result[irow] = NA_REAL;
    } else {
      result[irow] = numerator / denominator;
    }
  } // end each row

  return result;
}
')


