#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+
#+ extractCovariateAtLocation
#+
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+
#+ A function to extract the mean annual value registered for 
#+ a given covariate at set of location provided. 
#+ It was designed to work with copernicus data obtained from
#+ 
#+ https://data.marine.copernicus.eu/
#+ 
#+ and compiled with the function 
#+ 
#+ callCopernicusCovariate()
#+ 
#+  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+
#+ Authors:
#+ - Max Lindmark
#+ - Eros Quesada
#+ 
#+ Dev.notes:
#+ 
#+ - 20240507 Created. 
#+ 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
extractCovariateAtLocation <- function(
  covariate,  # Covariate to evaluate. One of: sst, chlorophyll, depth
  locations_data, # A df containing the set of year ("year") and locations ("lon", "lat") to be evaluated.
  covariate_data,  # A df containing the covariate at location
  nametocov, # A string indicating how to rename the covariate
  messages = c(0,1) # dichotomous
){
  
  # Get the years 
  listYearsAvailable <- sort(unique(locations_data$year))
  
  # Loop, make raster and extract
  covListAtLocation <- lapply(
    listYearsAvailable, 
    function(x){

      # Inform on the process 
      if(messages == 1){
        cat(cyan("Processing"), "-", paste0("Gathering covariate information at location for year ", which(listYearsAvailable == x),"/",length(listYearsAvailable), ":", x, "."), "\n")
      }
            
      # Filter the year 
      d_sub <- filter(locations_data, year == x)
      cov_tibble_sub <- filter(covariate_data, year == x)
    
      # Convert covariate tibble to raster
      cov_raster <- as_spatraster(cov_tibble_sub, xycols = 2:3,
                                   crs = "WGS84", digits = 2)
      
      # Extract at location from raster
      d_sub$cov <- pull(terra::extract(cov_raster[covariate],
                                d_sub[, c("lon", "lat")])[2])
      
      # Return the df. 
      d_sub
      
    })
  
  # From list of tibbles to tibble 
  dfCovAtLocation <- bind_rows(covListAtLocation)
  
  # Rename the covariate based on user selection 
  dfCovAtLocation <- dfCovAtLocation %>% 
    dplyr::rename(
      !!nametocov := cov
    )
  
  # Inform completion
  if(messages == 1){
    cat("\n"); cat("\n")
    cat(green("Completed"))
  }
  
  # Return resulting df
  dfCovAtLocation
  
}











