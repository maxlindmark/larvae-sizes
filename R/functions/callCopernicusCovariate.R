#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+
#+ callCopernicusCovariate
#+
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+
#+ A function to call NetCDF files and reduce them to a df. 
#+ Implemented to call a given covariate (e.g. sst) using 
#+ Copernicus data (hence the name) obtained from: 
#+ 
#+ https://data.marine.copernicus.eu/
#+ 
#+ These are tipically large files coming in partitions that 
#+ need to be gathered together and wrangled to be used in
#+ the analysis. 
#+  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+
#+ Authors:
#+ - Eros Quesada
#+ - Max Lindmark
#+ 
#+ Dev.notes:
#+ 
#+ - 20240507 Created. Current version filter for January in 
#+            order to meet the needs of a specific project. 
#+            Useful to generalize the filtering 
#+            options in next versions in order to apply the 
#+            functions elsewhere as well. 
#+ 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
callCopernicusCovariate <- function(
  covariate, # One of: sst, chl, depth
  path, # path to covariates
  messages = 1 # dichotomous
){
  # Make specification of path possible
  if(missing(path)){
    pathToCovariate = paste(covPath, covariate, sep = "/")
  }else{
    pathToCovariate = paste(path, covariate, sep = "/")
  }
  # Get files names
  listCovFilesStrings <- list.files(pathToCovariate)
  
  # From nc to tibble, apply Max wrangling (i.e. data will be filtered for January month) 
  listCovFiles <- lapply(listCovFilesStrings, function(x){
    if(messages == 1){
      cat(cyan("Processing"), "-", paste0("Gathering data from df ", which(listCovFilesStrings == x),"/",length(listCovFilesStrings), "."), "\n")
    }
    tidync(paste(pathToCovariate, x, sep = "/")) %>%
      hyper_tibble() %>% 
      mutate(date = as_datetime(time, origin = '1970-01-01')) %>%
      mutate(month = month(date),
             day = day(date),
             year = year(date)) %>% 
      filter(month == 1) %>% 
      group_by(year, longitude, latitude) %>% 
      summarise(sst = mean(analysed_sst) - 273.15) %>% 
      ungroup()
  })
  
  # Inform that data is a subset (may be useful to add filtering options as arguments)
  if(messages == 1){
    cat("\n"); cat("\n")
    cat(cyan("Note"), "-", "The data were filtered internally for January month")
  }
  # From list of tibbles to tibble 
  dfCov <- bind_rows(listCovFiles)
  
  # Inform completion
  if(messages == 1){
    cat("\n"); cat("\n")
    cat(green("Completed"))
  }
  
  # Return tibble
  dfCov
  
}