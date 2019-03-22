# library -----------------------------------------------------------------
library(sp)
library(tmaptools)
library(tidyverse)


# input -------------------------------------------------------------------
path_tweetsRaw <- "/path/to/your/folder/here"
path_tweetsUse <- "/path/to/your/folder/here"
path_output <- "/path/to/your/folder/here"
path_map <- "/path/to/your/folder/here"

foldernames <- list.dirs(path_tweetsRaw), full.names=TRUE)

from <- 
to <- 

tweets_period <- foldernames[from:to]
tweets_area <- read_shape(path_map)

# function for loading the tweets
tweets_read <- function(tweets_period, tweets_area) {
  for(i in tweets_period){
    
    nc <- as.numeric(as.character(nchar(i)))
    i_file <- substr(as.character(i),(nc-13),nc)
    filenames <- list.files(i, pattern="*.csv", full.names=TRUE)
    
    for(j in filenames){
      # if the merged dataset doesn't exist, create it
      if (!exists("dataset")){
        dataset <- read_csv(j) %>% 
          select(id_str,user_id_str,name,screen_name,full_name,protected,verified,
                 in_reply_to_screen_name,in_reply_to_status_id_str,in_reply_to_user_id_str,
                 created_at,time_zone,utc_offset,lang,user_lang,
                 country_code,country,
                 place_type,place_name,place_id,place_lat,place_lon,
                 location,geo_enabled,lat,lon,
                 text,truncated,retweet_count,retweeted)
        dataset <- drop_na(dataset, c(place_lon,place_lat))
        
        # Tweets points
        coords <- SpatialPoints(dataset[,c("place_lon","place_lat")])
        dataset <- SpatialPointsDataFrame(coords, dataset)
        ## assign CRS/projection
        proj4string(dataset) <- CRS(get_projection(tweets_area))
        
        # clip the tweets 
        tweets <- dataset[tweets_area, ]
        dataset <- attr(tweets,"data")
        
        rm(coords)
        rm(tweets)
      }
      # if the merged dataset does exist, append to it
      if (exists("dataset")){
        temp_dataset <- read_csv(j) %>% 
          select(id_str,user_id_str,name,screen_name,full_name,protected,verified,
                 in_reply_to_screen_name,in_reply_to_status_id_str,in_reply_to_user_id_str,
                 created_at,time_zone,utc_offset,lang,user_lang,
                 country_code,country,
                 place_type,place_name,place_id,place_lat,place_lon,
                 location,geo_enabled,lat,lon,
                 text,truncated,retweet_count,retweeted)
        temp_dataset <- drop_na(temp_dataset, c(place_lon,place_lat))
        
        # Tweets points
        coords <- SpatialPoints(temp_dataset[,c("place_lon", "place_lat")])
        temp_dataset <- SpatialPointsDataFrame(coords, temp_dataset)
        ## assign CRS/projection
        proj4string(temp_dataset) <- CRS(get_projection(tweets_area))
        
        # clip the tweets 
        tweets <- temp_dataset[tweets_area, ]
        temp_dataset <- attr(tweets,"data")
        
        rm(coords)
        rm(tweets)
        
        ## join
        dataset<-rbind(dataset, temp_dataset)
        rm(temp_dataset)
      }
    }
    
    write.csv(dataset, file=gzfile(paste(path_tweetsUse,i_file,".csv.gz",sep="")))
    rm(dataset)
  }
  
  rm(filenames)
  rm(i)
  rm(nc)
  rm(i_file)
  #rm(i_folder)
  rm(j)
}

# function for joining the tweets
tweets_join <- function(path_output, fileName_output) {
  filenames <- list.files(paste(path_output,"/",sep=""), pattern="*.csv", full.names=TRUE)
  #filenames
  
  for(j in filenames){
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read_csv(j)
    }
    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset <- read_csv(j)
      dataset <- rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  
  dataset <- unique(dataset[,])
  write_csv(dataset, fileName_output)
  
  rm(dataset)
  rm(filenames)
  rm(j)
}

# loading the tweets ------------------------------------------------------
tweets_read(tweets_period, tweets_area)
fileName_output = "fileName_output_here.csv"
tweets_join(path_output, fileName_output)
