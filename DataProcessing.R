
library(tidyverse)
library(dplyr)
library(countrycode)


df_artists <-  read.csv("wasabi_artists.csv",na.strings=c("","NA"))

# Code to add the continents ---------------------------------------------------
df_artists$continent <- countrycode(sourcevar = df_artists[, "location.country"],
               origin = "country.name", destination = "continent")
myvars <- c("name","type","gender","genres","continent","deezerFans")
newdata <- df_artists[myvars]
newdata$gender[is.na(newdata$gender)] <- "Both"

# group_df_c <- aggregate(deezerFans ~ continent, data = df_artists, sum)
# ------------------------------------------------------------------------------
# Add the lat column -----------------------------------------------------------
newdata <- newdata %>% mutate(lat =
                          case_when(continent == 'Europe' ~ '49.17557', 
                                     continent == 'Asia' ~ "34.0479",
                                      continent == 'Americas' ~ "46.24609",
                                    continent == 'Oceania' ~ "-26.70487",
                                    continent == 'Africa' ~ "13.73973"))
newdata <- newdata %>% mutate(lon =
                                      case_when(continent == 'Europe' ~ '13.49344', 
                                                continent == 'Asia' ~ "100.6197",
                                                continent == 'Americas' ~ "-104.09475",
                                                continent == 'Oceania' ~ "138.19662",
                                                continent == 'Africa' ~ "20.01333"))

newdata <- transform(newdata, lat = as.numeric(lat), 
          lon = as.numeric(lon))
newdata <- newdata[!is.na(newdata$continent),]
newdata <- newdata[!is.na(newdata$deezerFans),]
newdata$deezerFans_convert <- NA
for(i in 1:length(newdata$deezerFans)){
  
  newdata$deezerFans_convert[i] <- newdata$deezerFans[i]/10**5
}
# write.csv(newdata,"fans1.csv", row.names = FALSE)



