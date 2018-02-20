library(stringr)
library(dplyr)
library(purrr)
library(rvest)

library(GGally)
library(ggplot2)
library(gpairs)

walkscorepage <- read_html("https://www.walkscore.com/cities-and-neighborhoods/")


vector_of_tags <- c(".pop" , ".bsc" , ".tsc" , ".score" , ".state" , ".city")

for (i in vector_of_tags) {
  print(i)
}
# If you have time turn to function where you iterate over
for (i in vector_of_tags) {
  my_list <- list()
  my_list[i] <- walkscorepage %>% html_nodes(i) %>% html_text(trim = TRUE)

}

pop <- walkscorepage %>% html_nodes(".pop") %>% html_text(trim = TRUE) %>% as.data.frame()
bikeScore <- walkscorepage %>% html_nodes(".bsc") %>% html_text(trim = TRUE)  %>% as.data.frame()
transitScore <- walkscorepage %>% html_nodes(".tsc") %>% html_text(trim = TRUE) %>% as.data.frame()
walkScore <- walkscorepage %>% html_nodes(".score") %>% html_text(trim = TRUE) %>% as.data.frame()
state <- walkscorepage %>% html_nodes(".state") %>% html_text(trim = TRUE) %>% as.data.frame()
city <- walkscorepage %>% html_nodes(".city") %>% html_text(trim = TRUE) %>% as.data.frame()

walkScore <- as.character(walkScore[-1,]) #remove extra element "locate me"
names(walkScore) <- walkScore %>% as.data.frame()

walkDF <- bind_cols(pop, bikeScore, transitScore, as.data.frame(walkScore), state, city)
colnames(walkDF) <- c("population", "bike_score", "transit_score", "walk_score", "state", "city")

walkDF <- walkDF[-1,]
walkDF2 <- walkDF %>% mutate(
                            transit_score = na_if(walkDF$transit_score, "--"),
                            population = na_if(walkDF$population, "--"),
                            bike_score = na_if(walkDF$bike_score, "--"),
                            walk_score = na_if(walkDF$walk_score, "--"),
                            state = na_if(walkDF$state, "--"),
                            city = na_if(walkDF$city, "--")
                            ) %>%
                      mutate( transit_score = as.numeric(transit_score),
                              population = as.numeric(population),
                              bike_score = as.numeric(bike_score),
                              walk_score = as.numeric(walk_score),
                              state = as.character(state),
                              city = as.character(city)
                              )

View(walkDF2)

class(walkDF2$state)
gpairs(walkDF2)
ggpairs(walkDF2, cardinality_threshold = 200)
ggcorr(walkDF2, label = TRUE)



# R twitter APIs
library(rtweet)
#library(twitteR)
#library(streamR)
# clean
library(dplyr)
library(tidytext)
library(tidyr)
# visualize / sanity check
library(ggplot2)
library(ggmap)


stream_tweets(
  "",
  # "a, the, i, you, u, and, is, in, it, of, for, on, my, that, at, with, me, do, have",
  # language = "en",# empty brackets pulls random sample...   COMPLETELY INEFECTIVE 
  #timeout = 60 * 60 * 24 * 7, Two weeks
  #lookup_coords("usa"), # streams from USA... may be lower 48 only. 
  # We may want to compare english speaking countries. 
  timeout = 60,
  file_name = "tweetstest.json",
  parse = FALSE, # avoids the length process of converting JSON to R data type. 
  #token = twitter_token,
  verbose = FALSE,
  include_rts = FALSE
)

large_tweets <- parse_stream("tweetstest.json")## read in the data as a tidy tbl data frame
unique(large_tweets$lang)
tweets <- select(large_tweets, screen_name, text, country, lang, geo_coords, coords_coords, bbox_coords) %>% 
  lat_lng() %>% 
  filter(!is.na(lng)) %>%
  filter(lang == "en")

cleaned <- unnest_tokens(tweets, input = text, output = words)


