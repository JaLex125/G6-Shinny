library(data.table)
library(ggplot2)
library(igraph)
library(tidyverse)
library(corrplot)
library(knitr)
library(stringr)
library(RColorBrewer)
library(tidyselect)
library(shiny)
library(dplyr)
library(plyr)
library(viridisLite) 

setwd("C:/Users/joaod/Nova SBE/Network Analytics/Shinny App/Spotify Project/Shinny App")

df.spotify <- read.csv('Spotify-2000.csv')


df.spotify$Liveness <- NULL
df.spotify$Danceability <- NULL
df.spotify$Valence <- NULL
df.spotify$Acousticness <- NULL
df.spotify$Speechiness <- NULL

newnames <- c('index', 'title', 'artist', 'top_genre', 'year', 'bpm', 'energy', 'dB', 'duration', 'popularity')

names(df.spotify) <- newnames

df.spotify <- df.spotify %>% 
  mutate(popularity_ranking = as.numeric(case_when(
    (('popularity' > 0) & ('popularity' < 20)) ~ "1",
    (('popularity' >= 20) & ('popularity' < 40))~ "2",
    (('popularity' >= 40) & ('popularity' < 60)) ~ "3",
    TRUE ~ "4"))
  )

df.spotify$energy_only <- cut(df.spotify$energy, breaks = 10)

energy.plot <- df.spotify %>%
  ggplot( aes(x = energy_only )) +
  geom_bar(width = 0.2, fill = "#FF9999", colour = "black") +
  scale_x_discrete(name = "Energy")

df.spotify.artist.popularity <- df.spotify %>% 
  group_by(artist) %>%
  filter(n() >= 5) %>% 
  dplyr::summarize(mean_popularity = mean(popularity)) %>% 
  arrange(desc(mean_popularity)) %>% 
  slice_head(n = 10)


# save(df.spotify, file = "spotify_clean.RData")
# save(energy.plot, file = "energy_plot.RData")
# save(df.spotify.artist.popularity, file = "spotify_artist_popularity.RData")



song.bpm <- data.frame(matrix(ncol = 3, nrow = nrow(df.spotify)))
colnames(song.bpm) <- c('title','top_genre', 'bpm' )
song.bpm$title <- df.spotify$title
song.bpm$top_genre <- df.spotify$top_genre
song.bpm$bpm <- df.spotify$bpm




setDT(song.bpm)[bpm <=25, bpm_group := "0-25"]
song.bpm[bpm >25 & bpm <=50, bpm_group := "25-50"]
song.bpm[bpm >50 & bpm <=75, bpm_group := "50-75"]
song.bpm[bpm >75 & bpm <=100, bpm_group := "75-100"]
song.bpm[bpm >100 & bpm <=125, bpm_group := "100-125"]
song.bpm[bpm >125 & bpm <=150, bpm_group := "125-150"]
song.bpm[bpm >150 & bpm <=175, bpm_group := "150-175"]
song.bpm[bpm >175 & bpm <=200, bpm_group := "175-200"]
song.bpm[bpm >200 , bpm_group := "200+"]



sum(is.na(song.bpm))

song.bpm.clean <- na.omit(song.bpm)

sum(is.na(song.bpm.clean))



df.search.criteria <- song.bpm

df.search.criteria$popularity <- df.spotify$popularity

df.search.criteria$year <- df.spotify$year

df.search.criteria$duration <- df.spotify$duration




df.search.criteria$duration <- as.numeric(df.search.criteria$duration)




df.search.criteria <- na.omit(df.search.criteria)

# 
# save(df.spotify, file = "spotify_clean.RData")
# save(energy.plot, file = "energy_plot.RData")
# save(df.spotify.artist.popularity, file = "spotify_artist_popularity.RData")

save(df.spotify, energy.plot, df.spotify.artist.popularity, df.search.criteria, file = "search-criteria.RData")



