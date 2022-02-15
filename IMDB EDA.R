#EDA
library(tidyverse)


title.ratings = read.delim('~/School/4th Year/DS4002/DS_Upload/title.ratings.tsv', sep = "\t", header = TRUE)
title.basics.red = title.basics.red = read.csv("titleBasicsReduced.csv", header=TRUE)[,2:10]

#Fix Frankenstein

#index = match("mary-shelley's-frankenstein",title.basics3$primaryTitle)
#title.basics.red[1,] = title.basics3[index,]
#write.csv(file="titleBasicsReduced.csv",title.basics.red)

title.basics.red = merge(title.basics.red,title.ratings, by.x = "tconst", by.y = "tconst")

ggplot(data = title.basics.red) + geom_point(aes(x=startYear,y=averageRating)) + xlab("Release Year") + ylab("Average IMDb Rating")

#write.csv(file = "scriptBasicRating.csv",title.basics.red)

genres = c()
for (i in 1:length(title.basics.red$genres)) {
  movieGenres = strsplit(title.basics.red$genres[i],",")
  genres = c(genres,movieGenres)
}
genres = unlist(genres)
genres = genres[!duplicated(genres)]




#strsplit(title.basics.red$genres,",")

titles = title.basics.red
titles = titles %>% mutate ("isDrama" = grepl("Drama",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isSport" = grepl("Sport",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isRomance" = grepl("Romance",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isComedy" = grepl("Comedy",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isAdventure" = grepl("Adventure",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isHistory" = grepl("History",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isAction" = grepl("Action",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isBiography" = grepl("Biography",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isCrime" = grepl("Crime",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isMystery" = grepl("Mystery",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isWar" = grepl("War",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isSci-Fi" = grepl("Sci-Fi",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isFantasy" = grepl("Fantasy",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isThriller" = grepl("Thriller",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isHorror" = grepl("Horror",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isAnimation" = grepl("Animation",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isMusic" = grepl("Music",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isFamily" = grepl("Family",title.basics.red$genres, fixed = TRUE)) %>%
  mutate ("isMusical" = grepl("Musical",title.basics.red$genres, fixed = TRUE))



genreModel = lm(averageRating ~ isDrama + isSport + isRomance + isComedy+ isAdventure + isHistory + isAction + isBiography + isCrime + 
                  isMystery + isWar + `isSci-Fi` + isFantasy + isThriller + isHorror + isAnimation + isMusic + isFamily + isMusical, data=titles)  

