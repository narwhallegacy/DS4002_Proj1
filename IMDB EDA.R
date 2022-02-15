#EDA
library(tidyverse)


title.ratings = read.delim('~/School/4th Year/DS4002/DS_Upload/title.ratings.tsv', sep = "\t", header = TRUE)
title.basics.red = title.basics.red = read.csv("titleBasicsReduced.csv", header=TRUE)[,2:10]
allBigMovies = read.csv("AllBigMovies.csv",header = TRUE)[,2:14]
people = read.csv("name.basics.red.csv",header = TRUE)[,2:7]


#------------------------------------------------------------------------------
#SingleRandomSample
set.seed(4002)
fold = sample(rep(1:10,length = length(allBigMovies$tconst)))
test.basic = allBigMovies[fold==10,]
train.basic = allBigMovies[!fold==10,]
title.basics.red = merge(title.basics.red,title.ratings, by.x = "tconst", by.y = "tconst")
#write.csv(file = "scriptBasicRating.csv",title.basics.red)

#-----------------------------------------------------------------------------




#Fix Frankenstein

#index = match("mary-shelley's-frankenstein",title.basics3$primaryTitle)
#title.basics.red[1,] = title.basics3[index,]
#write.csv(file="titleBasicsReduced.csv",title.basics.red)





#-------------------------------------------------------------------------------
#Determine what genres are in data
genres = c()
for (i in 1:length(allBigMovies$genres)) {
  movieGenres = strsplit(allBigMovies$genres[i],",")
  genres = c(genres,movieGenres)
}
genres = unlist(genres)
genres = genres[!duplicated(genres)]

#------------------------------------------------------------------------------


#strsplit(title.basics.red$genres,",")

titles = train.basic
titles = titles %>% mutate ("isDrama" = grepl("Drama",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isSport" = grepl("Sport",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isRomance" = grepl("Romance",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isComedy" = grepl("Comedy",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isAdventure" = grepl("Adventure",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isHistory" = grepl("History",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isAction" = grepl("Action",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isBiography" = grepl("Biography",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isCrime" = grepl("Crime",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isMystery" = grepl("Mystery",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isWar" = grepl("War",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isSci-Fi" = grepl("Sci-Fi",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isFantasy" = grepl("Fantasy",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isThriller" = grepl("Thriller",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isHorror" = grepl("Horror",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isAnimation" = grepl("Animation",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isMusic" = grepl("Music",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isFamily" = grepl("Family",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isMusical" = grepl("Musical",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isDocumentary" = grepl("Documentary",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isNews" = grepl("News",train.basic$genres, fixed = TRUE)) %>%
  mutate ("isWestern" = grepl("Western",train.basic$genres, fixed = TRUE))



#GenreModel
genreModel = lm(averageRating ~ isDrama + isSport + isRomance + isComedy+ isAdventure + isHistory + isAction + isBiography + isCrime + 
                  isMystery + isWar + `isSci-Fi` + isFantasy + isThriller + isHorror + isAnimation + isMusic + isFamily + isMusical + isDocumentary +
                  isNews + isWestern, data=titles)  


#Release Year & Rating
ggplot(data = train.basic) + geom_point(aes(x=startYear,y=averageRating)) + xlab("Release Year") + ylab("Average IMDb Rating")

#Runtime & Rating
ggplot(data = train.basic) + geom_point(aes(x=runtimeMinutes,y=averageRating)) + xlab("RuntimeMinutes") + ylab("Average IMDb Rating")

#NumReviews & Rating
ggplot(data = train.basic) + geom_point(aes(x=numVotes,y=averageRating)) + xlab("Number of IMDb User Reviews") + ylab("Average IMDb Rating")
