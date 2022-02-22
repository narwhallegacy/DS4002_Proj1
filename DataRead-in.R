library(tidyverse)
library(stringr)


name.basics = read.delim('~/School/4th Year/DS4002/DS_Upload/name.basics.tsv', sep = "\t", header = TRUE)
title.akas = read.delim('~/School/4th Year/DS4002/DS_Upload/title.akas.tsv', sep = "\t", header = TRUE)
title.basics = read.delim('~/School/4th Year/DS4002/DS_Upload/title.basics.tsv', sep = "\t", header = TRUE)
title.crew = read.delim('~/School/4th Year/DS4002/DS_Upload/title.crew.tsv', sep = "\t", header = TRUE)
title.episode = read.delim('~/School/4th Year/DS4002/DS_Upload/title.episode.tsv', sep = "\t", header = TRUE)
title.principals= read.delim('~/School/4th Year/DS4002/DS_Upload/title.principals.tsv', sep = "\t", header = TRUE)
title.ratings = read.delim('~/School/4th Year/DS4002/DS_Upload/title.ratings.tsv', sep = "\t", header = TRUE)

#Filter title.basics down to just titles we have scripts of
title.basics2 = title.basics[title.basics$titleType == "movie",]

scripts = c(list.files(path = "~/School/4th Year/DS4002/Github/DS4002_Proj1/scripts"))
scripts = gsub(".txt","",scripts)
title.basics3 = title.basics2
title.basics3$primaryTitle = tolower(gsub(" ","-",title.basics3$primaryTitle))
title.basics3$originalTitle = tolower(gsub(" ","-",title.basics3$originalTitle))

have.scripts = title.basics3$primaryTitle %in% scripts
have.scripts2 = title.basics3$originalTitle %in% scripts

have.scripts.f = have.scripts & have.scripts2

title.basics4 = title.basics3[have.scripts.f,]




#Filter out duplicate items
movieDupes = title.basics4$primaryTitle[duplicated(title.basics4$primaryTitle)]

#List of duplicate film titles for manual selection
tbsh = title.basics4[title.basics4$primaryTitle %in% movieDupes,]

#Checks number of duplicates to look through
test = tbsh[!duplicated(tbsh$primaryTitle),]


title.basics.rate = merge(title.basics4,title.ratings, by.x = "tconst", by.y = "tconst")


#Dataframe of movie and release date
#TODO manually change release date to correct date for duplicate title films
scriptKey = data.frame(title.basics.rate$primaryTitle,title.basics.rate$startYear,title.basics.rate$numVotes)
scriptKey = scriptKey[order(scriptKey$title.basics.rate.numVotes, decreasing = TRUE),]
scriptKey = scriptKey[!duplicated(scriptKey[,1]),]
#clash of the titans 1981
#conan the barbarain 2011
#crash 1996
#dawn of the dead 1978
#Mary Shelley's Frankenstein 1994
#Frozen 2010
#get-carter 1971
#godzilla 1998
#legend 1985
#point break 2015


#------------------------------------------------------------------------------
###Erroneous titles will be manually fixed in a text editor and re-uploaded with correct dates
write.csv(file = "scriptKey.csv",scriptKey)
#------------------------------------------------------------------------------

# Cannot believe I lost the code again
# In case I need to reference the coding method, scriptKey was sorted by descending number of ratings
# Duplicate titles were removed
# titles where the script was not the most reviewed movie of the same name had year changed to correct value manually changed in the script key
# Name+year were combined into an identifier script in both scriptkey2 and titles.basic4 to create a titles.basic.red file, which was written to the folder

#write.csv(file = "titleBasics_Scripts.csv",  title.basics4)







#Obtain random sample of movies after 1960 w/ at least 7000 reviews
bigMovies.basics = title.basics3
bigMovies.basics$startYear = as.numeric(title.basics3$startYear)
bigMovies.basics = bigMovies.basics[!is.na(bigMovies.basics$startYear),]
bigMovies.basics = bigMovies.basics[bigMovies.basics$startYear>=1960,]
bigMovies.basics = merge(bigMovies.basics,title.ratings, by.x = "tconst", by.y = "tconst")
bigMovies.basics = merge(bigMovies.basics,title.crew, by.x = "tconst", by.y = "tconst")
bigMovies.basics = bigMovies.basics[bigMovies.basics$numVotes >= 7000,]

#write.csv(file = "AllBigMovies.csv",bigMovies.basics)
#allBigMovies = read.csv("AllBigMovies.csv",header = TRUE)[,2:14]

#Create list of people ONLY who appear in bigMovies.basics
directors = c()
writers = c()
for (i in 1:length(allBigMovies$genres)) {
  director = strsplit(allBigMovies$directors[i],",")
  writer = strsplit(allBigMovies$writers[i],",")
  directors = c(directors,director)
  writers = c(writers, writer)
}
directors = unlist(directors)
writers = unlist(writers)
people = c(directors,writers)

people = people[!duplicated(people)]


name.basics.2 = name.basics[name.basics$nconst %in% people,]
#write.csv(file = "name.basics.red.csv",name.basics.2)



name.basics.2 = read.csv("name.basics.red.csv")
strlengths = nchar(name.basics.2$knownForTitles)
knownFor = str_split(name.basics.2$knownForTitles,",")

kn = data.frame(matrix(nrow=0,ncol=4))
names(kn) = c("kn1","kn2","kn3","kn4")

for (i in 1:nrow(name.basics.2)) {
  kn1.i = kn2.i = kn3.i = kn4.i = NA
  kn.i = unlist(knownFor[i])
  
  kn1.i = kn.i[1]
  kn2.i = kn.i[2]
  kn3.i = kn.i[3]
  kn4.i = kn.i[4]
  
  kn[i,] = c(kn1.i,kn2.i,kn3.i,kn4.i)
}

name.basics.3 = data.frame(name.basics.2[,-7],kn)


#----------------------------------------------------------------------------
### Function to extract movie review score from unique ID
#----------------------------------------------------------------------------
findScore = function(tconst,movieData) {
  index = match(tconst,movieData$tconst)
  if (is.na(index)) {
    return(NA)
  } else {
    return(movieData$averageRating[index])
  }
}

avgScore = c()
for (i in 1:nrow(name.basics.3)) {
  avgScore.i = mean(c(findScore(name.basics.3$kn1[i],allBigMovies), findScore(name.basics.3$kn2[i],allBigMovies), findScore(name.basics.3$kn3[i],allBigMovies), findScore(name.basics.3$kn4[i],allBigMovies)),na.rm = TRUE)
  if (is.na(avgScore.i)) {
    avgScore = c(avgScore, NA)
  } else {
    avgScore = c(avgScore, avgScore.i)
  }
}
name.basics.4 = name.basics.3 %>% mutate("AvgKnownScore" = avgScore)







findVotes = function(tconst,movieData) {
  index = match(tconst,movieData$tconst)
  if (is.na(index)) {
    return(NA)
  } else {
    return(movieData$numVotes[index])
  }
}

avgVotes = c()
for (i in 1:nrow(name.basics.3)) {
  avgVotes.i = mean(c(findVotes(name.basics.3$kn1[i],allBigMovies), findVotes(name.basics.3$kn2[i],allBigMovies), findVotes(name.basics.3$kn3[i],allBigMovies), findVotes(name.basics.3$kn4[i],allBigMovies)),na.rm = TRUE)
  if (is.na(avgVotes.i)) {
    avgVotes = c(avgVotes, NA)
  } else {
    avgVotes = c(avgVotes, avgVotes.i)
  }
}
name.basics.4 = name.basics.4 %>% mutate("AvgNumVotes" = avgVotes)

#write.csv(name.basics.4,"names.basic.expanded.csv")



allBigMovies = read.csv("AllBigMovies.csv",header = TRUE)[,2:14]
people = read.csv("names.basic.expanded.csv",header = TRUE)[,3:13]

#------------------------------------------------------------------------------
### Add Director data to movie
#------------------------------------------------------------------------------
people.red = people[,-c(2,4:9)]

allBigMovies2 = merge(allBigMovies,people.red,by.x = "directors", by.y = "nconst")

names(allBigMovies2)[14:16] = c("DirBirthYear","DirAvgScore","DirAvgVotes")
#TODO look into multiple directors
#TODO leave NA when entry is not in people





allAvgScores = c()
allAvgVotes = c()
#Test code
allBigMovies2 = allBigMovies %>% mutate("DirAvgScore" = 0) %>% mutate("DirAvgVotes" = 0)
for (i in 1:nrow(allBigMovies)) {
  directors = unlist(strsplit(allBigMovies$directors[i],","))
  av.scores = c()
  av.votes = c()
  for (j in 1:length(directors)) {
    #TODO run through all directors, extracting & averaging DirAvgScore and DirAvgVotes
    index = match(directors[j],people$nconst)
    score = people$AvgKnownScore[index]
    votes = people$AvgNumVotes[index]
    av.scores = c(av.scores,score)
    av.votes = c(av.votes,votes)
  }
  av.scores = mean(score)
  av.votes = mean(votes)
  
  allBigMovies2$DirAvgScore[i] = av.scores
  allBigMovies2$DirAvgVotes[i] = av.votes
}



#------------------------------------------------------------------------------
### Same for writers
#------------------------------------------------------------------------------
allAvgScores = c()
allAvgVotes = c()
#Test code
allBigMovies2 = allBigMovies2 %>% mutate("WriteAvgScore" = 0) %>% mutate("WriteAvgVotes" = 0)
for (i in 1:nrow(allBigMovies)) {
  writers = unlist(strsplit(allBigMovies$writers[i],","))
  av.scores = c()
  av.votes = c()
  for (j in 1:length(writers)) {
    index = match(writers[j],people$nconst)
    score = people$AvgKnownScore[index]
    votes = people$AvgNumVotes[index]
    av.scores = c(av.scores,score)
    av.votes = c(av.votes,votes)
  }
  av.scores = mean(score)
  av.votes = mean(votes)
  
  allBigMovies2$WriteAvgScore[i] = av.scores
  allBigMovies2$WriteAvgVotes[i] = av.votes
}





#------------------------------------------------------------------------------
### Add Genre to movie data
# -----------------------------------------------------------------------------


#Determine what genres are in data
#genres = c()
#for (i in 1:length(allBigMovies$genres)) {
#  movieGenres = strsplit(allBigMovies$genres[i],",")
#  genres = c(genres,movieGenres)
#}
#genres = unlist(genres)
#genres = genres[!duplicated(genres)]


allBigMovies3 = allBigMovies2 %>% mutate ("isDrama" = grepl("Drama",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isSport" = grepl("Sport",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isRomance" = grepl("Romance",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isComedy" = grepl("Comedy",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isAdventure" = grepl("Adventure",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isHistory" = grepl("History",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isAction" = grepl("Action",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isBiography" = grepl("Biography",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isCrime" = grepl("Crime",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isMystery" = grepl("Mystery",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isWar" = grepl("War",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isSci-Fi" = grepl("Sci-Fi",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isFantasy" = grepl("Fantasy",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isThriller" = grepl("Thriller",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isHorror" = grepl("Horror",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isAnimation" = grepl("Animation",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isMusic" = grepl("Music",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isFamily" = grepl("Family",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isMusical" = grepl("Musical",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isDocumentary" = grepl("Documentary",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isNews" = grepl("News",allBigMovies$genres, fixed = TRUE)) %>%
  mutate ("isWestern" = grepl("Western",allBigMovies$genres, fixed = TRUE))


# write.csv(allBigMovies3,"AllBigMovies.csv")
