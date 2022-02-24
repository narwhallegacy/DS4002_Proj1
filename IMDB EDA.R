#EDA
library(tidyverse)
library(MASS)


title.ratings = read.delim('~/School/4th Year/DS4002/DS_Upload/title.ratings.tsv', sep = "\t", header = TRUE)
title.basics.red = title.basics.red = read.csv("titleBasicsReduced.csv", header=TRUE)[,2:10]
allBigMovies = read.csv("AllBigMovies.csv",header = TRUE)[,2:40]
people = read.csv("names.basic.expanded.csv", header = TRUE)[,3:13]


#------------------------------------------------------------------------------
#SingleRandomSample
#allBigMovies.red = allBigMovies[,-c(1:5,7,9,12:13)]
set.seed(4002)
fold = sample(rep(1:10,length = length(allBigMovies$tconst)))
test.basic = allBigMovies[fold==10,]
train.basic = allBigMovies[!fold==10,]
#title.basics.red = merge(title.basics.red,title.ratings, by.x = "tconst", by.y = "tconst")
#write.csv(file = "scriptBasicRating.csv",title.basics.red)

#-----------------------------------------------------------------------------




#Fix Frankenstein

#index = match("mary-shelley's-frankenstein",title.basics3$primaryTitle)
#title.basics.red[1,] = title.basics3[index,]
#write.csv(file="titleBasicsReduced.csv",title.basics.red)





#-------------------------------------------------------------------------------
#Determine what genres are in data
#genres = c()
#for (i in 1:length(allBigMovies$genres)) {
#  movieGenres = strsplit(allBigMovies$genres[i],",")
#  genres = c(genres,movieGenres)
#}
#genres = unlist(genres)
#genres = genres[!duplicated(genres)]

#------------------------------------------------------------------------------


#GenreModel
genreModel = lm(averageRating ~ isDrama + isSport + isRomance + isComedy+ isAdventure + isHistory + isAction + isBiography + isCrime + 
                  isMystery + isWar + `isSci.Fi` + isFantasy + isThriller + isHorror + isAnimation + isMusic + isFamily + isMusical + isDocumentary +
                  isNews + isWestern, data=allBigMovies)  


#Release Year & Rating
ggplot(data = train.basic) + geom_point(aes(x=startYear,y=averageRating)) + xlab("Release Year") + ylab("Average IMDb Rating")

#Runtime & Rating
ggplot(data = train.basic) + geom_point(aes(x=runtimeMinutes,y=averageRating)) + xlab("RuntimeMinutes") + ylab("Average IMDb Rating")

#NumReviews & Rating
ggplot(data = train.basic) + geom_point(aes(x=numVotes,y=averageRating)) + xlab("Number of IMDb User Reviews") + ylab("Average IMDb Rating")







#------------------------------------------------------------------------------
### Big Model
#------------------------------------------------------------------------------

#Function to generate MSE of MLR model on test data
validate.mse = function(model,testData) {
  preds = predict(model,testData[,-c(1:5,7,9,12:13)])
  resid = preds - testData$averageRating
  return(mean(resid**2, na.rm = TRUE))
}


validate.mae = function(model,testData) {
  preds = predict(model,testData[,-c(1:5,7,9,12:13)])
  resid = preds - testData$averageRating
  return(mean(abs(resid), na.rm = TRUE))
}


full.model.1o = lm(averageRating ~ ., data=train.basic[,-c(1:5,7,9,12:13)])

step.model.1o = stepAIC(full.model.1o, dorection = "both", trace = FALSE)

summary(step.model.1o)

validate.mse(step.model.1o,test.basic)
validate.mae(step.model.1o,test.basic)


#TODO MSE, MAE, RMSE, R^2 adj


#saveRDS(step.model.1o, "IMDb-MLR.rds")





#full.model.1int = lm(averageRating ~ (.)^2,data=train.basic[,-c(1:5,7,9,12:13)])


#validate(full.model.1int,test.basic)
#step.model.1int = step.model.1o = stepAIC(full.model.1int, dorection = "both", trace = FALSE)



#saveRDS(step.model.1o, "IMDb-MLR.rds")
