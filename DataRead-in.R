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


