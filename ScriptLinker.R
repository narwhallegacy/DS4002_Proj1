library(tidyverse)
library(stringr)

allBigMovies = allBigMovies = read.csv("AllBigMovies.csv",header = TRUE)
people = read.csv("names.basic.expanded.csv",header = TRUE)[,3:13]
imdbKey = allBigMovies[,c(2,4,5,7,13,14)]

script.file = c(list.files(path = "~/School/4th Year/DS4002/Github/Github Backup/imsdbscripts"))
scripts = gsub(".txt","",script.file)
scripts = tolower(scripts)

scripts[endsWith(scripts,",-the")] = paste("the-",substr(scripts[endsWith(scripts,",-the")],1,nchar(scripts[endsWith(scripts,",-the")])-5),sep = "")




#Wacky unicode characters used to space writers in script footer, has to be manually extracted
read = readLines(paste("~/School/4th Year/DS4002/Github/Github Backup/imsdbscripts/",script.file[1],sep = ""))
writers = read[startsWith(read, "Writers : ")]
space = substr(writers,11,14)


linkTable = data.frame("file" = script.file, "name" = scripts, "writers" = NA)

#Extract authors of script
scriptWriters = c()
for (i in 1:length(script.file)) {
  read = NA
  writer.list = NA
  read = readLines(paste("~/School/4th Year/DS4002/Github/Github Backup/imsdbscripts/",script.file[i],sep = ""))
  writers = read[startsWith(read, "Writers : ")]
  writer.list = strsplit(writers, space)
  if (length(writer.list) == 0) {
    scriptWriters = c(scriptWriters, NA)
  } else {
    writer.list = list(unlist(writer.list)[2:length(unlist(writer.list))])
    linkTable$writers[i] = writer.list
  }
}




#match script titles to movie titles and writer names




name.match1 = imdbKey$primaryTitle %in% linkTable$name
name.match2 = imdbKey$originalTitle %in% linkTable$name

name.match = name.match1 | name.match2

imdbKey.red = imdbKey[name.match,]


imdbKey.red = imdbKey.red %>% mutate("writernames" = NA)
#get writer names
for (i in 1:nrow(imdbKey.red)) {
  writers = unlist(strsplit(imdbKey.red$writers[i],","))
  names = c()
  for (j in 1:length(writers)) {
    index = match(writers[j], people$nconst)
    names = c(names,people$primaryName[index])
  }
  names = na.exclude(names)
  if (length(names) > 0) {
    #names.writer = c(names.writer,list(names))
    imdbKey.red$writernames[i] = list(names)
  } else {
    #names.writer = c(names.writer,NA)
    imdbKey.red$writernames[i] = NA
    
  }
}




#linkTable2 = linkTable
#linkTable = linkTable2
linkTable = linkTable %>% mutate("tconst" = NA)
tconst = c()

for (i in 1:nrow(linkTable)) {
  subset = imdbKey.red[((imdbKey.red$primaryTitle == linkTable$name[i]) | (imdbKey.red$originalTitle == linkTable$name[i])),]
  if (nrow(subset) > 1) {
    authorMatch = c()
    for (j in 1:nrow(subset)) {
      matches = sum(unlist(linkTable$writers[i]) %in% unlist(subset$writernames[j]))
      authorMatch = c(authorMatch,matches)
    }
    if (max(authorMatch) == 0) {
      unid = NA
    } else {
      index = match(max(authorMatch),authorMatch)
      unid = subset$tconst[index]
    }
  } else {
    unid = subset$tconst[1]
  }
  tconst = c(tconst,unid)  
}

linkTable$tconst = tconst



linkTable.red = linkTable[!is.na(linkTable$tconst),]

write.csv(linkTable.red[,c(1,2,4)], "linkTable.csv")
