##### Scoring Behavioral Data from Face Perception Task #####
library(readtext)
library(dplyr)
##### Set working directory #####
setwd(file.path("C:",'Users','erich_000','Documents','GitHub','bwh_face_perception_behavioral'))
##### get all behavioral filepaths #####
beh.file.paths <- choose.files(default = file.path(getwd()),caption = "Get all behavioral files",multi = T)
##### Read In Behavioral File #####
##  from externally stored function
get.beh.file.contents <- dget(file.path(getwd(),'face_perception_behavioral_readSingleRun.R'))
file.contents <- list()
for(i in 1:length(beh.file.paths)){
  file.contents[[i]] <- get.beh.file.contents(beh.file.paths[i])
  names(file.contents)[i]=basename(tools::file_path_sans_ext(beh.file.paths[i]))
}
##### convert file contents to list ####
##  from externally stored function
score.run <- dget(file.path(getwd(),'face_perception_behavioral_scoreSingleRun.R'))
beh.data <- list()
for(i in 1:length(file.contents)){
  cat('\nscoring run',names(file.contents)[i])
  beh.data[[i]] <- score.run(file.contents[[i]])
  names(beh.data)[i] <- names(file.contents)[i]
}
##### Write single xlsx with two sheets: Header and Values #####
for(i in 1:length(beh.data)){
  cat('\nsaving',names(beh.data)[i],'at',getwd())
  write.xlsx(x=as.data.frame(beh.data[[i]]$log.frames.df),sheetName='values',
             file = file.path(getwd(),paste0(beh.data[[i]]$header.frame$DataFile.Basename,"_behOutput.xlsx")),
             col.names = T,row.names = F,append = F,showNA = F)
  write.xlsx(x=t(data.frame(beh.data[[i]]$header.frame)),sheetName='header',
             file = file.path(getwd(),paste0(beh.data[[i]]$header.frame$DataFile.Basename,"_behOutput.xlsx")),
             col.names = F,row.names = T,append = T,showNA = F)
}
##TODO##
##### write xlsx with summary values across all sheets with file names #####

 cat('\n\nDONE\n')