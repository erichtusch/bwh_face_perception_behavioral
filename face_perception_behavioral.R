##### Scoring Behavioral Data from Face Perception Task #####
library(readtext)
library(dplyr)

##### Set working directory #####
cat('\n set working directory')
setwd(file.path(choose.dir(default = getwd(),caption="set working directory where helper files live"),'/'))
#setwd(file.path("C:",'Users','erich_000','Documents','GitHub','bwh_face_perception_behavioral'))

##### get all behavioral filepaths #####
beh.file.paths <- choose.files(default = file.path(getwd()),caption = "Get all behavioral files",multi = T)

##### Read In Behavioral File #####
##  from externally stored function ##
get.beh.file.contents <- dget(file.path(getwd(),'face_perception_behavioral_readSingleRun.R'))
file.contents <- list()
for(i in 1:length(beh.file.paths)){
  file.contents[[i]] <- get.beh.file.contents(beh.file.paths[i])
  names(file.contents)[i]=basename(tools::file_path_sans_ext(beh.file.paths[i]))
}

##### convert file contents to list ####
##  from externally stored function ##
score.run <- dget(file.path(getwd(),'face_perception_behavioral_scoreSingleRun.R'))
beh.data <- list()
for(i in 1:length(file.contents)){
  cat('\nscoring run',names(file.contents)[i])
  beh.data[[i]] <- score.run(file.contents[[i]])
  names(beh.data)[i] <- names(file.contents)[i]
  # clear present file contents for RAM conservation
  #   can't do null, because that removes the index, makes the loop break
  file.contents[i] <- "scored"
}
##### for each run, write single xlsx with two sheets: Header and Values #####
# for(i in 1:length(beh.data)){
#   cat('\nsaving',names(beh.data)[i],'at',getwd())
#   write.xlsx(x=as.data.frame(beh.data[[i]]$log.frames.df),sheetName='values',
#              file = file.path(getwd(),paste0(beh.data[[i]]$header.frame$DataFile.Basename,"_behOutput.xlsx")),
#              col.names = T,row.names = F,append = F,showNA = F)
#   write.xlsx(x=t(data.frame(beh.data[[i]]$header.frame)),sheetName='header',
#              file = file.path(getwd(),paste0(beh.data[[i]]$header.frame$DataFile.Basename,"_behOutput.xlsx")),
#              col.names = F,row.names = T,append = T,showNA = F)
# }

##### Score each block and across blocks for each n pairs and across n pairs #####
cat('\nscoring multiple runs')
##TODO## do this once per subject
beh.data.by.subj <- list()
# make beh.data list per subject
for (i in 1:length(beh.data)){
  if(i==1){subject.IDs <- NA}
  subject.IDs <- na.omit(unique(c(subject.IDs,beh.data[[i]]$header.frame$Subject)))
}
score.mult.run <- dget(file.path(getwd(),'face_perception_behavioral_scoreMultipleRuns.R'))
for(i in 1:length(subject.IDs)){
  cat('\n scoring multiple runs for subject ID =',subject.IDs[i])
  beh.data.by.subj[[i]] <- score.mult.run(beh.data[grep(subject.IDs[i],names(beh.data))])
  names(beh.data.by.subj)[i] <- subject.IDs[i]
}
rm(beh.data)
######  reformat subject/block data for output ######
for (i in 1:length(beh.data.by.subj)){
  n.subject.pairings = length(beh.data.by.subj[[i]])
  beh.data.by.subj[[i]]$summary.measures <- data.frame()
  for (j in 1:n.subject.pairings){
    current.subject = beh.data.by.subj[[i]][[j]]$header.frame$Subject
    if(is.null(current.subject)){
      current.subject <- strsplit(beh.data.by.subj[[i]][[j]]$DataFile.Basename,split='-')[[1]][2]
    }
    if(!is.null(beh.data.by.subj[[i]][[j]]$header.frame$DataFile.Basename)){
      current.pairing = unlist(strsplit(beh.data.by.subj[[i]][[j]]$header.frame$DataFile.Basename,split="_"))[3]
    } else{current.pairing <- "all.pairings"}
    cat('\nadding current.subject',current.subject,'and current.pairing',current.pairing,'to subject summary')
    beh.data.by.subj[[i]]$summary.measures <- beh.data.by.subj[[i]]$summary.measures %>%
      bind_rows(as.data.frame(beh.data.by.subj[[i]][[j]]$summary.measures) %>% 
                  mutate(subject.pairing=paste(current.subject,current.pairing,sep='.')))
  }
}
cat('\nbinding summary measures into output')
beh.data.output <- data.frame()
for(i in 1:length(beh.data.by.subj)){
  beh.data.output <- bind_rows(beh.data.output,beh.data.by.subj[[i]]$summary.measures)
}
beh.data.output <- select(beh.data.output,subject.pairing,everything())

##### write xlsx with summary values across all sheets for each subject #####
cat('\nwriting output xlsx')
xlsx::write.xlsx(x=as.data.frame(beh.data.output),file = file.path(getwd(),"beh_data_output.xlsx"),
                 sheetName = "behavioral_data",col.names = T,row.names = F,append = F,showNA = F)

cat('\n\nDONE\n')
