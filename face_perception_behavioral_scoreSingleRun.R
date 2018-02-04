score.beh.run <- function(file.contents){
  ##### Scoring Behavioral Data from single run in  Face Perception Task #####
  require(dplyr)
  require(xlsx)
  
  ##### convert file contents to list ####
  ##  Here's the rub!
  ##    gotta figure out what info to keep from behavioral files
  ##    figure out all possibilities for info within a behavioral file, and take the value
  ##
  start.locs = grep('Start ***',file.contents$text,fixed = T)
  end.locs = grep('End ***',file.contents$text,fixed = T)
  
  ## data arrangement:
  ##  Header: list of items within the header. 
  ##    walk through and make the post-colon value the value of the pre-colon string, within the list
  ##  log frames: list of log frames
  ##    log frame:  list of items within the log frame, 
  ##      pre-colon string is list item, post colon value is value
  frame.2.list <- function(v,frame.start.i,frame.end.i){
    frame.list = list()
    # clean up v
    v = gsub(gsub(x = v,pattern = '\t\t',replacement = '',fixed = T),
             pattern = '\t',replacement = '',fixed = T)
    j=1
    for (i in seq(frame.start.i+1,frame.end.i-1)){
      ##TODO## fix time fuckery UTC
      line.split = unlist(strsplit(v[i],split = ':',fixed=T))
      #print(line.split)
      frame.list[[j]]=trimws(line.split[2:length(line.split)],which='both')
      # if length is greater than 1, it's UTC, so paste the time back together
      #print(frame.list[[j]]); print(length(frame.list[[j]]))
      if(length(frame.list[[j]]) > 1){
        frame.list[[j]]=paste(frame.list[[j]],collapse = ':')
      }
      if(substr(frame.list[[j]],1,2)=='NA'){
        frame.list[[j]]=NA
      }
      names(frame.list)[j]=line.split[1]
      j=j+1
    }
    return(frame.list)
  }
  # Header is first, walk through for everything else
  beh.data <- list(header.frame=list(),log.frames=list())
  cat('\ngetting header frame')
  beh.data$header.frame <- frame.2.list(as.vector(file.contents$text),
                                        start.locs[1],end.locs[1])
  cat('\ngetting log frames\n',length(start.locs),'log frames')
  for (i in seq(2,length(start.locs))){
    beh.data$log.frames[[i-1]] <- frame.2.list(as.vector(file.contents$text),
                                               start.locs[i],end.locs[i])
  }
  # cat('\n')
  # cat('\nSession Start Date Time UTC\n')
  print(beh.data$header.frame$SessionStartDateTimeUtc)
  ## bind log frames into df
  beh.data$log.frames.df=data.frame(beh.data$log.frames[[1]])
  for(i in 2:length(beh.data$log.frames)){
    beh.data$log.frames.df <- beh.data$log.frames.df %>%
      bind_rows(beh.data$log.frames[[i]])
  }
  # remove log.frames in list after re-storing in df
  beh.data$log.frames <- NULL
  ## coalesce similar columns and remove session-general stuff
  beh.data$log.frames.df <- beh.data$log.frames.df %>% 
    select(-c(matches('cue|ProcList|Image[1,2]|Session|Version|Runtime'),
              Subject,Experiment,DataFile.Basename,RandomSeed,Group,
              Display.RefreshRate,Clock.Information)) %>%
    filter(!grepl('Cue|Block',Procedure)) %>%
    rowwise() %>%
    mutate(ProbeList.all = coalesce(ProbeList,ProbeList1,ProbeList2),
           ProbeList.all.Cycle = coalesce(ProbeList.Cycle,ProbeList1.Cycle,ProbeList2.Cycle),
           ProbeList.all.Sample=coalesce(ProbeList.Sample,ProbeList1.Sample,ProbeList2.Sample),
           Probe.all.OnsetDelay=coalesce(Probe.OnsetDelay,Probe1.OnsetDelay,Probe2.OnsetDelay),
           Probe.all.OnsetTime=coalesce(Probe.OnsetTime,Probe1.OnsetTime,Probe2.OnsetTime),
           Probe.all.DurationError=
             coalesce(Probe.DurationError,Probe1.DurationError,Probe2.DurationError),
           Probe.all.Duration=coalesce(Probe.Duration,Probe1.Duration,Probe2.Duration),
           Probe.all.StartTime=coalesce(Probe.StartTime,Probe1.StartTime,Probe2.StartTime),
           Probe.all.FinishTime=coalesce(Probe.FinishTime,Probe1.FinishTime,Probe2.FinishTime),
           Probe.all.RTTime=coalesce(Probe.RTTime,Probe1.RTTime,Probe2.RTTime),
           Probe.all.ACC=coalesce(Probe.ACC,Probe1.ACC,Probe2.ACC),
           Probe.all.RT=coalesce(Probe.RT,Probe1.RT,Probe2.RT),
           Probe.all.RESP=coalesce(Probe.RESP,Probe1.RESP,Probe2.RESP),
           Probe.all.CRESP=coalesce(Probe.CRESP,Probe1.CRESP,Probe2.CRESP),
           Probe.all.OnsetToOnsetTime=
             coalesce(Probe.OnsetToOnsetTime,Probe1.OnsetToOnsetTime,Probe2.OnsetToOnsetTime)
    ) %>%
    mutate_at(vars(matches('Probe|ClickCount|Code|CorrectResponse|Dur'),-Procedure),funs(as.numeric(.)))
  return(beh.data)
}
