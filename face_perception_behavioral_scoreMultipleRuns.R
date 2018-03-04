### beh.data is a list of lists ###
##  each sub list has: 
##    list header.frame with run information
##    data frame log.frames.df with variables for three blocks ('','1','2') and all blocks appended ('all')
  
score.mult.run <- function(beh.data){
  require(dplyr)
  ##  add n pair to beh.data sublists
  # The string of numbers following M or F represent the face pair.
  #   Because each discrete pair of faces is represented by a number, 
  #   we can tell how many pairs were in the run by counting the number of numbers in the string. 
  #   We consider 10 as one number. Also, each face pair has two versions, so there are eight versions total.
  # 1 Face Pair: 1; 5
  # 2 Face Pair: 32; 76
  # 3 Face Pair: 654; 8910
  # 4 Face Pair: 1234; 10987
  ##  write list for block measures
  block.measures <- data.frame(targ.perc.corr_100 = NA, targ.perc.corr_69 = NA, 
                               targ.perc.corr_22 = NA, targ.perc.corr_100_69_22 = NA,
                               targcorr.mn.RT_100 = NA, targcorr.mn.RT_69 = NA,
                               targcorr.mn.RT_22 = NA, targcorr.mn.RT_100_69_22 = NA,
                               targcorr.med.RT_100 = NA, targcorr.med.RT_69 = NA,
                               targcorr.med.RT_22 = NA, targcorr.med.RT_100_69_22 = NA,
                               targ.perc.inc_100 = NA, targ.perc.inc_69 = NA,
                               targ.perc.inc_22 = NA, targ.perc.inc_100_69_22 = NA,
                               targinc.mn.RT_100 = NA, targinc.mn.RT_69 = NA,
                               targinc.mn.RT_22 = NA, targinc.mn.RT_100_69_22 = NA,
                               targinc.med.RT_100 = NA, targinc.med.RT_69 = NA,
                               targinc.med.RT_22 = NA, targinc.med.RT_100_69_22 = NA,
                               nonTarg.perc.corr_100 = NA, nonTarg.perc.corr_69 = NA,
                               nonTarg.perc.corr_22 = NA, nonTarg.perc.corr_100_69_22 = NA,
                               nonTargcorr.mn.RT_100 = NA, nonTargcorr.mn.RT_69 = NA,
                               nonTargcorr.mn.RT_22 = NA, nonTargcorr.mn.RT_100_69_22 = NA,
                               nonTargcorr.med.RT_100 = NA, nonTargcorr.med.RT_69 = NA,
                               nonTargcorr.med.RT_22 = NA, nonTargcorr.med.RT_100_69_22 = NA,
                               nonTarg.perc.inc.FA_100 = NA, nonTarg.perc.inc.FA_69 = NA,
                               nonTarg.perc.inc.FA_22 = NA, nonTarg.perc.inc.FA_100_69_22 = NA,
                               nonTarginc.FA.mn.RT_100 = NA, nonTarginc.FA.mn.RT_69 = NA,
                               nonTarginc.FA.mn.RT_22 = NA, nonTarginc.FA.mn.RT_100_69_22 = NA,
                               nonTarginc.FA.med.RT_100 = NA, nonTarginc.FA.med.RT_69 = NA,
                               nonTarginc.FA.med.RT_22 = NA, nonTarginc.FA.med.RT_100_69_22 = NA,
                               targ.perc.tooEarly_100 = NA, targ.perc.tooEarly_69 = NA,
                               targ.perc.tooEarly_22 = NA, targ.perc.tooEarly_100_69_22 = NA,
                               nonTarg.perc.tooEarly_100 = NA, nonTarg.perc.tooEarly_69 = NA,
                               nonTarg.perc.tooEarly_22 = NA, nonTarg.perc.tooEarly_100_69_22 = NA,
                               targ.perc.tooLate_100 = NA, targ.perc.tooLate_69 = NA,
                               targ.perc.tooLate_22 = NA, targ.perc.tooLate_100_69_22 = NA,
                               nonTarg.perc.tooLate_100 = NA, nonTarg.perc.tooLate_69 = NA,
                               nonTarg.perc.tooLate_22 = NA, nonTarg.perc.tooLate_100_69_22 = NA,
                               targ.perc.noResp_100 = NA, targ.perc.noResp_69 = NA,
                               targ.perc.noResp_22 = NA, targ.perc.noResp_100_69_22 = NA,
                               nonTarg.perc.noResp_100 = NA, nonTarg.perc.noResp_69 = NA,
                               nonTarg.perc.noResp_22 = NA, nonTarg.perc.noResp_100_69_22 = NA)
  ##  define item codes
  item.codes <- list(targ_100 = seq(10,120,10),
                     targ_69 =seq(10,120,10)+1,
                     targ_22 = seq(10,120,10)+2,
                     nonTarg_100 = seq(10,120,10)+3,
                     nonTarg_69 = seq(10,120,10)+4,
                     nonTarg_22 = seq(10,120,10)+5,
                     block_1 = c(10:15,40:45,70:75,100:105),
                     block_2 = c(10:15,40:45,70:75,100:105)+10,
                     block_3 = c(10:15,40:45,70:75,100:105)+20
  )
  score.block.measures <- function(block.n.data,block.measures){
    ##  TODO  ##  refine this function so that I'm doing less filtering
    # calculate percent correct
    # cat('\ncalc percent correct')
    block.measures$targ.perc.corr_100 <-
      (block.n.data %>% filter(ItemCode %in% item.codes$targ_100 & ResponseType=="Target-Correct") %>% nrow()) / 
      (block.n.data %>% filter(ItemCode %in% item.codes$targ_100) %>% nrow())
    block.measures$targ.perc.corr_69 <- 
      (block.n.data %>% filter(ItemCode %in% item.codes$targ_69 & ResponseType=="Target-Correct") %>% nrow()) /
      (block.n.data %>% filter(ItemCode %in% item.codes$targ_69) %>% nrow())
    block.measures$targ.perc.corr_22 <- 
      (block.n.data %>% filter(ItemCode %in% item.codes$targ_22 & ResponseType=="Target-Correct") %>% nrow()) /
      (block.n.data %>% filter(ItemCode %in% item.codes$targ_22) %>% nrow())
    block.measures$targ.perc.corr_100_69_22 <- 
      (block.n.data %>% filter(ItemCode %in% c(item.codes$targ_100,item.codes$targ_22,item.codes$targ_69) &
                                 ResponseType=="Target-Correct") %>% nrow()) /
      (block.n.data %>% filter(ItemCode %in% c(item.codes$targ_100,item.codes$targ_22,item.codes$targ_69)) %>% nrow())
    # create vectors of targ correct RT measures
    targcorr.RT_100 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$targ_100 & ResponseType=="Target-Correct") %>% 
      select(Probe.all.RT) %>% unlist()
    targcorr.RT_69 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$targ_69 & ResponseType=="Target-Correct") %>% 
      select(Probe.all.RT) %>% unlist()
    targcorr.RT_22 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$targ_22 & ResponseType=="Target-Correct") %>% 
      select(Probe.all.RT) %>% unlist()
    # calculate means of RT measures
    # cat('\ncalc target correct RT means')
    block.measures$targcorr.mn.RT_100 <- mean(targcorr.RT_100)
    block.measures$targcorr.mn.RT_69 <- mean(targcorr.RT_69)
    block.measures$targcorr.mn.RT_22 <- mean(targcorr.RT_22)
    block.measures$targcorr.mn.RT_100_69_22 <- mean(c(targcorr.RT_100,targcorr.RT_22,targcorr.RT_69))
    # calculate medians of RT measures
    # cat('\ncalc target correct RT medians')
    block.measures$targcorr.med.RT_100 <- median(targcorr.RT_100)
    block.measures$targcorr.med.RT_69 <- median(targcorr.RT_69)
    block.measures$targcorr.med.RT_22 <- median(targcorr.RT_69)
    block.measures$targcorr.med.RT_100_69_22 <- median(c(targcorr.RT_100,targcorr.RT_69,targcorr.RT_22))
    # calculate percent target incorrect missed
    # cat('\ncalc target percent incorrect')
    block.measures$targ.perc.inc_100 <- 
      (block.n.data %>% 
      filter(ItemCode %in% item.codes$targ_100 & ResponseType=="Target-Incorrect-Missed") %>% nrow()) / 
      (block.n.data %>% filter(ItemCode %in% item.codes$targ_100) %>% nrow())
    block.measures$targ.perc.inc_69 <- 
      (block.n.data %>% 
         filter(ItemCode %in% item.codes$targ_69 & ResponseType=="Target-Incorrect-Missed") %>% nrow()) / 
      (block.n.data %>% filter(ItemCode %in% item.codes$targ_69) %>% nrow())
    block.measures$targ.perc.inc_22 <- 
      (block.n.data %>% 
         filter(ItemCode %in% item.codes$targ_22 & ResponseType=="Target-Incorrect-Missed") %>% nrow()) / 
      (block.n.data %>% filter(ItemCode %in% item.codes$targ_22) %>% nrow())
    block.measures$targ.perc.inc_100_69_22 <- 
      (block.n.data %>% 
         filter(ItemCode %in% c(item.codes$targ_100,item.codes$targ_69,item.codes$targ_22) &
                  ResponseType=="Target-Incorrect-Missed") %>% nrow()) / 
      (block.n.data %>% 
         filter(ItemCode %in% c(item.codes$targ_100,item.codes$targ_69,item.codes$targ_22)) %>% nrow())
    # create vectors of targ incorrect RT measures
    targinc.RT_100 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$targ_100 & ResponseType=="Target-Incorrect-Missed") %>% 
      select(Probe.all.RT) %>% unlist()
    targinc.RT_69 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$targ_69 & ResponseType=="Target-Incorrect-Missed") %>% 
      select(Probe.all.RT) %>% unlist()
    targinc.RT_22 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$targ_22 & ResponseType=="Target-Incorrect-Missed") %>% 
      select(Probe.all.RT) %>% unlist()
    # calculate means of target incorrect RT measures
    # cat('\ncalc target incorrect RT means')
    block.measures$targinc.mn.RT_100 <- mean(targinc.RT_100)
    block.measures$targinc.mn.RT_69 <- mean(targinc.RT_69)
    block.measures$targinc.mn.RT_22 <- mean(targinc.RT_22)
    block.measures$targinc.mn.RT_100_69_22 <- mean(c(targinc.RT_100,targinc.RT_69,targinc.RT_22))
    # calculate medians of target incorrect RT measures
    # cat('\ncalc target incorrect RT medians')
    block.measures$targinc.med.RT_100 <- median(targinc.RT_100)
    block.measures$targinc.med.RT_69 <- median(targinc.RT_69)
    block.measures$targinc.med.RT_22 <- median(targinc.RT_22)
    block.measures$targinc.med.RT_100_69_22 <- median(c(targinc.RT_100,targinc.RT_69,targinc.RT_22))
    # calculate non target percent correct
    # cat('\ncalc non target percent correct')
    block.measures$nonTarg.perc.corr_100 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_100 & ResponseType=="NonTarget-Correct") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_100) %>% nrow())
    block.measures$nonTarg.perc.corr_69 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_69 & ResponseType=="NonTarget-Correct") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_69) %>% nrow())
    block.measures$nonTarg.perc.corr_22 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_22 & ResponseType=="NonTarget-Correct") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_22) %>% nrow())
    block.measures$nonTarg.perc.corr_100_69_22 <- 
      (filter(block.n.data,ItemCode %in% c(item.codes$nonTarg_22,item.codes$nonTarg_69,item.codes$nonTarg_100) &
                ResponseType=="NonTarget-Correct") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% c(item.codes$nonTarg_22,item.codes$nonTarg_69,item.codes$nonTarg_100)) %>% 
               nrow())
    # create vectors of non targ correct RT measures
    nonTarg.corr.RT_100 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$nonTarg_100 & ResponseType=="NonTarget-Correct") %>% 
      select(Probe.all.RT) %>% unlist()
    nonTarg.corr.RT_69 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$nonTarg_69 & ResponseType=="NonTarget-Correct") %>% 
      select(Probe.all.RT) %>% unlist()
    nonTarg.corr.RT_22 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$nonTarg_22 & ResponseType=="NonTarget-Correct") %>% 
      select(Probe.all.RT) %>% unlist()
    nonTarg.corr.RT_100_69_22 <- c(nonTarg.corr.RT_100,nonTarg.corr.RT_69,nonTarg.corr.RT_22)
    # calculate means of non target correct RT measures
    #cat('\ncalc non target correct RT means')
    block.measures$nonTargcorr.mn.RT_100 <- mean(nonTarg.corr.RT_100)
    block.measures$nonTargcorr.mn.RT_69 <- mean(nonTarg.corr.RT_69)
    block.measures$nonTargcorr.mn.RT_22 <- mean(nonTarg.corr.RT_22)
    block.measures$nonTargcorr.mn.RT_100_69_22 <- mean(nonTarg.corr.RT_100_69_22)
    # calculate medians of non target correct RT measures
    #cat('\ncalc non target correct RT medians')
    block.measures$nonTargcorr.med.RT_100 <- median(nonTarg.corr.RT_100)
    block.measures$nonTargcorr.med.RT_69 <- median(nonTarg.corr.RT_69)
    block.measures$nonTargcorr.med.RT_22 <- median(nonTarg.corr.RT_22)
    block.measures$nonTargcorr.med.RT_100_69_22 <- median(nonTarg.corr.RT_100_69_22)
    # calculate non target percent incorrect False Alarm
    #cat('\ncalc non targ percent FA')
    block.measures$nonTarg.perc.inc.FA_100 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_100 & ResponseType=="NonTarget-FalseAlarm") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_100) %>% nrow())
    block.measures$nonTarg.perc.inc.FA_69 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_69 & ResponseType=="NonTarget-FalseAlarm") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_69) %>% nrow())
    block.measures$nonTarg.perc.inc.FA_22 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_22 & ResponseType=="NonTarget-FalseAlarm") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_22) %>% nrow())
    block.measures$nonTarg.perc.inc.FA_100_69_22 <- 
      (filter(block.n.data,ItemCode %in% c(item.codes$nonTarg_100,item.codes$nonTarg_69,item.codes$nonTarg_22)
              & ResponseType=="NonTarget-FalseAlarm") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% c(item.codes$nonTarg_100,item.codes$nonTarg_69,item.codes$nonTarg_22)) %>%
         nrow())
    # create vectors of non target incorrect False Alarms
    nonTarginc.FA.RT_100 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$nonTarg_100 & ResponseType=="NonTarget-FalseAlarm") %>% 
      select(Probe.all.RT) %>% unlist()
    nonTarginc.FA.RT_69 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$nonTarg_69 & ResponseType=="NonTarget-FalseAlarm") %>% 
      select(Probe.all.RT) %>% unlist()
    nonTarginc.FA.RT_22 <- block.n.data %>% 
      filter(ItemCode %in% item.codes$nonTarg_22 & ResponseType=="NonTarget-FalseAlarm") %>% 
      select(Probe.all.RT) %>% unlist()
    nonTarginc.FA.RT_100_69_22 <- c(nonTarginc.FA.RT_100,nonTarginc.FA.RT_69,nonTarginc.FA.RT_22)
    # calculate means of non target incorrect False Alarm RT measures
    #cat('\ncalc non targ FA RT means')
    block.measures$nonTarginc.FA.mn.RT_100 <- mean(nonTarginc.FA.RT_100)
    block.measures$nonTarginc.FA.mn.RT_69 <- mean(nonTarginc.FA.RT_69)
    block.measures$nonTarginc.FA.mn.RT_22 <- mean(nonTarginc.FA.RT_22)
    block.measures$nonTarginc.FA.mn.RT_100_69_22 <- mean(nonTarginc.FA.RT_100_69_22)
    # calculate medians of non target incorrect False Alarm RT measures
    #cat('\ncalc non targ FA RT medians')
    block.measures$nonTarginc.FA.med.RT_100 <- median(nonTarginc.FA.RT_100)
    block.measures$nonTarginc.FA.med.RT_69 <- median(nonTarginc.FA.RT_69)
    block.measures$nonTarginc.FA.med.RT_22 <- median(nonTarginc.FA.RT_22)
    block.measures$nonTarginc.FA.med.RT_100_69_22 <- median(nonTarginc.FA.RT_100_69_22)
    # calculate percentages of target too early
    #cat('\ncalc percent target too early')
    block.measures$targ.perc.tooEarly_100 <-
      (filter(block.n.data,ItemCode %in% item.codes$targ_100 & ResponseType=="Incorrect-TooEarly") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$targ_100) %>% nrow())
    block.measures$targ.perc.tooEarly_69 <- 
      (filter(block.n.data,ItemCode %in% item.codes$targ_69 & ResponseType=="Incorrect-TooEarly") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$targ_69) %>% nrow())
    block.measures$targ.perc.tooEarly_22 <-
      (filter(block.n.data,ItemCode %in% item.codes$targ_22 & ResponseType=="Incorrect-TooEarly") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$targ_22) %>% nrow())
    block.measures$targ.perc.tooEarly_100_69_22 <- 
      (filter(block.n.data,ItemCode %in% c(item.codes$targ_100,item.codes$targ_69,item.codes$targ_22) &
                ResponseType=="Incorrect-TooEarly") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% c(item.codes$targ_100,item.codes$targ_69,item.codes$targ_22)) %>% nrow())
    # calculate percentages of non target too early
    #cat("\ncalc percent non target too early")
    block.measures$nonTarg.perc.tooEarly_100 <-
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_100 & ResponseType=="Incorrect-TooEarly") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_100) %>% nrow())
    block.measures$nonTarg.perc.tooEarly_69 <-
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_69 & ResponseType=="Incorrect-TooEarly") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_69) %>% nrow())
    block.measures$nonTarg.perc.tooEarly_22 <-
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_22 & ResponseType=="Incorrect-TooEarly") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_22) %>% nrow())
    block.measures$nonTarg.perc.tooEarly_100_69_22 <- 
      (filter(block.n.data,ItemCode %in% c(item.codes$nonTarg_100,item.codes$nonTarg_69,item.codes$nonTarg_22) &
                ResponseType=="Incorrect-TooEarly") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% c(item.codes$nonTarg_100,item.codes$nonTarg_69,item.codes$nonTarg_22)) %>% nrow())
    # calculate percentages of target too late
    #cat("\ncalc target percent too late")
    block.measures$targ.perc.tooLate_100 <- 
      (filter(block.n.data,ItemCode %in% item.codes$targ_100 & ResponseType=="Incorrect-TooLate") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$targ_100) %>% nrow())
    block.measures$targ.perc.tooLate_69 <- 
      (filter(block.n.data,ItemCode %in% item.codes$targ_69 & ResponseType=="Incorrect-TooLate") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$targ_69) %>% nrow())
    block.measures$targ.perc.tooLate_22 <- 
      (filter(block.n.data,ItemCode %in% item.codes$targ_22 & ResponseType=="Incorrect-TooLate") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$targ_22) %>% nrow())
    block.measures$targ.perc.tooLate_100_69_22 <- 
      (filter(block.n.data,ItemCode %in% c(item.codes$targ_100,item.codes$targ_69,item.codes$targ_22) &
                ResponseType=="Incorrect-TooLate") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% c(item.codes$targ_100,item.codes$targ_69,item.codes$targ_22)) %>% nrow())
    # calculate percentages of non target too late
    #cat('\ncalc non target percent too late')
    block.measures$nonTarg.perc.tooLate_100 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_100 & ResponseType=="Incorrect-TooLate") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_100) %>% nrow())
    block.measures$nonTarg.perc.tooLate_69 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_69 & ResponseType=="Incorrect-TooLate") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_69) %>% nrow())
    block.measures$nonTarg.perc.tooLate_22 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_22 & ResponseType=="Incorrect-TooLate") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_22) %>% nrow())
    block.measures$nonTarg.perc.tooLate_100_69_22 <- 
      (filter(block.n.data,ItemCode %in% c(item.codes$nonTarg_100,item.codes$nonTarg_69,item.codes$nonTarg_22) &
                ResponseType=="Incorrect-TooLate") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% c(item.codes$nonTarg_100,item.codes$nonTarg_69,item.codes$nonTarg_22)) %>% 
         nrow())
    # calculate target percent no response
    #cat('\ncalc target percent no response')
    block.measures$targ.perc.noResp_100 <- 
      (filter(block.n.data,ItemCode %in% item.codes$targ_100 & ResponseType=="Target-NoResponse") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$targ_100) %>% nrow())
    block.measures$targ.perc.noResp_69 <- 
      (filter(block.n.data,ItemCode %in% item.codes$targ_69 & ResponseType=="Target-NoResponse") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$targ_69) %>% nrow())
    block.measures$targ.perc.noResp_22 <- 
      (filter(block.n.data,ItemCode %in% item.codes$targ_22 & ResponseType=="Target-NoResponse") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$targ_22) %>% nrow())
    block.measures$targ.perc.noResp_100_69_22 <- 
      (filter(block.n.data,ItemCode %in% c(item.codes$targ_100,item.codes$targ_69,item.codes$targ_22) &
                ResponseType=="Target-NoResponse") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% c(item.codes$targ_100,item.codes$targ_69,item.codes$targ_22)) %>% nrow())
    # calculate non target percent no response
    #cat('\ncalc non target percent no response')
    block.measures$nonTarg.perc.noResp_100 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_100 & ResponseType=="Target-NoResponse") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_100) %>% nrow())
    block.measures$nonTarg.perc.noResp_69 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_69 & ResponseType=="Target-NoResponse") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_69) %>% nrow())
    block.measures$nonTarg.perc.noResp_22 <- 
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_22 & ResponseType=="Target-NoResponse") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% item.codes$nonTarg_22) %>% nrow())
    block.measures$nonTarg.perc.noResp_100_69_22 <- 
      (filter(block.n.data,ItemCode %in% c(item.codes$nonTarg_100,item.codes$nonTarg_69,item.codes$nonTarg_22) &
                ResponseType=="Target-NoResponse") %>% nrow()) /
      (filter(block.n.data,ItemCode %in% c(item.codes$nonTarg_100,item.codes$nonTarg_69,item.codes$nonTarg_22)) %>% 
         nrow())
    return(block.measures)
}
  ##  Add measures data frame to each sublist in beh.data
  beh.data.initial.len <- length(beh.data)
  for(i in 1:beh.data.initial.len){
    ##  use function to score all trials within a given block
    beh.data[[i]]$summary.measures <- list(block.1=score.block.measures((beh.data[[i]]$log.frames.df %>%
                                                                          filter(ItemCode %in% item.codes$block_1)),
                                                                        block.measures),
                                           block.2=score.block.measures((beh.data[[i]]$log.frames.df %>%
                                                                          filter(ItemCode %in% item.codes$block_2)),
                                                                        block.measures),
                                           block.3=score.block.measures((beh.data[[i]]$log.frames.df %>%
                                                                          filter(ItemCode %in% item.codes$block_3)),
                                                                        block.measures),
                                           block.all=score.block.measures(beh.data[[i]]$log.frames.df,
                                                                          block.measures))
    ## beh.data[[length(beh.data)+1]] should be all other pairing runs within beh.data, bound together
    if(i==1){
      cat('\ni is 1. setting beh.data[[',beh.data.initial.len+1,']] as beh.data[[1]]')
      beh.data[[beh.data.initial.len+1]] <- beh.data[[i]]
      beh.data[[beh.data.initial.len+1]]$DataFile.Basename <- 
        beh.data[[beh.data.initial.len+1]]$header.frame$DataFile.Basename
      beh.data[[beh.data.initial.len+1]]$header.frame <- NULL
    }
    else if(i<=beh.data.initial.len){
      cat('\ni is',i,'binding  beh.data[[',beh.data.initial.len+1,']] with beh.data[[',i,']]')
      beh.data[[beh.data.initial.len+1]]$log.frames.df <- bind_rows(beh.data[[beh.data.initial.len+1]]$log.frames.df,
                                                                    beh.data[[i]]$log.frames.df)
      beh.data[[beh.data.initial.len+1]]$DataFile.Basename <- c(beh.data[[beh.data.initial.len+1]]$DataFile.Basename,
                                                                beh.data[[i]]$header.frame$DataFile.Basename)
    }
    names(beh.data)[beh.data.initial.len+1] <- "all.pairings"
  }
  ##### score data from all pairings #####
  cat('\nscoring summary measures for ',names(beh.data)[beh.data.initial.len+1])
  beh.data[[beh.data.initial.len+1]]$summary.measures <- list(block.1=score.block.measures((beh.data[[beh.data.initial.len+1]]$log.frames.df %>%
                                                                                              filter(ItemCode %in% item.codes$block_1)),
                                                                                           block.measures),
                                                              block.2=score.block.measures((beh.data[[beh.data.initial.len+1]]$log.frames.df %>%
                                                                                              filter(ItemCode %in% item.codes$block_2)),
                                                                                           block.measures),
                                                              block.3=score.block.measures((beh.data[[beh.data.initial.len+1]]$log.frames.df %>%
                                                                                              filter(ItemCode %in% item.codes$block_3)),
                                                                                           block.measures),
                                                              block.all=score.block.measures(beh.data[[beh.data.initial.len+1]]$log.frames.df,
                                                                                             block.measures))
  ##### return #####
  return(beh.data)
}
