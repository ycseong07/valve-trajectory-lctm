library(SevEchoLctm)
library(rmarkdown)
library(dplyr)

#### analysis
# sample<-DataAccess(csvFilePath = "./data/Preprocessed_sex_birth_death_uft8.csv")
# 
# sample<-PreProcessing(sample)
# 
# sample<-Sampling(sample)

#sample<-RandomSampling(sample)

#### Raw data was modified. Run codes below. Same results of codes above ####
as  <-  read.csv("./data/AS_rawdata_patients.csv", stringsAsFactors = F)
as_obs <- read.csv("./data/AS_rawdata_echo.csv", stringsAsFactors = F)
sample<-as_obs
# as_obs <- as_obs %>% 
#   group_by(ptno) %>%
#   arrange(ptno, studySeq) %>%
#   filter( (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) >= 90) %>%
#   filter(length(avarMeanPressureGradient)>=3)
#   
# as <- as[as$ptno %in% as_obs$ptno,]
# 
# as<-subset(as,select = -c(group2, group3, group4, isGroup2))
# as_obs<-subset(as_obs,select = -c(group2, group3, group4))
# 
# sample<-as_obs



#### end ####

# LCTMLinear(seed_num=100,sample)
LCTMLinearAddCV(seed_num=100,sample)

## plotting model 2 ~ 4 only
LCTMPlot(sample,fileName="LCTM_plot_model") # save plots in 'plot' folder
