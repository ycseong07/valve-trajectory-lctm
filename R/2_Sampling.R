#' Sampling
#'
#' @details
#' Data Sampling
#' 
#' @param echo    echo data
#' 
#' @import tidyverse
#'
#' @export


Sampling<-function(echo)
{
  
  if (is.null(echo)){
  echo<-PreProcessing()
  }
  
  sample <- echo %>%
    filter(avStructure=="Native") %>% 
    filter(sex!="") %>% 
    filter(!is.na(age)) %>% 
    group_by(ptno) %>%
    arrange(ptno, studySeq) %>%
    filter(!is.na(avarMeanPressureGradient)) %>% 
    filter(cummax(between(avarMeanPressureGradient, 20,40)) > 0)
  
  avrSurgical_list  <-  read.csv("./data/avr_sur_list.csv", stringsAsFactors = F)
  names(avrSurgical_list) <- c("ptno","birthDate","sur_date")
  avrSurgical_list$sur_date <- as.Date(avrSurgical_list$sur_date)
  avrSurgical_list$birthDate <- as.Date(avrSurgical_list$birthDate)
  avrSurgical_list$ptno <- as.numeric(avrSurgical_list$ptno)
  
  avrSurgical_list<- avrSurgical_list %>%
    group_by(ptno) %>%
    arrange(sur_date) %>%
    filter(row_number()==1)
  
  tavi_list  <-  read.csv("./data/tavi_list.csv", stringsAsFactors = F)
  names(tavi_list) <- c("ptno","birthDate","sur_date")
  tavi_list$sur_date <- as.Date(tavi_list$sur_date)
  tavi_list$birthDate <- as.Date(tavi_list$birthDate)
  tavi_list$ptno <- as.numeric(tavi_list$ptno)
  
  tavi_list<- tavi_list %>%
    group_by(ptno) %>%
    arrange(sur_date) %>%
    filter(row_number()==1)
  
  avr_list<- rbind(avrSurgical_list, tavi_list) %>%
    group_by(ptno) %>%
    arrange(sur_date) %>%
    filter(row_number()==1)
  
  names(avr_list) <- c("ptno","birthDate","avr_date")
  
  names(avrSurgical_list) <- c("ptno","birthDate","avr_sur_date")
  
  names(tavi_list) <- c("ptno","birthDate","tavi_date")
  
  sample<-left_join(sample, avr_list, by=c('ptno', 'birthDate'))
  sample<-left_join(sample, avrSurgical_list, by=c('ptno', 'birthDate'))
  sample<-left_join(sample, tavi_list, by=c('ptno', 'birthDate'))
  
  sample<-sample %>%
    mutate(avrSurgical = ifelse(!is.na(avr_sur_date), 1, 0))
  
  sample<-sample %>%
    mutate(tavi = ifelse(!is.na(tavi_date), 1, 0))
  
  sample<-sample %>%
    mutate(avr = ifelse(!is.na(avr_date), 1, 0))

  # avr list에 있는 환자들 중 avr_date보다 studydate가 늦은 obs 제외
  
  tmp1 <- sample[is.na(sample$avr_date),] 
 
  tmp2 <- sample[!is.na(sample$avr_date),] %>%
    filter(avr_date > studyDate) 
   
  sample<- rbind(tmp1, tmp2)
  
  ## (마지막 echo - 첫 echo) < 90일 제외
  
  sample <- sample %>% 
    group_by(ptno) %>%
    arrange(ptno, studySeq) %>%
    filter( (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) >= 90) 
  
  ## avr, tavi, bentall이 있을경우 그 이후 obs는 제거, 3번이상 추적만 추출
  
  sample <- sample %>%
    group_by(ptno) %>%
    arrange(studySeq) %>%
    mutate(containAVR = ifelse(grepl ("[Ss]\\/[Pp].{0,2}[Aa][Vv][Rr]",commentForFixedLines), 1, 0))
  
  sample<-sample[ ave(sample$containAVR, sample$ptno, FUN=function(x) x>=cummax(x))==1, ]

  sample <- sample %>%
    group_by(ptno) %>%
    arrange(studySeq) %>%
    mutate(containTAVI = ifelse(grepl ("[Ss]\\/[Pp].{0,2}[Tt][Aa][Vv][Ii]",commentForFixedLines), 1, 0))
  
  sample<-sample[ ave(sample$containTAVI, sample$ptno, FUN=function(x) x>=cummax(x))==1, ]
  
  sample <- sample %>%
    group_by(ptno) %>%
    arrange(studySeq) %>%
    mutate(containBEN = ifelse(grepl ("[Ss]\\/[Pp].{0,2}[Bb][Ee][Nn][Tt][Aa][Ll][Ll]",commentForFixedLines), 1, 0))
  
  sample<-sample[ ave(sample$containBEN, sample$ptno, FUN=function(x) x>=cummax(x))==1, ]
  
  sample <- sample %>% 
    group_by(ptno) %>%
    arrange(ptno, studySeq) %>%
    filter(length(avarMeanPressureGradient)>=3)
  
 
  ## death
  death_list  <-  read.csv("./data/death_list.csv", stringsAsFactors = F)
  
  death_list$death_1<-as.Date(death_list$death_1)
  death_list$death_2<-as.Date(death_list$death_2)
  
  death_list<-death_list %>%
    mutate(death_date = case_when(!is.na(death_1) ~ death_1,
                                  is.na(death_1) ~ death_2))
  
  death_list<-death_list%>%
    mutate(death = ifelse(!is.na(death_date), 1, 0))
  
  death_list<-death_list %>%
    select(ptno, death_date, death)
  
  
  sample <- sample[,-which(names(sample) %in% c("death"))]
  
  
  sample<-left_join(sample,death_list,by='ptno')


  
  # AV MSPG log
  sample<-sample %>%
    mutate(avarMeanPressureGradientLog = log(avarMeanPressureGradient))
  
  sample<-sample %>%
    filter(!is.na(avarMeanPressureGradientLog))
  
  return(sample)
  
}


RandomSampling<-function(echo,
                         num_of_pat = 100)
{
  if (is.null(echo)){
    echo<-Sampling()
  }
  
  set.seed(100)
  
  ptno_random <- echo[sample(nrow(echo), num_of_pat), ]
  
  sample_random <- echo %>%
    filter(!is.na(tvRegur)) %>%
    filter(ptno %in% ptno_random$ptno)
  
  return(sample_random)
}















