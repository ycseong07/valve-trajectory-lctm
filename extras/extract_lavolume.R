require(dplyr)

as_obs <- read.csv("./data/Preprocessed_sex_birth_death_uft8.csv", stringsAsFactors = F)
cxrlist<-readxl::read_excel("./data/cxrlist.xlsx")

##  echo studydate-90일 < cxrlsit의 날짜 <= echo studydate+30일 인 la volume 붙이기

as_obs$studyDate<-as.Date(as_obs$studyDate)

cxrlist$cxrDate = format(as.Date(cxrlist$cxrDate, "%m/%d/%Y"), "20%Y-%m-%d")
cxrlist$cxrDate = as.Date(cxrlist$cxrDate)


lav<-as_obs %>% select(ptno, studyDate, laVolumeIndexBymMode,laVolumeIndexBymMode_1)

lav<- lav %>% mutate(studyDate_b90 = studyDate-90)
lav<- lav %>% mutate(studyDate_a30 = studyDate+30)

str(lav)
str(cxrlist)

lav<-rename(lav,c("ptno_enc" = "ptno"))

# library(sqldf)
# res<-sqldf("SELECT a.ptno, a.ptno_enc, a.cxrDate, a.ms, b.studyDate, b.laVolumeIndexBymMode, b.laVolumeIndexBymMode_1, b.studyDate_b90, b.studyDate_a30
#             FROM cxrlist a
#             LEFT JOIN lav b
#             ON  a.ptno_enc  = b.ptno_enc
#             AND a.cxrDate BETWEEN b.studyDate_b90 AND b.studyDate_a30")


res <- left_join(cxrlist, lav, by = c("ptno_enc" = "ptno_enc")) %>% filter(cxrDate > studyDate_b90, cxrDate <= studyDate_a30)


##


tmp <- res %>%
  select(ptno,ptno_enc, cxrDate,studyDate,laVolumeIndexBymMode, laVolumeIndexBymMode_1, ms, pacsNo) %>%
  mutate(la_volume = ifelse(laVolumeIndexBymMode_1 !="", laVolumeIndexBymMode_1, laVolumeIndexBymMode)) %>%
  select(ptno,ptno_enc, pacsNo, cxrDate, studyDate, la_volume, ms)
## 11431

## la volume이 없으면 지우고
tmp <- tmp %>% filter(!is.na(la_volume)) %>% filter(la_volume != "")
## 11035

## id가 중복이면 |cxrDate - studyDate|가 최소인 것만 남기기
tmp <- tmp %>%
  group_by(ptno) %>%
  arrange(abs(cxrDate - studyDate)) %>%
  filter(!duplicated(ptno))
  

write.csv(tmp,"ptno_lavolume_merged.csv")

library('tidyverse')
tmp[str_detect(tmp$la_volume,"-"),]
