library(BSDA)
library(pROC)
library(Epi)
require(dplyr)
require(ggplot2)
require(gridExtra)
library(survival) 
library(survminer)
library(epiR)
library(fmsb)
library(rms)
library(splines)
library(lcmm)
library(lubridate)

# devtools::install_github("hlennon/LCTMtools", force = T)
library(LCTMtools)


############ Strat from here ############

#### Data preparation ####
# save(modelLCV_1, modelLCV_2, modelLCV_3, modelLCV_4, modelLCV_5, modelLCV_6, file = "./data/latest_model.rds")
# load(file = "./data/latest_model.rds")

as  <-  read.csv("./data/AS_rawdata_patients.csv", stringsAsFactors = F)
as_obs <- read.csv("./data/AS_rawdata_echo.csv", stringsAsFactors = F)

####  Customize latent class trajectory plot ####

as_obs$group2 <- as.factor(as_obs$group2)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#plot check
p1 <- ggplot(as_obs, aes(x=(diffInDay/365), y=avarMeanPressureGradient, group = ptno, colour = group2)) +
  geom_line() +
  geom_smooth(aes(group=group2), method = "loess", size = 2, se = F)  +
  scale_y_continuous(limits = c(10,70)) +
  theme(text = element_text(size=15), panel.background = element_blank(), axis.line = element_line(colour = "grey")) +
  labs(x = "Year",y = "MPG", colour = "Latent Class", title = "(A) Raw") +
  scale_colour_manual(labels=c("Trajectory 1 (Slow progression)", "Trajectory 2 (Rapid progression)"), values = c("blue","red")) +
  theme(legend.position = "bottom", legend.box = "horizontal")

p2 <- ggplot(as_obs, aes(x=(diffInDay/365), y=avarMeanPressureGradient, group = ptno, colour = group2)) +
  geom_smooth(aes(group = ptno, colour = group2), size = 0.5, se = F) +
  geom_smooth(aes(group = group2), method = "loess", size = 2, se = T)  +
  scale_y_continuous(limits = c(10,70)) +
  theme(text = element_text(size=15), panel.background = element_blank(), axis.line = element_line(colour = "grey")) +
  labs(x = "Year",y = "MPG",colour = "Latent Class", title = "(B) Smoothed") +
  scale_colour_manual(labels=c("Trajectory 1 (Slow progression)", "Trajectory 2 (Rapid progression)"), values = c("blue","red")) +
  theme(legend.position = "none")

legend <- get_legend(p1)

## legend.position to "none" after extract legend
p1 <- ggplot(as_obs, aes(x=(diffInDay/365), y=avarMeanPressureGradient, group = ptno, colour = group2)) +
  geom_line(alpha=0.3) +
  # geom_smooth(aes(group=group2), method = "loess", size = 2, se = F)  +
  scale_y_continuous(limits = c(10,70)) +
  theme(text = element_text(size=15), panel.background = element_blank(), axis.line = element_line(colour = "grey")) +
  labs(x = "Year",y = "MPG", colour = "Latent Class", title = "(A) Raw") +
  scale_colour_manual(labels=c("Trajectory 1 (Slow progression)", "Trajectory 2 (Rapid progression)"), values = c("blue","red")) +
  theme(legend.position = "none", legend.box = "horizontal")

g <- arrangeGrob(p1, p2, legend, 
                 layout_matrix=rbind(c(1,1,2,2), c(1,1,2,2), c(1,1,2,2), c(1,1,2,2),c(1,1,2,2), c(3))) #generates g

ggsave(file="./plot/model_2.tiff", g, width=23, height=12, units="cm", dpi = 300) #saves g
# ggsave(file="./plot/model_2.png", g, width=23, height=12, units="cm") #saves g



#### Compare models ####

## model 1
modelLCV_2$BIC

tmp<-postprob(modelLCV_2)[[1]]
paste0(tmp[2,1]," : ",tmp[2,2])

tmp<-postprob(modelLCV_2)[[2]]
paste0(tmp[1,1]," : ",tmp[2,2])

tmp<-LCTMtoolkit(modelLCV_2)$relativeentropy
tmp

## model 2
modelLCV_3$BIC

tmp<-postprob(modelLCV_3)[[1]]
paste0(tmp[2,1]," : ",tmp[2,2]," : ",tmp[2,3])

tmp<-postprob(modelLCV_3)[[2]]
paste0(tmp[1,1]," : ",tmp[2,2], " : ",tmp[3,3])

tmp<-LCTMtoolkit(modelLCV_3)$relativeentropy
tmp

## model 3
modelLCV_4$BIC

tmp<-postprob(modelLCV_4)[[1]]
paste0(tmp[2,1]," : ",tmp[2,2]," : ",tmp[2,3]," : ",tmp[2,4])

tmp<-postprob(modelLCV_4)[[2]]
paste0(tmp[1,1]," : ",tmp[2,2], " : ",tmp[3,3], " : ",tmp[4,4])

tmp<-LCTMtoolkit(modelLCV_4)$relativeentropy
tmp

## model 4
modelLCV_5$BIC

tmp<-postprob(modelLCV_5)[[1]]
paste0(tmp[2,1]," : ",tmp[2,2]," : ",tmp[2,3]," : ",tmp[2,4]," : ",tmp[2,5])

tmp<-postprob(modelLCV_5)[[2]]
paste0(tmp[1,1]," : ",tmp[2,2], " : ",tmp[3,3], " : ",tmp[4,4], " : ",tmp[5,5])

tmp<-LCTMtoolkit(modelLCV_5)$relativeentropy
tmp

## model 5
modelLCV_6$BIC

tmp<-postprob(modelLCV_6)[[1]]
paste0(tmp[2,1]," : ",tmp[2,2]," : ",tmp[2,3]," : ",tmp[2,4]," : ",tmp[2,5]," : ",tmp[2,6])

tmp<-postprob(modelLCV_6)[[2]]
paste0(tmp[1,1]," : ",tmp[2,2], " : ",tmp[3,3], " : ",tmp[4,4], " : ",tmp[5,5], " : ",tmp[6,6])

tmp<-LCTMtoolkit(modelLCV_6)$relativeentropy
tmp


#### Make some variables for analysis ####

## bmi
as$weight<-as.numeric(as$weight)
as$height<-as.numeric(as$height)
as$bmi <- as$weight / ((as$height/100)^2)

## bmi_i
as$bmi25 <- ifelse(as$bmi > 25, 1, 0)

## age/10
as$age10<-as$age/10

## f/u days
as <- as %>%
  mutate(fuDays = as.Date(as$lastFUDay, format="%Y-%m-%d") - as.Date(as$studyDate, format="%Y-%m-%d"))

## CAD 수정 -> CAD_re
as <- as %>% 
  mutate(CAD_re = case_when(
    CAD==1 |CABG==1|MI==1 ~ 1,
    TRUE ~ 0
  )) 

## CAD 수정 -> CAD_re
as <- as %>% 
  mutate(AVA_re = case_when(
    !is.na(as$avaByContinuityEquation) ~ as$avaByContinuityEquation,
    is.na(as$avaByContinuityEquation) ~ as$avaByPlanimetry
  )) 

## Mean interscan interval
as_obs$studyDate<-as.Date(as_obs$studyDate, format="%Y-%m-%d")

## interscan interval, f/u
tmp<-as_obs %>%
  group_by(ptno) %>%
  # filter(group2 ==1) %>%
  mutate(mean_interscan_interval = (last(studyDate) - first(studyDate)) / (n()-1) ) %>%
  mutate(follow_up = last(studyDate) - first(studyDate) ) %>%
  select(ptno, mean_interscan_interval, follow_up) %>%
  distinct(ptno, mean_interscan_interval,follow_up, .keep_all = T)


as<-left_join(as,tmp, by="ptno")
rm(tmp)

## add composite outcome (date)

as<-as%>%
  mutate(compOut = case_when(death==1 ~ 1,
                             avr ==1 ~ 1,
                             avrSurgical==1 ~ 1,
                             tavi==1 ~ 1,
                             TRUE ~ 0))

as<-as%>%
  mutate(compOut_date = pmin(death_date, avr_date, avr_sur_date, tavi_date, na.rm = TRUE))

## isGroup2
as <- as %>%
  mutate(isGroup2 = ifelse(group2==2, 1, 0))

## class weight

as <- as %>%
  mutate(class_weight = ifelse(isGroup2==0, 306/686, 380/686))

## to date format
as$death_date <-as.Date(as$death_date, format="%Y-%m-%d")
as$avr_date <-as.Date(as$avr_date, format="%Y-%m-%d")
as$avr_sur_date <-as.Date(as$avr_sur_date, format="%Y-%m-%d")
as$tavi_date <-as.Date(as$tavi_date, format="%Y-%m-%d")
as$compOut_date <-as.Date(as$compOut_date, format="%Y-%m-%d")
as$studyDate <-as.Date(as$studyDate, format="%Y-%m-%d")
as$lastFUDay <-as.Date(as$lastFUDay, format="%Y-%m-%d")

#### Make descriptive table ####

## make tmpDF for save raw data
tmpDF<-as

group1N<-nrow(as[as$group2==1,]) ## 306
group2N<-nrow(as[as$group2==2,]) ## 380
totalNum<-group1N+group2N ## 686

## check 2x2 table if needed
# varname = "Hypertension"
# xtabs(~ eval(as.name(varname)) + group2 , as)

### check Echocardiography Findings
# tmpDF %>%
#   # filter(group2==1) %>%
#   pull(LVEF)%>%
#   mean(na.rm=T)
# 
# tmpDF %>%
#   # filter(group2==2) %>%
#   pull(LVEF)%>%
#   sd(na.rm=T)

## for dummy variables

as_dataframe <- data.frame(matrix(ncol = 6, nrow = 0))
names(as_dataframe) <- c("Var","G1_mean_N","G1_sd_per","G2_mean_N","G2_sd_per", "p-value")

list <- c("sexDummy", "bmi25","Hypertension","Dyslipidemia","DM","PAOD","Stroke","Renal.dysfunction",
          "COPD","CAD_re","CABG","HF", "AF", "Rheumatic", "Bicuspid.AV", "Concomittant.AR")

for (varname in list){
  Var<-varname
  
  a<-tmpDF %>%
    filter(group2==1) %>%
    filter(!!as.name(varname)==1) %>%
    nrow()
  
  b<-a/group1N*100
  
  c<-tmpDF %>%
    filter(group2==2) %>%
    filter(!!as.name(varname)==1) %>%
    nrow()
  
  d<-c/group2N*100
  
  tmp_table <- xtabs(~ eval(as.name(varname)) + group2 , tmpDF)
  
  e<-chisq.test(tmp_table, correct=FALSE)[3]
  
  tempdf<-data.frame(Var, a,b,c,d,e)
  names(tempdf) <- c("Var","G1_mean_N","G1_sd_per","G2_mean_N","G2_sd_per", "p-value")
  as_dataframe<-rbind(as_dataframe, tempdf)
}

# write.csv(as_dataframe, "./descriptive.csv")

## Total
as_dataframe <- data.frame(matrix(ncol = 3, nrow = 0))
names(as_dataframe) <- c("Var","G1_mean_N","G1_sd_per")


for (varname in list){
  Var<-varname
  
  a<-tmpDF %>%
    filter(!!as.name(varname)==1) %>%
    nrow()
  
  b<-a/totalNum*100
  
  tempdf<-data.frame(Var, a,b)
  names(tempdf) <- c("Var","G1_mean_N","G1_sd_per")
  as_dataframe<-rbind(as_dataframe, tempdf)
}

tempdf<-data.frame(Var, a,b)
names(tempdf) <- c("Var","G1_mean_N","G1_sd_per")

## for continuous variables
as_dataframe <- data.frame(matrix(ncol = 6, nrow = 0))
names(as_dataframe) <-  c("Var","G1_mean_N","G1_sd_per","G2_mean_N","G2_sd_per", "p-value")

list <- c("age", "mean_interscan_interval", "avarMeanPressureGradient","avarPeakPressureGradient","avPeakVelocity", "AVA_re",
          "lvotvti","LVEF", "lviddBymMode","lvidsBymMode")

# mean
for (varname in list){
  Var<-varname

  a<-tmpDF %>%
    # filter(group2==1) %>%
    pull(!!as.name(varname))%>%
    mean(na.rm=T)

  b<-tmpDF %>%
    # filter(group2==1) %>%
    pull(!!as.name(varname))%>%
    sd(na.rm=T)

  c<-tmpDF %>%
    filter(group2==2) %>%
    pull(!!as.name(varname))%>%
    mean(na.rm=T)

  d<-tmpDF %>%
    filter(group2==2) %>%
    pull(!!as.name(varname))%>%
    sd(na.rm=T)

  e<-tsum.test(mean.x=a ,   s.x=b, n.x=group1N,
               mean.y=c , s.y=d, n.y=group2N)$p.value

  tempdf<-data.frame(Var, a,b,c,d,e)
  names(tempdf) <- c("Var","G1_mean_N","G1_sd_per","G2_mean_N","G2_sd_per", "p-value")
  as_dataframe<-rbind(as_dataframe, tempdf)
}

# ## median
# as_dataframe <- data.frame(matrix(ncol = 8, nrow = 0))
# names(as_dataframe) <- c("Var","G1_median","G1_q25", "G1_q75", "G2_median","G2_q25", "G2_q75","p-value")
# 
# for (varname in list){
#   Var<-varname
#   
#   a<-tmpDF %>%
#     filter(group2==1) %>%
#     pull(!!as.name(varname))%>%
#     median(na.rm=T)
#   
#   b<-tmpDF %>%
#     filter(group2==1) %>%
#     pull(!!as.name(varname))%>%
#     quantile(., 0.25, na.rm = T)
#   
#   c<-tmpDF %>%
#     filter(group2==1) %>%
#     pull(!!as.name(varname))%>%
#     quantile(., 0.75, na.rm = T)
#   
#   d<-tmpDF %>%
#     filter(group2==2) %>%
#     pull(!!as.name(varname))%>%
#     median(na.rm=T)
#   
#   e<-tmpDF %>%
#     filter(group2==2) %>%
#     pull(!!as.name(varname))%>%
#     quantile(., 0.25, na.rm = T)
#   
#   f<-tmpDF %>%
#     filter(group2==2) %>%
#     pull(!!as.name(varname))%>%
#     quantile(., 0.75, na.rm = T)
#   
#   g<-kruskal.test(tmpDF[[varname]], tmpDF$group2, tmpDF)[3]$p.value
#   
#   tempdf<-data.frame(Var, a,b,c,d,e,f, g)
#   names(tempdf) <- c("Var","G1_median","G1_q25", "G1_q75", "G2_median","G2_q25", "G2_q75", "p-value")
#   as_dataframe<-rbind(as_dataframe, tempdf)
# }

# write.csv(as_dataframe, "./descriptive.csv")

## Total
as_dataframe <- data.frame(matrix(ncol = 4, nrow = 0))
names(as_dataframe) <- c("Var","G1_median","G1_q25", "G1_q75")


for (varname in list){
  Var<-varname
  
  a<-tmpDF %>%
    pull(!!as.name(varname))%>%
    median(na.rm=T)
  
  b<-tmpDF %>%
    pull(!!as.name(varname))%>%
    quantile(., 0.25, na.rm = T)
  
  c<-tmpDF %>%
    pull(!!as.name(varname))%>%
    quantile(., 0.75, na.rm = T)
  
  tempdf<-data.frame(Var, a,b,c)
  names(tempdf) <- c("Var","G1_median","G1_q25", "G1_q75")
  as_dataframe<-rbind(as_dataframe, tempdf)
}

tempdf<-data.frame(Var, a,b,c)
names(tempdf) <- c("Var","G1_median","G1_q25", "G1_q75")


#### progession rate ####

as_obs$studyDate <- as.Date(as_obs$studyDate)

a<-as_obs %>%
  filter(group2==1) %>%
  group_by(ptno) %>%
  # filter( (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) < 730 ) %>%
  mutate(velocity = (last(avarMeanPressureGradient)-first(avarMeanPressureGradient)) / (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) *365) %>%
  select(ptno, velocity, avarMeanPressureGradient, studyDate, commentForFixedLines) %>%
  distinct(ptno, .keep_all = T)


b<-as_obs %>%
  filter(group2==2) %>%
  group_by(ptno) %>%
  # filter( (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) < 730 ) %>%
  mutate(velocity = (last(avarMeanPressureGradient)-first(avarMeanPressureGradient)) / (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) * 365 ) %>%
  select(ptno, velocity, avarMeanPressureGradient, studyDate) %>%
  distinct(ptno, .keep_all = T)

mean(a$velocity, na.rm = T)
sd(a$velocity, na.rm = T)

mean(b$velocity)
sd(b$velocity)

tsum.test(mean.x=mean(a$velocity) , s.x=sd(a$velocity), n.x=group1N,
          mean.y=mean(b$velocity) , s.y=sd(b$velocity), n.y=group2N)$p.value


a[,"group2"] = 1
b[,"group2"] = 2
tmp<-rbind(a,b)
# kruskal.test(tmp$velocity, tmp$group2, tmp)[3]$p.value
tsum.test(mean.x=mean(a$velocity) , s.x=sd(a$velocity), n.x=group1N,
          mean.y=mean(b$velocity) , s.y=sd(b$velocity), n.y=group2N)$p.value

median(a$velocity)
quantile(a$velocity, 0.25, na.rm = T)
quantile(a$velocity, 0.75, na.rm = T)

## Check cases of velocity < 0
# a<- a %>%
#   filter(velocity<0)
# write.csv(a,"group1_velocity.csv")


## 1,2,5년 MPG progression rate 까지의 progression rate
tmp<-as_obs %>%
  group_by(ptno) %>%
  # filter(group2==1) %>%
  # filter( (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) <= 1825 ) %>%
  filter( (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) > 90 ) %>%
  mutate(velocity = (last(avarMeanPressureGradient)-first(avarMeanPressureGradient)) / (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) *365) %>%
  select(ptno, velocity, avarMeanPressureGradient, studyDate, commentForFixedLines) %>%
  distinct(ptno, .keep_all = T)

a<- quantile(tmp$velocity, 0.25, na.rm = T)
b<- median(tmp$velocity)
c<- quantile(tmp$velocity, 0.75, na.rm = T)
d<- mean(tmp$velocity)
e<-nrow(tmp)

tempdf<-data.frame(a,b,c,d, e)
names(tempdf) <- c("25","median","75", "mean", "N")


# a<-as_obs %>%
#   filter(ptno==3878790) %>%
#   filter( (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) <= 365 ) %>%
#   mutate(velocity = (last(avarMeanPressureGradient)-first(avarMeanPressureGradient)) / (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) *365) %>%
#   select(ptno, velocity, avarMeanPressureGradient, studyDate, commentForFixedLines)

#### Incidence rate ####  

as_dataframe <- data.frame(matrix(ncol = 6, nrow = 0))
names(as_dataframe) <- c("IR_g1", "CI_l_g1", "CI_u_g1","IR_g2", "CI_l_g2", "CI_u_g2")

# event가 없을 경우 lastFUday 까지, 있는 경우 event 날짜까지

event<-c("compOut", "death", "avr", "avrSurgical", "tavi")
event_date<-c('compOut_date', 'death_date', 'avr_date', 'avr_sur_date', 'tavi_date')
event_df<-data.frame(event, event_date)


for (i in 1:nrow(event_df)) {
  event = as.character(event_df[i,1])
  event_date = as.character(event_df[i,2])
  
  a<-tmpDF %>% 
    filter(group2==1) %>%
    filter(!!as.name(event)==1) %>%
    nrow()
  b<-tmpDF %>% 
    filter(group2==2) %>%
    filter(!!as.name(event)==1) %>%
    nrow()
  
  apt <- tmpDF %>%
    filter(group2==1) %>%
    summarise(tmpDate = ifelse(is.na(!!as.name(event_date)) , lastFUDay-studyDate, !!as.name(event_date)-studyDate))
  bpt <- tmpDF %>%
    filter(group2==2) %>%
    summarise(tmpDate = ifelse(is.na(!!as.name(event_date)) ,lastFUDay-studyDate, !!as.name(event_date)-studyDate)) 
  
  bptSum <- sum(as.numeric(bpt$tmpDate))/365
  aptSum <- sum(as.numeric(apt$tmpDate))/365
   
  tmp <- as.matrix(cbind(a, aptSum))
  tmp2<-epi.conf(tmp, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
           conf.level = 0.95) * 100;tmp2
  
  tmp3 <- as.matrix(cbind(b, bptSum))
  tmp4<-epi.conf(tmp3, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
           conf.level = 0.95) * 100;tmp4
  
  
  tempdf<-data.frame(tmp2[1],tmp2[2],tmp2[3],tmp4[1], tmp4[2], tmp4[3])
  names(tempdf) <- c("IR_g1", "CI_l_g1", "CI_u_g1","IR_g2", "CI_l_g2", "CI_u_g2")
  
  as_dataframe<-rbind(as_dataframe, tempdf)
}

write.csv(as_dataframe, "incid.csv")

#### roc curve ####

tiff(paste0("./plot/ROC_Curve.tiff"), units="in", width=7, height=7, res=300)
roc_main<-Epi::ROC(form= isGroup2 ~ avarMeanPressureGradient, 
         data=tmpDF,
         plot="ROC",
         PV = F,
         MX = F,
         MI = F,
         AUC = F)
dev.off()

optimal_lr.eta=function(x){
  no=which.max(x$res$sens+x$res$spec)[1]
  result=x$res$lr.eta[no]
  result
}

optimal_cutpoint=function(x){
  y=optimal_lr.eta(x)
  b0=unname(x$lr$coeff[1])
  b1=unname(x$lr$coeff[2])
  result=(-log(1/y-1)-b0)/b1
  result
} 

optimal_cutpoint(roc_main)

## optimal cutoff: 24
tmpDF<-tmpDF %>%
  mutate(mspg24 = ifelse(avarMeanPressureGradient >= 24 , 1,0) )

# b1=pROC::roc(isGroup2 ~ avarMeanPressureGradient,as,ci=T,percent=T)
# plot(b1)
# b1$ci
# 
# wilcox.test(avarMeanPressureGradient ~ isGroup2,data=as)

#### survival analysis / Spline curve ####

## lastFUDay 수정 -> lastFUDay_r

tmp<-read.csv('./data/Preprocessed_sex_birth_death_uft8.csv')

tmp<- tmp %>% select(ptno, studyDate) %>%
  mutate(across(studyDate, as.Date, format = "%Y-%m-%d")) %>%
  group_by(ptno) %>%
  mutate(last_fu_r = last(studyDate)) %>%
  distinct(ptno, last_fu_r)

tmpDF<-left_join(tmpDF, tmp, by='ptno')

tmpDF<-tmpDF %>%
  mutate(tmp = ifelse(lastFUDay > last_fu_r, lastFUDay ,last_fu_r)) %>%
  mutate(across(tmp, as.Date, format = "%Y-%m-%d", origin="1970-01-01")) %>%
  mutate(tmp2 = pmax(death_date, avr_date, avr_sur_date, tavi_date, na.rm = TRUE)) %>%
  mutate(across(tmp2, as.Date, format = "%Y-%m-%d", origin="1970-01-01")) %>%
  mutate(lastFUDay_r = ifelse((tmp > tmp2)|(is.na(tmp2)), tmp ,tmp2)) %>%
  mutate(across(lastFUDay_r, as.Date, format = "%Y-%m-%d", origin="1970-01-01"))


## check 

event<-c("compOut", "death", "avr", "avrSurgical", "tavi")
event_date<-c('compOut_date', 'death_date', 'avr_date', 'avr_sur_date', 'tavi_date')
event_df<-data.frame(event, event_date)

event = as.character(event_df[3,1])
event_date = as.character(event_df[3,2])

a<-tmpDF %>%
  select(ptno, !!as.name(event), !!as.name(event_date), studyDate, lastFUDay, lastFUDay_r, isGroup2) %>%
  mutate(tmpDate = if_else(is.na(!!as.name(event_date)), lastFUDay_r, !!as.name(event_date))) %>%
  mutate(time = as.numeric(tmpDate - studyDate)/365) %>%
  select(ptno, !!as.name(event), !!as.name(event_date), studyDate, lastFUDay_r, tmpDate, time, isGroup2)

nrow(a[(a$avr==1)&(a$isGroup2==1),])

nrow(tmpDF[!is.na(tmpDF$lastFUDay_r),])


## KM survival curve

for (i in 1:nrow(event_df)){
  
  event = as.character(event_df[i,1])
  event_date = as.character(event_df[i,2])
  
  a <- tmpDF %>%
    mutate(tmpDate = if_else(is.na(!!as.name(event_date)), lastFUDay_r, !!as.name(event_date)))
  
  surv_object <- Surv(time = as.numeric(a$tmpDate-a$studyDate)/365, event = a[[event]])
  
  survfit <- survfit(surv_object ~ isGroup2, data = a)
  
  g<-ggsurvplot(survfit, 
             data = a, 
             pval = TRUE,
             legend.labs = c("Slow progression", "Rapid progression"),
             palette = c("blue", "red"),
             risk.table = TRUE,
             xlab = "Years",
             break.time.by = 1,
             ylim = c(0,1),
             xlim = c(0,9.6))
  
  # ggsave(file=paste0("./plot/surv_",event,".tiff"), print(g),width=20, height=14, units="cm", dpi = 300)
  ggsave(file=paste0("./surv_",event,".png"), print(g),width=20, height=14, units="cm", dpi = 300)
}



## Spline curve

## 이거 왜 저장안되지
# for (i in 1:nrow(event_df)){
#   
#   event = as.character(event_df[i,1])
#   event_date = as.character(event_df[i,2])
# 
#   spDF<-tmpDF %>%
#     dplyr::select(ptno, avarMeanPressureGradient, sexDummy, age, studyDate, lastFUDay, isGroup2, death, avr,
#                   avrSurgical, tavi, compOut, death_date, avr_date, avr_sur_date, tavi_date, compOut_date)
#   
#   ddist <- datadist(spDF)
#   options(datadist="ddist")
#   ddist$limits["Adjust to","avarMeanPressureGradient"] <- 24
#   
#   spDF <- spDF %>%
#     mutate(tmpDate = if_else(is.na(!!as.name(event_date)), lastFUDay, !!as.name(event_date)))
#   
#   surv_object <- Surv(time = as.numeric(spDF$tmpDate - spDF$studyDate)/365, event = spDF[[event]])
#   
#   Spline_mspg <-cph(surv_object ~ rcs(avarMeanPressureGradient,4)
#                     ,spDF, x=T, y=T)
#   
#   Splcurve_mspg=Predict(Spline_mspg,avarMeanPressureGradient, ref.zero=TRUE, fun=exp)
#   
#   tiff(paste0("./plot/spline_",event,".tiff"), units="in", width=7, height=7, res=300)
#   plot(Splcurve_mspg, xlab = "MPG", ylab = "HR (95% CI)")
#   dev.off()
# }

i=5
event = as.character(event_df[i,1])
event_date = as.character(event_df[i,2])

spDF<-tmpDF %>%
  dplyr::select(ptno, avarMeanPressureGradient, sexDummy, age, studyDate, lastFUDay, isGroup2, death, avr,
                avrSurgical, tavi, compOut, death_date, avr_date, avr_sur_date, tavi_date, compOut_date)

ddist <- datadist(spDF)
options(datadist="ddist")
ddist$limits["Adjust to","avarMeanPressureGradient"] <- 24

spDF <- spDF %>%
  mutate(tmpDate = if_else(is.na(!!as.name(event_date)), lastFUDay, !!as.name(event_date)))

surv_object <- Surv(time = as.numeric(spDF$tmpDate - spDF$studyDate)/365, event = spDF[[event]])

Spline_mspg <-cph(surv_object ~ rcs(avarMeanPressureGradient,4)
                  ,spDF, x=T, y=T)

Splcurve_mspg=Predict(Spline_mspg,avarMeanPressureGradient, ref.zero=TRUE, fun=exp)

tiff(paste0("./plot/spline_",event,".tiff"), units="in", width=7, height=7, res=300)
plot(Splcurve_mspg, xlab = "MPG", ylab = "HR (95% CI)")
dev.off()


# tmpDF$laVolumeBymMode
# tmpDF$laVolumeIndexBymMode
# 
# tmp<-tmpDF%>%
#   select(ptno, laVolumeBymMode, laVolumeBymMode_1, laVolumeIndexBymMode, laVolumeIndexBymMode_1)
# tmpDF$laVolumeIndexBymMode<-as.numeric(tmpDF$laVolumeIndexBymMode)
# hist(tmpDF$laVolumeIndexBymMode)
# quantile(tmpDF$laVolumeIndexBymMode, na.rm = T)

## landmark analysis ##
### From 1, 2, 5 year outcome variables ###

## death
# tmpDF<-tmpDF %>%
#   mutate(death_1y = ifelse(death_date-studyDate<= 365 |is.na(death_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(death_2y = ifelse(death_date-studyDate<= 730 |is.na(death_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(death_5y = ifelse(death_date-studyDate<= 1825 |is.na(death_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(death_1y_date = if_else(death_date-studyDate<= 365 |is.na(death_date) , as.Date(NA,origin="1970-01-01"), death_date) )
# tmpDF<-tmpDF %>%
#   mutate(death_2y_date = if_else(death_date-studyDate<= 730 |is.na(death_date) , as.Date(NA,origin="1970-01-01"), death_date) )
# tmpDF<-tmpDF %>%
#   mutate(death_5y_date = if_else(death_date-studyDate<= 1825 |is.na(death_date) , as.Date(NA,origin="1970-01-01"), death_date) )
# 
# ## tavi
# tmpDF<-tmpDF %>%
#   mutate(tavi_1y = ifelse(tavi_date-studyDate<= 365 |is.na(tavi_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(tavi_2y = ifelse(tavi_date-studyDate<= 730 |is.na(tavi_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(tavi_5y = ifelse(tavi_date-studyDate<= 1825 |is.na(tavi_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(tavi_1y_date = if_else(tavi_date-studyDate<= 365 |is.na(tavi_date) , as.Date(NA,origin="1970-01-01"), tavi_date) )
# tmpDF<-tmpDF %>%
#   mutate(tavi_2y_date = if_else(tavi_date-studyDate<= 730 |is.na(tavi_date) , as.Date(NA,origin="1970-01-01"), tavi_date) )
# tmpDF<-tmpDF %>%
#   mutate(tavi_5y_date = if_else(tavi_date-studyDate<= 1825 |is.na(tavi_date) , as.Date(NA,origin="1970-01-01"), tavi_date) )
# 
# ## avr_sur
# tmpDF<-tmpDF %>%
#   mutate(avr_sur_1y = ifelse(avr_sur_date-studyDate<= 365 |is.na(avr_sur_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(avr_sur_2y = ifelse(avr_sur_date-studyDate<= 730 |is.na(avr_sur_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(avr_sur_5y = ifelse(avr_sur_date-studyDate<= 1825 |is.na(avr_sur_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(avr_sur_1y_date = if_else(avr_sur_date-studyDate<= 365 |is.na(avr_sur_date) , as.Date(NA,origin="1970-01-01"), avr_sur_date) )
# tmpDF<-tmpDF %>%
#   mutate(avr_sur_2y_date = if_else(avr_sur_date-studyDate<= 730 |is.na(avr_sur_date) , as.Date(NA,origin="1970-01-01"), avr_sur_date) )
# tmpDF<-tmpDF %>%
#   mutate(avr_sur_5y_date = if_else(avr_sur_date-studyDate<= 1825 |is.na(avr_sur_date) , as.Date(NA,origin="1970-01-01"), avr_sur_date) )
# 
# ## avr
# tmpDF<-tmpDF %>%
#   mutate(avr_1y = ifelse(avr_date-studyDate<= 365 |is.na(avr_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(avr_2y = ifelse(avr_date-studyDate<= 730 |is.na(avr_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(avr_5y = ifelse(avr_date-studyDate<= 1825 |is.na(avr_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(avr_1y_date = if_else(avr_date-studyDate<= 365 |is.na(avr_date) , as.Date(NA,origin="1970-01-01"), avr_date) )
# tmpDF<-tmpDF %>%
#   mutate(avr_2y_date = if_else(avr_date-studyDate<= 730 |is.na(avr_date) , as.Date(NA,origin="1970-01-01"), avr_date) )
# tmpDF<-tmpDF %>%
#   mutate(avr_5y_date = if_else(avr_date-studyDate<= 1825 |is.na(avr_date) , as.Date(NA,origin="1970-01-01"), avr_date) )
# 
# ## composite
# tmpDF<-tmpDF %>%
#   mutate(comp_1y = ifelse(compOut_date-studyDate<= 365 |is.na(compOut_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(comp_2y = ifelse(compOut_date-studyDate<= 730 |is.na(compOut_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(comp_5y = ifelse(compOut_date-studyDate<= 1825 |is.na(compOut_date) , 0, 1) )
# tmpDF<-tmpDF %>%
#   mutate(comp_1y_date = if_else(compOut_date-studyDate<= 365 |is.na(compOut_date) , as.Date(NA,origin="1970-01-01"), compOut_date) )
# tmpDF<-tmpDF %>%
#   mutate(comp_2y_date = if_else(compOut_date-studyDate<= 730 |is.na(compOut_date) , as.Date(NA,origin="1970-01-01"), compOut_date) )
# tmpDF<-tmpDF %>%
#   mutate(comp_5y_date = if_else(compOut_date-studyDate<= 1825 |is.na(compOut_date) , as.Date(NA,origin="1970-01-01"), compOut_date) )
# 

#### univariate cox ####

covariates <- c("isGroup2", "sexDummy", "age10","Hypertension","Dyslipidemia","DM","PAOD","Stroke","Renal.dysfunction",
          "COPD","CAD_re","CABG","HF", "AF", "Rheumatic", "Bicuspid.AV", "Concomittant.AR")

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

event = "avrSurgical"   ## death, avr, compOut, tavi, avrSurgical
event_date = "avr_sur_date"   ## death_date, avr_date, compOut_date, tavi_date, avr_sur_date

df_for_cox<-tmpDF %>%
  mutate(tmpDate = if_else(is.na(!!as.name(event_date)), lastFUDay, !!as.name(event_date))) %>%
  mutate(time = as.numeric(tmpDF$tmpDate - tmpDF$studyDate)/365)

df_for_cox$status <- tmpDF[[event]]

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = df_for_cox, weights= class_weight)})

# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=3)
                         wald.test<-signif(x$wald["test"], digits=3)
                         beta<-signif(x$coef[1], digits=3);#coeficient beta
                         HR <-signif(x$coef[2], digits=3);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 3)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],3)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
res<-as.data.frame(res)
res

#### multivariate cox ####
##수정필요

## select variables whose p-value < 0.05
# res$p.value<-as.numeric(levels(res$p.value))
# tmp<-res[res$p.value<0.05,]
# cov_list<-rownames(tmp)
# cov_list
# 
# # out = survival::coxph(survival::Surv(time, eval(as.name(event)))~paste(cov_list, collapse= "+"), data=df_for_cox)
# paste(cov_list, collapse= "+")

event = "tavi"   ## compOut, death, avr, avrSurgical, tavi
event_date = "tavi_date"   ## compOut_date, death_date, avr_date, avr_sur_date, tavi_date


# comp: isGroup2+sexDummy+age10+Hypertension+DM+PAOD+Renal.dysfunction+COPD+CAD_re+Rheumatic+Bicuspid.AV+Concomittant.AR

# death: isGroup2+sexDummy+age10+Hypertension+DM+PAOD+Stroke+Renal.dysfunction+COPD+CAD_re+CABG+HF+Rheumatic+Bicuspid.AV+Concomittant.AR

# avr: isGroup2 +age10+PAOD+Renal.dysfunction+AF+Rheumatic

# surgical: isGroup2+age10+Dyslipidemia+CABG

# tavi: isGroup2+age10+Hypertension+Dyslipidemia+DM+PAOD+Renal.dysfunction+COPD+CAD_re+CABG+AF+Rheumatic+Bicuspid.AV+Concomittant.AR

out = survival::coxph(survival::Surv(time, eval(as.name(event)))~isGroup2+age10+Hypertension+Dyslipidemia+DM+PAOD+Renal.dysfunction+COPD+CAD_re+CABG+AF+Rheumatic+Bicuspid.AV+Concomittant.AR, data=df_for_cox, weights= class_weight)

out
summary(out)
exp(out$coefficients)[1] #HR

#### cumulative outcomes ####

## death
tmpDF<-tmpDF %>%
  mutate(death_1y = ifelse(death_date-studyDate>= 365 |is.na(death_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(death_2y = ifelse(death_date-studyDate>= 730 |is.na(death_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(death_5y = ifelse(death_date-studyDate>= 1825 |is.na(death_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(death_1y_date = if_else(death_date-studyDate>= 365 |is.na(death_date) , as.Date(NA,origin="1970-01-01"), death_date) )
tmpDF<-tmpDF %>%
  mutate(death_2y_date = if_else(death_date-studyDate>= 730 |is.na(death_date) , as.Date(NA,origin="1970-01-01"), death_date) )
tmpDF<-tmpDF %>%
  mutate(death_5y_date = if_else(death_date-studyDate>= 1825 |is.na(death_date) , as.Date(NA,origin="1970-01-01"), death_date) )

## tavi
tmpDF<-tmpDF %>%
  mutate(tavi_1y = ifelse(tavi_date-studyDate>= 365 |is.na(tavi_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(tavi_2y = ifelse(tavi_date-studyDate>= 730 |is.na(tavi_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(tavi_5y = ifelse(tavi_date-studyDate>= 1825 |is.na(tavi_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(tavi_1y_date = if_else(tavi_date-studyDate>= 365 |is.na(tavi_date) , as.Date(NA,origin="1970-01-01"), tavi_date) )
tmpDF<-tmpDF %>%
  mutate(tavi_2y_date = if_else(tavi_date-studyDate>= 730 |is.na(tavi_date) , as.Date(NA,origin="1970-01-01"), tavi_date) )
tmpDF<-tmpDF %>%
  mutate(tavi_5y_date = if_else(tavi_date-studyDate>= 1825 |is.na(tavi_date) , as.Date(NA,origin="1970-01-01"), tavi_date) )

## avr_sur
tmpDF<-tmpDF %>%
  mutate(avr_sur_1y = ifelse(avr_sur_date-studyDate>= 365 |is.na(avr_sur_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(avr_sur_2y = ifelse(avr_sur_date-studyDate>= 730 |is.na(avr_sur_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(avr_sur_5y = ifelse(avr_sur_date-studyDate>= 1825 |is.na(avr_sur_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(avr_sur_1y_date = if_else(avr_sur_date-studyDate>= 365 |is.na(avr_sur_date) , as.Date(NA,origin="1970-01-01"), avr_sur_date) )
tmpDF<-tmpDF %>%
  mutate(avr_sur_2y_date = if_else(avr_sur_date-studyDate>= 730 |is.na(avr_sur_date) , as.Date(NA,origin="1970-01-01"), avr_sur_date) )
tmpDF<-tmpDF %>%
  mutate(avr_sur_5y_date = if_else(avr_sur_date-studyDate>= 1825 |is.na(avr_sur_date) , as.Date(NA,origin="1970-01-01"), avr_sur_date) )

## avr
tmpDF<-tmpDF %>%
  mutate(avr_1y = ifelse(avr_date-studyDate>= 365 |is.na(avr_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(avr_2y = ifelse(avr_date-studyDate>= 730 |is.na(avr_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(avr_5y = ifelse(avr_date-studyDate>= 1825 |is.na(avr_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(avr_1y_date = if_else(avr_date-studyDate>= 365 |is.na(avr_date) , as.Date(NA,origin="1970-01-01"), avr_date) )
tmpDF<-tmpDF %>%
  mutate(avr_2y_date = if_else(avr_date-studyDate>= 730 |is.na(avr_date) , as.Date(NA,origin="1970-01-01"), avr_date) )
tmpDF<-tmpDF %>%
  mutate(avr_5y_date = if_else(avr_date-studyDate>= 1825 |is.na(avr_date) , as.Date(NA,origin="1970-01-01"), avr_date) )

## composite
tmpDF<-tmpDF %>%
  mutate(comp_1y = ifelse(compOut_date-studyDate>= 365 |is.na(compOut_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(comp_2y = ifelse(compOut_date-studyDate>= 730 |is.na(compOut_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(comp_5y = ifelse(compOut_date-studyDate>= 1825 |is.na(compOut_date) , 0, 1) )
tmpDF<-tmpDF %>%
  mutate(comp_1y_date = if_else(compOut_date-studyDate>= 365 |is.na(compOut_date) , as.Date(NA,origin="1970-01-01"), compOut_date) )
tmpDF<-tmpDF %>%
  mutate(comp_2y_date = if_else(compOut_date-studyDate>= 730 |is.na(compOut_date) , as.Date(NA,origin="1970-01-01"), compOut_date) )
tmpDF<-tmpDF %>%
  mutate(comp_5y_date = if_else(compOut_date-studyDate>= 1825 |is.na(compOut_date) , as.Date(NA,origin="1970-01-01"), compOut_date) )


# "compOut", "death", "avr", "avrSurgical", "tavi"
event = "compOut"   ## comp_1y/ death_1y/ avr_1y/ avr_sur_1y/ tavi_1y

tmpDF %>%
  filter(group2==2) %>%
  # filter(avr==1) %>%
  filter(!is.na(avr_date)==1) %>%
  nrow()


tmp$
######################################################################3
as_dataframe <- data.frame(matrix(ncol = 4, nrow = 0))
names(as_dataframe) <- c("n_g1", "per_g1", "n_g2","per_g2")

event<-c("comp_5y", "death_5y", "avr_5y", "avr_sur_5y", "tavi_5y")
event_date<-c('comp_5y_date', 'death_5y_date', 'avr_5y_date', 'avr_sur_5y_date', 'tavi_5y_date')
event_df<-data.frame(event, event_date)


for (i in 1:nrow(event_df)) {
  event = as.character(event_df[i,1])
  event_date = as.character(event_df[i,2])
  
  a<-tmpDF %>% 
    filter(group2==1) %>%
    filter(!!as.name(event)==1) %>%
    nrow()
  
  b<-signif(a/group1N*100, digits=3)
  
  c<-tmpDF %>% 
    filter(group2==2) %>%
    filter(!!as.name(event)==1) %>%
    nrow()
  
  d<-signif(c/group2N*100, digits=3)
  

  tempdf<-data.frame(a,b,c,d)
  names(tempdf) <- c("n_g1", "per_g1", "n_g2","per_g2")
  
  as_dataframe<-rbind(as_dataframe, tempdf)
}


# tmpDF$group2
# #### avr을 시행한 환자 중 1,2,5년 내로 사망했는지
# tmpDF<-tmpDF %>%
#   mutate(death_after_avr = ifelse(death_date-avr_date > 0 & !is.na(death_date) & !is.na(avr_date) , 1, 0) )
# 
# tmpDF<-tmpDF %>%
#   mutate(death_after_avr_y1 = ifelse(death_date-avr_date <= 365 & death_date-avr_date > 0 & !is.na(death_date) & !is.na(avr_date) , 1, 0) )
# 
# tmpDF<-tmpDF %>%
#   mutate(death_after_avr_y2 = ifelse(death_date-avr_date <= 730 & death_date-avr_date > 0 & !is.na(death_date) & !is.na(avr_date) , 1, 0) )
# 
# tmpDF<-tmpDF %>%
#   mutate(death_after_avr_y5 = ifelse(death_date-avr_date <= 1825 & death_date-avr_date > 0 & !is.na(death_date) & !is.na(avr_date) , 1, 0) )
# 
# tmpDF<-tmpDF %>%
#   mutate(death_1y_date = if_else(death_date-avr_date>= 365 |is.na(death_date) , as.Date(NA,origin="1970-01-01"), death_date) )
# 
# tmpDF<-tmpDF %>%
#   mutate(death_2y_date = if_else(death_date-avr_date>= 730 |is.na(death_date) , as.Date(NA,origin="1970-01-01"), death_date) )
# 
# tmpDF<-tmpDF %>%
#   mutate(death_5y_date = if_else(death_date-avr_date>= 1825 |is.na(death_date) , as.Date(NA,origin="1970-01-01"), death_date) )
# 
# 
# table(tmpDF$death_after_avr[tmpDF$group2==2])
# 
# a<-tmpDF %>%
#   select(ptno, death, avr, death_date, avr_date, death_after_avr, death_after_avr_y1, death_after_avr_y2, death_after_avr_y5)
# 




#### logistic ####

covariates <- c("mspg24","sexDummy", "age10", "Hypertension","Dyslipidemia","DM","PAOD","Stroke","Renal.dysfunction",
                "COPD","CAD","CABG","HF", "AF", "Rheumatic", "Bicuspid.AV", "Concomittant.AR")

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste0('isGroup2~', x)))

univ_models <- lapply( univ_formulas, function(x){glm(x, data = tmpDF, weights = class_weight, family=binomial)})

# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         y <- confint(x)
                         x <- summary(x)
                         OR<-signif(exp(x$coefficients[2]),3)
                         p.value<-signif(x$coefficients[8])
                         CI.upper<-signif(exp(y[4]),3)
                         CI.lower<-signif(exp(y[2]),3)
                         OR <- paste0(OR, " (", CI.lower, "-", CI.upper, ")")
                         res<-c(OR, p.value)
                         names(res)<-c("OR (95% CI for OR)", "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

## multivariate logistic

result<-glm(isGroup2 ~ mspg24  + AF , family=binomial, weights = class_weight, data=tmpDF)
summary(result)

odds<-as.data.frame(exp(result$coefficients))
ci = as.data.frame(exp(confint(result, level = 0.95)))
pval=as.data.frame(as.data.frame(summary(result)[12])[,4])

tmp<-cbind(odds,ci, pval)


#### information loss ####
death_compare  <-  read.csv("./data/death_information_loss.csv", stringsAsFactors = F)
death_compare$in_hospital<-as.Date(death_compare$in_hospital, format="%Y-%m-%d" )
death_compare$KMIS<-as.Date(death_compare$KMIS, format="%Y-%m-%d" )

death_compare %>%
  # filter(group2==1) %>%
  filter(!is.na(KMIS)) %>%
  nrow()


####interscan interval 분포 ####

# group1N<-nrow(tmpDF[tmpDF$group2==1,])
# group2N<-nrow(tmpDF[tmpDF$group2==2,])
# as_obs$studyDate <- as.Date(as_obs$studyDate)

tmp<-as_obs %>%
  group_by(ptno) %>%
  filter(group2 ==2) %>%
  mutate(mean_interscan_interval = (last(studyDate) - first(studyDate)) / (n()-1) ) %>%
  mutate(follow_up = last(studyDate) - first(studyDate) ) %>%
  select(ptno, mean_interscan_interval, follow_up, studySeq, studyDate) %>%
  distinct(ptno, mean_interscan_interval,follow_up, .keep_all = T)

# tmp[which(duplicated(tmp$ptno)),]
# hist(tmp$mean_interscan_interval)

a<- quantile(tmp$follow_up, 0, na.rm = T)/365
b<- quantile(tmp$follow_up, 0.25, na.rm = T)/365
c<- quantile(tmp$follow_up, 0.5, na.rm = T)/365
d<- quantile(tmp$follow_up, 0.75, na.rm = T)/365
e<- quantile(tmp$follow_up, 1, na.rm = T)/365
f<- mean(tmp$follow_up, na.rm = T)/365


tempdf<-data.frame(a,b,c,d,e,f)
names(tempdf) <- c("min","25","median","75", "max","mean")

## Kruskall-Wallis test
# kruskal.test(tmp$mean_interscan_interval, tmp$group2, tmp)[3]$p.value

tmp<-as_obs %>%
  group_by(ptno) %>%
  filter(group2 ==1) %>%
  mutate(mean_interscan_interval = (last(studyDate) - first(studyDate)) / (n()-1) ) %>%
  mutate(follow_up = last(studyDate) - first(studyDate) ) %>%
  select(ptno, mean_interscan_interval,follow_up, studySeq, studyDate) %>%
  distinct(ptno, mean_interscan_interval,follow_up, .keep_all = T)

a<-mean(as.numeric(tmp$mean_interscan_interval), na.rm = T)/365
b<-sd(as.numeric(tmp$mean_interscan_interval), na.rm = T)/365

tmp<-as_obs %>%
  group_by(ptno) %>%
  filter(group2 ==2) %>%
  mutate(mean_interscan_interval = (last(studyDate) - first(studyDate)) / (n()-1) ) %>%
  mutate(follow_up = last(studyDate) - first(studyDate) ) %>%
  select(ptno, mean_interscan_interval,follow_up, studySeq, studyDate) %>%
  distinct(ptno, mean_interscan_interval,follow_up, .keep_all = T)

c<-mean(as.numeric(tmp$mean_interscan_interval), na.rm = T)/365
d<-sd(as.numeric(tmp$mean_interscan_interval), na.rm = T)/365

tsum.test(mean.x=as.numeric(a) ,   s.x=as.numeric(b), n.x=group1N,
          mean.y=as.numeric(c) , s.y=as.numeric(d), n.y=group2N)$p.value

## 2. progression rate (1/2/5/total year)

tmp<-as_obs %>%
  group_by(ptno) %>%
  filter(group2==2) %>%
  # filter( (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) <= 1825 ) %>%
  mutate(velocity = (last(avarMeanPressureGradient)-first(avarMeanPressureGradient)) / (as.numeric(as.POSIXct(last(studyDate)) - as.POSIXct(first(studyDate)))) *365) %>%
  select(ptno, velocity, avarMeanPressureGradient, studyDate, commentForFixedLines,group2) %>%
  distinct(ptno, .keep_all = T)

a<- quantile(tmp$velocity, 0.25, na.rm = T)
b<- median(tmp$velocity)
c<- quantile(tmp$velocity, 0.75, na.rm = T)
d<- mean(tmp$velocity)
e<- sd(tmp$velocity)
f<-nrow(tmp)

e

tempdf<-data.frame(a,b,c,d,e,f)
names(tempdf) <- c("25","median","75", "mean","sd", "N")

tsum.test(mean.x=as.numeric(mean(tmp[tmp$group2==1,]$velocity)) , s.x=as.numeric(sd(tmp[tmp$group2==1,]$velocity)), n.x=group1N,
          mean.y=as.numeric(mean(tmp[tmp$group2==2,]$velocity)) , s.y=as.numeric(sd(tmp[tmp$group2==2,]$velocity)), n.y=group2N)$p.value


#### echo indication ####

## 1 

ind_df <- read.csv("./data/AS_first_echo_indication_merged.csv")

ind_df<- ind_df %>%
  mutate(Reason_Classified = case_when(Reason_1 =="AS"~"Routine",
                                       Reason_1 =="Bicuspid AV"~"Routine",
                                       Reason_1 =="Cardiomegaly"~"Symptomatic",
                                       Reason_1 =="Edema"~"Symptomatic",
                                       Reason_1 =="Effusion"~"Symptomatic",
                                       Reason_1 =="Pericardial effusion"~"Symptomatic",
                                       Reason_1 =="Pleural effusion"~"Symptomatic",
                                       Reason_1 =="Pulmonary edema"~"Symptomatic",
                                       Reason_1 =="Murmur"~"Symptomatic",
                                       Reason_1 =="PostOP"~"Evaluation for non~cardiac procedures",
                                       Reason_1 =="Pre AVR"~"Pre AVR",
                                       Reason_1 =="Pre TAVI"~"Pre AVR",
                                       Reason_1 =="PreOP evaluation"~"Evaluation for non~cardiac procedures",
                                       Reason_1 =="Chest pain"~"Symptomatic",
                                       Reason_1 =="Dizziness"~"Symptomatic",
                                       Reason_1 =="Dyspnea"~"Symptomatic",
                                       Reason_1 =="General weakness"~"Symptomatic",
                                       Reason_1 =="Orthopnea"~"Symptomatic",
                                       Reason_1 =="Presyncope"~"Symptomatic",
                                       Reason_1 =="Syncope"~"Symptomatic",
                                       Reason_1 =="AF"~"Routine",
                                       Reason_1 =="Afl"~"Routine",
                                       Reason_1 =="Arrhythmia"~"Routine",
                                       Reason_1 =="Bradycardia"~"Routine",
                                       Reason_1 =="Cardiac marker elevation"~"Routine",
                                       Reason_1 =="Cerebral infarct"~"Routine",
                                       Reason_1 =="CRF"~"Routine",
                                       Reason_1 =="DCMP"~"Routine",
                                       Reason_1 =="DVT"~"Routine",
                                       Reason_1 =="Embolism"~"Routine",
                                       Reason_1 =="ESRD"~"Routine",
                                       Reason_1 =="Follow up for other purpose"~"Routine",
                                       Reason_1 =="HCMP"~"Routine",
                                       Reason_1 =="Health check-up"~"Routine",
                                       Reason_1 =="HF aggravation"~"Routine",
                                       Reason_1 =="Hypertension"~"Routine",
                                       Reason_1 =="Hypotension"~"Routine",
                                       Reason_1 =="ICH"~"Routine",
                                       Reason_1 =="IE"~"Routine",
                                       Reason_1 =="Infection"~"Routine",
                                       Reason_1 =="Ischemic DCMP"~"Routine",
                                       Reason_1 =="Melena"~"Routine",
                                       Reason_1 =="Palpitation"~"Symptomatic",
                                       Reason_1 =="PAOD"~"Routine",
                                       Reason_1 =="Pneumonia"~"Routine",
                                       Reason_1 =="Post resuscitation"~"Routine",
                                       Reason_1 =="PTE"~"Symptomatic",
                                       Reason_1 =="Pulmonary hypertension"~"Routine",
                                       Reason_1 =="Renal infarct"~"Routine",
                                       Reason_1 =="SDH"~"Routine",
                                       Reason_1 =="Sepsis"~"Routine",
                                       Reason_1 =="Shock"~"Routine",
                                       Reason_1 =="Stroke"~"Routine",
                                       Reason_1 =="Thrombus"~"Routine",
                                       Reason_1 =="Fever"~"Routine"
                                       ))

table(ind_df$Reason_Classified,ind_df$group2)


install.packages("fastDummies")
library(fastDummies)

tmp<- ind_df %>%
  select(ptno, Reason_Classified, isGroup2)

tmpp<-dummy_cols(tmp, select_columns = 'Reason_Classified')

mylist = colnames(tmpp)
my.list <- lapply(mylist,function(var)chisq.test(tmpp$isGroup2, tmpp[,var]))

do.call(rbind, my.list)[,c(1,3)]

a<-as.data.frame(mylist)


## 2

# ## f/u 도중 40이상이 한번이라도 있는 케이스
tmp <- as_obs %>%
  # filter(group2==2) %>%
  group_by(ptno) %>%
  mutate(over40 = ifelse(any(avarMeanPressureGradient>=40), 1, 0)) %>%
  select(ptno, avarMeanPressureGradient, over40, group2) %>%
  distinct(ptno, .keep_all = T)

chisq.test(tmp$group2, tmp$over40)

# ## 한번이라도 40이상이었다가 미만이 된 케이스
tmp <- as_obs %>%
  group_by(ptno) %>%
  mutate(over40 = ifelse(any(avarMeanPressureGradient>=40), 1, 0)) %>%
  mutate(over40down = ifelse(any(avarMeanPressureGradient<40) & over40==1, 1, 0)) %>%
  select(ptno, avarMeanPressureGradient, over40, over40down,group2) %>%
  distinct(ptno, .keep_all = T)

chisq.test(tmp$group2, tmp$over40down)


## 3


## echo 검사수 median, quantile
tmp<-as_obs %>%
  group_by(ptno) %>%
  mutate(echo_N = n()) %>%
  # filter(group2 == 1) %>%
  distinct(ptno, .keep_all = T) %>%
  select(ptno, echo_N, group2) 

median(tmp$echo_N)
quantile(tmp$echo_N, 0.25)
quantile(tmp$echo_N, 0.75)
mean(tmp$echo_N)

a<-mean(tmp[tmp$group2==1,]$echo_N)
b<-sd(tmp[tmp$group2==1,]$echo_N)
c<-mean(tmp[tmp$group2==2,]$echo_N)
d<-sd(tmp[tmp$group2==2,]$echo_N)


tsum.test(mean.x=a ,   s.x=b, n.x=306,
           mean.y=c , s.y=d, n.y=380)$p.value



#### 

# 2. NA 몇명되는지 정리

as %>%
  filter(is.na(AVA_re)) %>%
  filter(group2==2) %>%
  nrow()

# total: 37 (slow group:23, rapid group:14)

# 3. AVA<1.0 각 군/total에서 몇명인지

as %>%
  filter(AVA_re < 1) %>%
  filter(group2==2) %>%
  nrow()

# total: 37 (slow group:17, rapid group:73)

## density plot
tmpDF$isGroup2_fac<-as.factor(tmpDF$isGroup2)


g<-ggplot(tmpDF, aes(x=avarMeanPressureGradient, color = isGroup2_fac)) +
  geom_density(bw = 1) +
  theme(text = element_text(size=15), panel.background = element_blank(), axis.line = element_line(colour = "grey")) +
  labs(x = "Initial MPG", y = "Density") +
  scale_color_manual(name = "Latent Class", values = c("blue", "red"), labels=c("Trajectory 1 (Slow progression)", "Trajectory 2 (Rapid progression)"))+
  theme(legend.position = "bottom", legend.box = "horizontal")


ggsave(file="./plot/density_plot.tiff", g, width=23, height=12, units="cm", dpi = 300) #saves g
## 3) 추후 mean interscan interval <30 인 경우 제외 조건도 고려해볼만 함.

# tmp<-as_obs %>%
#   group_by(ptno) %>%
#   mutate(interscan_interval = case_when(row_number()==1 ~ 0,
#                                         row_number()!=1 ~ as.numeric(studyDate) - as.numeric(lag(studyDate))) ) %>%
#   filter(interscan_interval!=0) %>%
#   mutate(mean_ii = mean(interscan_interval)) %>%
#   select(ptno, interscan_interval, mean_ii, studySeq, studyDate) %>%
#   filter(mean_ii <= 30) %>%
#   distinct(ptno, .keep_all = T)


#### This part is processed already. Do not run codes below ####
# 
# as <- read.csv("./data/Database_0609_utf.csv", stringsAsFactors = F, encoding = "UFT-8")
# ptList<-as[c("ptno", "patientName", "birthDate")]
# write.csv(ptList, "list.csv", fileEncoding = "euc-kr")
# 
# as$birthDate

# ## Add AVR, AVR Surgical, TAVI

# avrSurgical_list  <-  read.csv("./data/avr_sur_list.csv", stringsAsFactors = F)
# names(avrSurgical_list) <- c("ptno","birthDate","sur_date")
# 
# tavi_list  <-  read.csv("./data/tavi_list.csv", stringsAsFactors = F)
# names(tavi_list) <- c("ptno","birthDate","sur_date")
# 
# avr_list<- rbind(avrSurgical_list, tavi_list) %>%
#   distinct(ptno, .keep_all = T)
# names(avr_list) <- c("ptno","birthDate","avr_date")
# 
# names(avrSurgical_list) <- c("ptno","birthDate","avr_sur_date")
# names(tavi_list) <- c("ptno","birthDate","tavi_date")

# 
# as<-as %>%
#   mutate(avrSurgical = ifelse(ptno %in% avrSurgical_list$ptno, 1, 0))
# 
# as<-as %>%
#   mutate(tavi = ifelse(ptno %in% tavi_list$ptno, 1, 0))
# 
# as<-as %>%
#   mutate(avr = ifelse(ptno %in% avr_list$ptno, 1, 0))
# 
# as<-as %>%
#   mutate(avrSurgical_date = ifelse(ptno %in% avrSurgical_list$ptno, avrSurgical_list$avr_sur_date, NA))
# 
# as<-as %>%
#   mutate(tavi_date = ifelse(ptno %in% tavi_list$ptno, tavi_list$tavi_date, NA))
# 
# as<-as %>%
#   mutate(avr_date2 = ifelse(ptno %in% tavi_list$ptno, tavi_list$tavi_date, NA))
# 
# as<-as %>%
#   mutate(avr_date1 = ifelse(ptno %in% avrSurgical_list$ptno, avrSurgical_list$avr_sur_date, NA))
# 
# as$avrSurgical_date <-as.Date(as$avrSurgical_date)
# as$tavi_date <-as.Date(as$tavi_date)
# as$avr_date1 <-as.Date(as$avr_date1)
# as$avr_date2 <-as.Date(as$avr_date2)
# 
# 
# as<-as %>%
#   mutate(avr_date = case_when(avr_date1 < avr_date2 ~ avr_date1,
#                               avr_date1 >= avr_date2 ~ avr_date2,
#                               is.na(avr_date1) ~ avr_date2,
#                               is.na(avr_date2) ~ avr_date1))
# 
# #tmp<-as%>% select(ptno, avrSurgical, tavi, avr, avrSurgical_date, tavi_date, avr_date1, avr_date2, avr_date)
# 
# #as <- as[,-which(names(as) %in% c("avr_date1","avr_date2"))]
# 
# ## death
# death_list  <-  read.csv("./data/death_list.csv", stringsAsFactors = F)
# 
# death_list$death_1<-as.Date(death_list$death_1)
# death_list$death_2<-as.Date(death_list$death_2)
# 
# death_list<-death_list %>%
#   mutate(death_date = case_when(!is.na(death_1) ~ death_1,
#                                 is.na(death_1) ~ death_2))
# 
# death_list<-death_list%>%
#   mutate(death = ifelse(!is.na(death_date), 1, 0))
# 
# tmp<-death_list %>%
#   select(ptno, death_date, death)
# 
# 
# #as <- as[,-which(names(as) %in% c("death","Death"))]
# 
# 
# as<-left_join(as,tmp,by='ptno')
# 
# write.csv(as,"AS_rawdata_환자별.csv")
# 
# ##각 환자의 마지막 열만 뽑기
# tmp2<-as_obs %>%
#   arrange(ptno, studySeq) %>% 
#   group_by(ptno) %>% 
#   summarise_all(last)
# 
# tmp2<- tmp2 %>% select(ptno, studyDate)
# 
# names(tmp2) <- c("ptno","lastFuDay2")
# 
# 
# as2<-left_join(as,tmp2,by='ptno')
# 
# str(as2$lastFuDay2)
# as2$lastFuDay2 <- as.Date(as2$lastFuDay2)
# 
# as2<-as2 %>% mutate(lastFUDay = case_when(as.POSIXct(lastFuDay) > as.POSIXct(lastFuDay2) ~ lastFuDay,
#                                           as.POSIXct(lastFuDay) <= as.POSIXct(lastFuDay2) ~ lastFuDay2))
# 
# 
# a<-as2 %>% select(ptno, lastFuDay, lastFuDay2, lastFUDay)
# 
# b<-subset(as2, select=-c(lastFuDay,lastFuDay2))
# 
# as<-b
# 
# write.csv(as, "AS_rawdata_환자별.csv")
# 
# quantile(as$lastFUDay)
# str(as$lastFUDay)
# str(as$studyDate)
# 


################0712

# Save multiple objects

# ObjSave <- function(..., folder) {
#   objects <- list(...)
#   object_names <- sapply(substitute(list(...))[-1], deparse)
#   sapply(1:length(objects), function(i) {
#     filename = paste0(folder, "/", object_names[i], ".rda")
#     save(objects[i], file = filename)
#   })
# }
# 
# folder = "data"
# ObjSave(model_1, model_2,model_3,model_4,model_5, folder = folder)

# 
# a <- as[!(as$ptno %in% outcome_list$ptno),]
# 
# 
# 
# a<-as_obs[as_obs$ptno==4071596
#                |as_obs$ptno==3899707
#                |as_obs$ptno==2897438
#                |as_obs$ptno==5979255
#                |as_obs$ptno==198293
#                |as_obs$ptno==8333470,] %>%
#   select(ptno, studyDate, commentForFixedLines)
# 
# 
# write.csv(a,"11.csv")

####################  save data after mutate 0930

# as <- sample_data %>%
#   group_by(ptno) %>%
#   arrange(studyDate) %>%
#   filter(row_number()==1)
# 
# as_obs <- sample_data
# 
# ##outcome
# outcome_list <- read.csv("./data/outcome_list.csv", stringsAsFactors = F)
# as<-left_join(as,outcome_list,by='ptno')
# 
# ## LVEF
# as$avarPeakPressureGradient<-as.numeric(as$avarPeakPressureGradient)
# as$leftVentricleefBymMode<-as.numeric(as$leftVentricleefBymMode)
# 
# as<-as %>% mutate(LVEF = ifelse(is.na(leftVentricleefBy2dMode), leftVentricleefBymMode, leftVentricleefBy2dMode))
# 
# 
# as$avr_date<- as.Date(as$avr_date)
# as$tavi_date<- as.Date(as$tavi_date)
# as$death_date<- as.Date(as$death_date)
# as$avr_sur_date<- as.Date(as$avr_sur_date)
# as$studyDate<- as.Date(as$studyDate)
# 
# ## lastFUDay
#
# a<- as_obs %>%
#   group_by(ptno) %>%
#   arrange(studySeq) %>%
#   mutate(lastFUDay = last(studyDate)) %>%
#   select(ptno, lastFUDay) %>%
#   distinct(ptno, .keep_all = T)
# 
# as<-left_join(as,a,by='ptno')
# as$lastFUDay<- as.Date(as$lastFUDay)
# 
# save(model_1, file = "./data/model_1.rda")
# save(model_2, file = "./data/model_2.rda")
# save(model_3, file = "./data/model_3.rda")
# save(model_4, file = "./data/model_4.rda")
# 
# write.csv(as,"AS_rawdata_patients.csv")
# write.csv(as_obs,"AS_rawdata_echo.csv")

## death 확인
# a <- read.csv("./data/death_list.csv", stringsAsFactors = F)
# b <- tmpDF %>%
#   select(ptno, isGroup2)
# 
# a<- left_join(b, a, by = "ptno")
# 
# a$death_1 <- as.Date(a$death_1, format="%Y-%m-%d")
# a$death_2 <- as.Date(a$death_2, format="%Y-%m-%d")
# 
# a<-a%>%
#   mutate(death_hos = ifelse(is.na(death_1), 0, 1)) %>%
#   mutate(death_kmis = ifelse(is.na(death_2), 0, 1))
# 
# a %>%
#   # filter(isGroup2==0) %>%
#   filter(death_hos == 1) %>%
#   filter(death_kmis == 0) %>%
#   nrow()



# write.csv(as_obs,"AS_rawdata_echo.csv")


