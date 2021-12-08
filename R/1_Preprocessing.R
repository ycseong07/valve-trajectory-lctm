#' Preprocessing
#'
#' @details
#' Data Preprocessing
#'
#' @param echo    echo data
#'  
#' @import tidyverse
#'
#' @export

PreProcessing<-function(echo)
{
  if (is.null(echo)){
    stop("No echo data. It starts from DataAccess function")
    echo<-DataAccess() 
  }
  
  
  ########## TV.Regurgitation cleaning #############
  
  ## remove observations after TAP
  # echo <- echo %>% 
  #   filter(!(grepl("[Ss]\\/[Pp].{1,40}[Tt][Aa][Pp]", commentForFixedLines)))
  # ## echo = 404,245 rows
  # 
  # echo <- echo %>% mutate(tvRegur = case_when(tvRegurgitation %in% c("No","-") ~ "no",  
  #                                             tvRegurgitation %in% c("Trivial") ~ "trivial",  
  #                                             tvRegurgitation %in% c("G I","GI", "I", "l") ~ "G I",
  #                                             tvRegurgitation %in% c("CI-II","G I-II","G I- II","G I_II","G I~II","G I-ll","GI- II","GI-II", "I-II", "G I-OO") ~ "G I-II",
  #                                             tvRegurgitation %in% c("G II", "GII", "II", "G II-II") ~ "G II",
  #                                             tvRegurgitation %in% c("G II-III","G II~III","G II->III", "GII-III", "II-III") ~ "G II-III",
  #                                             tvRegurgitation %in% c("G III","GIII","G III\\") ~ "G III",
  #                                             tvRegurgitation %in% c("G III-IV","GIII-IV", "III-IV") ~ "G III-IV",
  #                                             tvRegurgitation %in% c("G IV", "IV", "G IIII") ~ "G IV"
  #                                             #TRUE ~ NA
  # )
  # )
  # 
  # ##Transforming class of tvRegur into factor
  # echo$tvRegur <- factor(echo$tvRegur, levels = c("no", "trivial", "G I", "G I-II", "G II", "G II-III", "G III", "G III-IV", "G IV"))
  # 
  # ##add numeric column for TV regurgitation
  # echo$tvRegurNum <- as.numeric(echo$tvRegur)
  
  
  ########## AV Stenosis cleaning #############
  
  echo <- echo %>% mutate(avSte = case_when(grepl("[Nn]o",avStenosis) ~ "no",
                                            grepl("[Mm]ild$",avStenosis) ~ "mild",
                                            grepl("[Mm]ild.+[Mm]od*.+",avStenosis) ~ "mild to moderate",
                                            grepl("[Mm]oderate",avStenosis) ~ "moderate",
                                            grepl("[Mm]od.+[Ss]ev.*",avStenosis) ~ "moderate to severe",
                                            avStenosis %in% c("Severe", "R/O Severe","R/O severe","Severe to severe", "R/O severe AS",  "r/o Severe","Very severe") ~ "severe",
                                            avStenosis %in% c("No","-","--") ~ "no",  
                                            avStenosis %in% c("Mlid", "Mild??") ~ "mild",
                                            avStenosis %in% c("Mild to mdorate", "Mild to oderate") ~ "mild to moderate"
                                            #TRUE ~ NA
  )
  )
  
  ##Transforming class of AV Stenosis into factor
  echo$avSte <- factor(echo$avSte, levels = c("no", "mild", "mild to moderate", "moderate", "moderate to severe", "severe"))
  
  ##add numeric column for AV Stenosis
  echo$avSteNum <- as.numeric(echo$avSte)
  
  echo<-echo %>% mutate(LVEF = ifelse(is.na(leftVentricleefBy2dMode), leftVentricleefBymMode, leftVentricleefBy2dMode))
  
  return(echo)
}





