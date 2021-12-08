#' Data Access
#'
#' @details
#' Data Access
#'
#' @param csvFilePath    file path for the excel file. csv file needs to be encoded as ANSI on Window OS.
#' @param echo echo
#' 
#' @import dplyr
#' 
#' @export

DataAccess <- function(echo = NULL,
                       csvFilePath = NULL)
  
  {
  if (is.null(echo)){
  echo  <-  read.csv(csvFilePath, stringsAsFactors = F)
  }
  
  # echo <- echo %>%
  #   rename( 
  #     templateName= "TEMPLATENAME", 
  #     statusCode= "STATUSCODE", 
  #     ptno= "PTNO", 
  #     patientName= "PATIENTNAME", 
  #     studyDate= "STUDYDATE", 
  #     reportDate= "REPORTDATE", 
  #     commentForFixedLines= "comment.for.fixed.lines", 
  #     location= "Location", 
  #     physician= "Physician", 
  #     indication= "Indication", 
  #     height= "Height", 
  #     weight= "Weight", 
  #     bsa= "BSA", 
  #     sbp= "SBP", 
  #     dbp= "DBP", 
  #     regionalWallMotionForhy= "Regional.Wall.Motion.for.HY", 
  #     regionalWallMotionForhy_1= "Regional.Wall.Motion.for.HY..1.", 
  #     regionalWallMotionForhy_5= "Regional.Wall.Motion.for.HY..5.", 
  #     regionalWallMotionForhy_6= "Regional.Wall.Motion.for.HY..6.", 
  #     mvStructure= "MV.Structure", 
  #     mvProlapse= "MV.Prolapse", 
  #     avStructure= "AV.Structure", 
  #     avVegetationsProlapse= "AV.Vegetations.Prolapse", 
  #     tvStructure= "TV.Structure", 
  #     tvProlapse= "TV.Prolapse", 
  #     mvRegurgitation= "MV.Regurgitation", 
  #     avRegurgitation= "AV.Regurgitation", 
  #     tvRegurgitation= "TV.Regurgitation", 
  #     pvRegurgitation= "PV.Regurgitation", 
  #     mvStenosis= "MV.Stenosis", 
  #     avStenosis= "AV.Stenosis", 
  #     tvStenosis= "TV.Stenosis", 
  #     pvStenosis= "PV.Stenosis", 
  #     pericardialEffusion= "Pericardial.Effusion", 
  #     interacardiacThrombi= "Interacardiac.Thrombi", 
  #     interacariacMass= "Interacariac.mass", 
  #     diastolicFunctionForhy= "Diastolic.function..for.HY", 
  #     rvddBy2dMode= "RVDd.by.2D.Mode", 
  #     lviddBymMode= "LVIDd.by.M.Mode", 
  #     lvidsBymMode= "LVIDs.by.M.Mode", 
  #     leftVentricleefBymMode= "Left.Ventricle.EF.by.M.Mode", 
  #     leftVentricleefBy2dMode= "Left.Ventricle.EF.by.2D.Mode", 
  #     tvAnnulusAtrvot= "TV.Annulus.at.RVOT", 
  #     ivc= "IVC", 
  #     msEchoScore= "MS.Echo.Score", 
  #     msEchoScoreValveMobility= "MS.Echo.Score.Valve.Mobility", 
  #     msEchoScoreValveThickness= "MS.Echo.Score.Valve.Thickness", 
  #     msEchoScoreSubvalThickness= "MS.Echo.Score.Subval.Thickness", 
  #     msEchoScoreCalcification= "MS.Echo.Score.Calcification", 
  #     ivsdBymMode= "IVSd.by.M.mode", 
  #     ivssBymMode= "IVSs.by.M.mode", 
  #     lvpwdBymMode= "LVPWd.by.M.mode", 
  #     lvpwsBymMode= "LVPWs.by.M.mode", 
  #     lvotDimensionByDopplerMode= "LVOT.Dimension.by.Doppler.Mode", 
  #     lvMassBymMode= "LV.Mass.by.M.Mode", 
  #     lvmiBymMode= "LVMI.by.M.Mode", 
  #     rwt= "RWT", 
  #     aorticRootDimensionBymMode= "Aortic.Root.Dimension.by.M.Mode", 
  #     ladBymMode= "LAD.by.M.Mode", 
  #     ladim22ch= "LA.Dim2_2Ch", 
  #     laChamber= "LA.Chamber", 
  #     laVolumeBymMode= "LA.Volume.by.M.Mode", 
  #     laVolumeBymMode_1= "LA.Volume.by.M.Mode..1.", 
  #     laVolumeIndexBymMode= "LA.Volume.Index.by.M.Mode", 
  #     laVolumeIndexBymMode_1= "LA.Volume.Index.by.M.Mode..1.", 
  #     mvAnnulusToPlax= "MV.Annulus.to.PLAX", 
  #     lvotvti= "LVOT.VTI", 
  #     avvti= "AV.VTI", 
  #     lvotPeakVelocity= "LVOT.Peak.Velocity", 
  #     avPeakPressureGradient= "AV.Peak.Pressure.Gradient", 
  #     avMeanPressureGradient= "AV.Mean.Pressure.Gradient", 
  #     avPeakVelocity= "AV.Peak.Velocity", 
  #     avarPeakPressureGradient= "AV.AR.Peak.Pressure.Gradient", 
  #     avarMeanPressureGradient= "AV.AR.Mean.Pressure.Gradient", 
  #     avaByPlanimetry= "AVA.by.Planimetry", 
  #     avaByContinuityEquation= "AVA.by.Continuity.Equation", 
  #     avpht= "AV.PHT", 
  #     mvvticw= "MV.VTI.CW.", 
  #     eWaveVelocity= "E.wave.Velocity", 
  #     aWaveVelocity= "A.wave.Velocity", 
  #     dt= "DT", 
  #     mvPht= "MV.PHT", 
  #     mvMeanPressureGradient= "MV.Mean.Pressure.Gradient", 
  #     mvhr= "MV.HR", 
  #     mvaByPlanimetry= "MVA.by.Planimetry", 
  #     mvaBypht= "MVA.by.PHT", 
  #     mvaByContinuityEquation= "MVA.by.Continuity.Equation", 
  #     tvtrJet= "TV.TR.jet", 
  #     rightVentricularPeakSystolicPressurervsp= "Right.Ventricular.Peak.Systolic.Pressure.RVSP", 
  #     rap= "RAP", 
  #     tvMeanPressureGradient= "TV.Mean.Pressure.Gradient", 
  #     pvprPeakPressureGradient= "PV.PR.Peak.Pressure.Gradient", 
  #     pvprMeanPressureGradient= "PV.PR.Mean.Pressure.Gradient", 
  #     s= "S.", 
  #     e= "E.", 
  #     a= "A.", 
  #     ee= "E.E.", 
  #     tricuspidmpap= "Tricuspid...mPAP", 
  #     pulmonicrvotat= "Pulmonic...RVOT.AT", 
  #     tricuspidpvr= "Tricuspid...PVR", 
  #     rvotvti= "RVOT.VTI", 
  #     inspectionItem= "검사항목", 
  #     inspectionItem_1= "검사항목..1."
  #   )
  

  ## remove dummy observations
  echo <- echo %>% 
    filter(!(grepl("임시메모", commentForFixedLines) | grepl("임시 메모", commentForFixedLines)))
  ## echo = 407,768 rows
  
  echo <- echo %>% 
    filter(!(grepl("TEST", ptno)))
  ## echo = 407,757 rows
  
  echo <- echo %>% 
    filter(!(grepl("YOU", ptno)))
  ## echo = 407,756 rows
  
  echo <- echo %>% 
    filter(!(grepl("TEST", patientName)))
  ##echo = 407,751 rows
  
  echo <- echo %>% 
    filter(ptno!="00000000")
  ##echo = 407,750 rows
  
  ## ptno cleaning
  echo$ptno<-echo$ptno %>%
    gsub("\\-.?[0-9]*","",.)
  
  ##remove remain speicial char
  echo$ptno<-echo$ptno %>%
    gsub("[$&+,:;=?@#|'<>.^*()%!-/]","",.)
  
  ##remove a-Z
  echo$ptno<-echo$ptno %>%
    gsub("[a-zA-Z]","",.)
  
  echo$ptno <- as.numeric(echo$ptno)
  
  ######### any non-numeric ptno?
  # non_num_ptno<-echo %>%
  #   filter(!grepl("^[0-9]+$",ptno))
  
  ## make study sequence/total observation variables
  echo <- echo %>% 
    arrange(ptno,studyDate)
  
  echo$studySeq <- as.numeric(ave(echo$ptno, echo$ptno, FUN=seq_along))    
  echo$totalObs <- as.numeric(ave(echo$ptno, echo$ptno, FUN=length))       
  
  ## Making date gap variable (It takes long time...)
  echo$studyDate <- as.Date(echo$studyDate)
  
  echo <- echo %>% 
    group_by(ptno) %>% 
    mutate(diffInSec = as.POSIXct(studyDate, format = "%Y/%m/%d") - first(as.POSIXct(studyDate, format = "%Y/%m/%d"))) %>%
    mutate(diffInDay = as.numeric(diffInSec/(60*60*24)))
  
  ##Last echo date - first echo date
  echo <- echo %>% 
    group_by(ptno) %>%
    mutate(termOfEcho = as.numeric(last(as.POSIXct(studyDate, format = "%Y/%m/%d")) - first(as.POSIXct(studyDate, format = "%Y/%m/%d"))))
  
  ##caculate age
  age = function(from, to) {
    from_lt = as.POSIXlt(from)
    to_lt = as.POSIXlt(to)
    
    age = to_lt$year - from_lt$year
    
    ifelse(to_lt$mon < from_lt$mon |
             (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
           age - 1, age)
  }
  
  echo$studyDate<-strftime(echo$studyDate, format="%Y-%m-%d")
  echo$studyDate<-as.Date(echo$studyDate, format="%Y-%m-%d")
  echo$birthDate<-as.Date(echo$birthDate, format="%Y-%m-%d")
  
  echo$age<-age(echo$birthDate, echo$studyDate)
  
  ##sex
  echo$sexDummy<-NA
  echo$sexDummy[echo$sex=="M"]<-0
  echo$sexDummy[echo$sex=="F"]<-1
  
  return(echo)
}

