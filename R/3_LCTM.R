#' LCTM
#'
#' @details
#' LCTM
#' 
#' @param sample    echo sample_data
#' @param seed_num seed_num
#' @param fileName fileName
#'  
#' @import lcmm
#' @import gridExtra
#' @import ggplot2
#'
#' @export



LCTMLinear<-function(seed_num = 100, sample=NULL)
{
  
   set.seed(seed_num)
   modelL_1 <- lcmm::hlme(fixed = avarMeanPressureGradient ~ diffInDay,
                         random = ~ diffInDay,
                         ng = 1,
                         subject = "ptno",
                         data = data.frame(sample) )

   for (i in 2:6){
     set.seed(seed_num)
     m<-paste("modelL_",i,sep = "")

     assign(m, lcmm::hlme(fixed = avarMeanPressureGradient ~ diffInDay,
                          mixture= ~ diffInDay,
                          random = ~ diffInDay,
                          ng = i,
                          nwg = TRUE,
                          subject = "ptno",
                          idiag = TRUE,
                          data = data.frame(sample) ),
            envir = parent.frame())
     
  }
  
}

#' LCTMLinearAddCV
#'
#' @details
#' LCTMLinearAddCV
#' 
#' @param sample    echo sample_data
#' @param seed_num seed_num
#'  
#' @import lcmm
#' @import gridExtra
#' @import ggplot2
#'
#' @export

LCTMLinearAddCV<-function(seed_num = 100, sample=NULL )
{
  
  set.seed(seed_num)
  assign("modelLCV_1", lcmm::hlme(fixed = avarMeanPressureGradient ~ diffInDay + sexDummy + age,
                        random = ~ diffInDay,
                        ng = 1,
                        subject = "ptno",
                        data = data.frame(sample) ),
         envir = parent.frame())
  
  for (i in 2:6){
    set.seed(seed_num)
    m<-paste("modelLCV_",i,sep = "")
    
    assign(m, lcmm::hlme(fixed = avarMeanPressureGradient ~ diffInDay + sexDummy + age,
                         mixture= ~ diffInDay,
                         random = ~ diffInDay,
                         ng = i,
                         nwg = TRUE,
                         subject = "ptno",
                         idiag = TRUE,
                         data = data.frame(sample) ),
           envir = parent.frame())
    
  }
  
}

LCTMQuadratic<-function(sample,
                     seed_num = 100)
{
  if (is.null(sample)){
    stop("No sample data.")
  }
  
  # set.seed(seed_num)
  # model_1 <- lcmm::hlme(fixed = avarMeanPressureGradientLog ~ diffInDay + I(diffInDay^2), 
  #                       random = ~ diffInDay, 
  #                       ng = 1, 
  #                       subject = "ptno", 
  #                       data = data.frame(sample) )
  
  for (i in 2:4){
    set.seed(seed_num)
    m<-paste("model_",i,sep = "")
    
    assign(m, lcmm::hlme(fixed = avarMeanPressureGradientLog ~ 1+diffInDay + I(diffInDay^2), 
                         mixture= ~ diffInDay, 
                         random = ~ diffInDay, 
                         ng = i, 
                         nwg = TRUE,  
                         subject = "ptno",
                         idiag = TRUE,
                         data = data.frame(sample) ), 
           envir = parent.frame())
  }
  
  
}

LCTMCubic<-function(sample,
                        seed_num = 100)
{
  if (is.null(sample)){
    stop("No sample data.")
  }
  
  # set.seed(seed_num)
  # model_1 <- lcmm::hlme(fixed = avarMeanPressureGradientLog ~ diffInDay + I(diffInDay^2), 
  #                       random = ~ diffInDay, 
  #                       ng = 1, 
  #                       subject = "ptno", 
  #                       data = data.frame(sample) )
  
  for (i in 2:4){
    set.seed(seed_num)
    m<-paste("model_",i,sep = "")
    
    assign(m, lcmm::hlme(fixed = avarMeanPressureGradientLog ~ 1+diffInDay + I(diffInDay^2) + I(diffInDay^3), 
                         mixture= ~ diffInDay, 
                         random = ~ diffInDay, 
                         ng = i, 
                         nwg = TRUE,  
                         subject = "ptno",
                         idiag = TRUE,
                         data = data.frame(sample) ), 
           envir = parent.frame())
  }
  
  
}


#' LCTMPlot
#'
#' @details
#' LCTMPlot
#' 
#' @param sample    echo sample_data
#' @param fileName fileName
#'  
#' @import lcmm
#' @import gridExtra
#' @import ggplot2
#'
#' @export

LCTMPlot <- function(sample, fileName){
  
  ##group 2
  people2 <- as.data.frame(modelLCV_2$pprob[,1:2])
  
  sample$group2 <- factor(people2$class[sapply(as.numeric(sample$ptno), function(x) which(people2$ptno == x))])
  
  #plot check
  p1 <- ggplot(sample, aes(x=diffInDay, y=avarMeanPressureGradient, group = ptno, colour = group2)) +
    geom_line() +
    geom_smooth(aes(group=group2), method = "loess", size = 2, se = F)  +
    #scale_y_continuous(limits = c(0,8)) +
    theme(text = element_text(size=15)) +
    labs(x = "x", y = "y", colour = "Latent Class", title = "Raw")
  
  p2 <- ggplot(sample, aes(x=diffInDay, y=avarMeanPressureGradient, group = ptno, colour = group2)) +
    geom_smooth(aes(group = ptno, colour = group2),size = 0.5, se = F) +
    geom_smooth(aes(group = group2), method = "loess", size = 2, se = T)  +
    #scale_y_continuous(limits = c(0,8)) +
    theme(text = element_text(size=15)) +
    labs(x = "x",y = "y",colour = "Latent Class", title = "Smoothed") +
    theme(legend.position = "none")
  
  plot_2<<-grid.arrange(p1,p2, ncol = 2, top = "2 Latent Classes")
  
  ggsave(paste("./plot/",fileName,"_2.png"),plot_2, width=26, height=12, units="cm", dpi = 300)
  
  ##group 3
  people3 <- as.data.frame(modelLCV_3$pprob[,1:2])
  
  sample$group3 <- factor(people3$class[sapply(as.numeric(sample$ptno), function(x) which(people3$ptno == x))])
  
  #plot check
  p1 <- ggplot(sample, aes(x=diffInDay, y=avarMeanPressureGradient, group = ptno, colour = group3)) +
    geom_line() +
    geom_smooth(aes(group=group3), method = "loess", size = 2, se = F)  +
    #scale_y_continuous(limits = c(0,8)) +
    theme(text = element_text(size=15)) +
    labs(x = "x", y = "y", colour = "Latent Class", title = "Raw")
  
  p2 <- ggplot(sample, aes(x=diffInDay, y=avarMeanPressureGradient, group = ptno, colour = group3)) +
    geom_smooth(aes(group = ptno, colour = group3),size = 0.5, se = F) +
    geom_smooth(aes(group = group3), method = "loess", size = 2, se = T)  +
    #scale_y_continuous(limits = c(0,8)) +
    theme(text = element_text(size=15)) +
    labs(x = "x", y = "y",colour = "Latent Class", title = "Smoothed") +
    theme(legend.position = "none")
  
  plot_3<<-grid.arrange(p1,p2, ncol = 2, top = "3 Latent Classes")
  
  ggsave(paste("./plot/",fileName,"_3.png"),plot_3, width=26, height=12, units="cm", dpi = 300)
  
  ##group 4
  people4 <- as.data.frame(modelLCV_4$pprob[,1:2])
  
  sample$group4 <- factor(people4$class[sapply(as.numeric(sample$ptno), function(x) which(people4$ptno == x))])
  
  #plot check
  p1 <- ggplot(sample, aes(x=diffInDay, y=avarMeanPressureGradient, group = ptno, colour = group4)) +
    geom_line() +
    geom_smooth(aes(group=group4), method = "loess", size = 2, se = F)  +
    #scale_y_continuous(limits = c(0,8)) +
    theme(text = element_text(size=15)) +
    labs(x = "x", y = "y", colour = "Latent Class", title = "Raw")
  
  p2 <- ggplot(sample, aes(x=diffInDay, y=avarMeanPressureGradient, group = ptno, colour = group4)) +
    geom_smooth(aes(group = ptno, colour = group4),size = 0.5, se = F) +
    geom_smooth(aes(group = group4), method = "loess", size = 2, se = T)  +
    #scale_y_continuous(limits = c(0,8)) +
    theme(text = element_text(size=15)) +
    labs(x = "x",y = "y",colour = "Latent Class", title = "Smoothed") +
    theme(legend.position = "none")
  
  plot_4<<-grid.arrange(p1,p2, ncol = 2, top = "4 Latent Classes")
  
  ggsave(paste("./plot/",fileName,"_4.png"),plot_4, width=26, height=12, units="cm", dpi = 300)
  
  sample_grouped <<- sample
  
}










