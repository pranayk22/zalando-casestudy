
# functions 

removeOutliers = function(value) {
  q1 <- summary(value)[[2]]
  q3 <- summary(value)[[5]]
  
  iqr <- q3 - q1
  minValue <- q1 - iqr * 1.5
  maxValue <- q3 + iqr * 1.5
  
  attr <-
    list(
      'q1' = q1 ,
      'q3' = q3 ,
      'iqr' = iqr  ,
      'minValue' = minValue ,
      'maxValue' = maxValue
    )
  return(attr)
}

# generate automatically 

monthOrder <- c(
    "Dec-2016" ,  
    "Jan-2017" , 
    "Feb-2017" , 
    "Mar-2017"  , 
    "Apr-2017", 
    "May-2017" ,
    "Jun-2017" ,
    "Jul-2017",
    "Aug-2017", 
    "Sep-2017" ,
    "Oct-2017" ,
    "Nov-2017" ,
    "Dec-2017"
  )

# to plot line function custom functions 

plotLine = function( dt , xcol, ycol , title , ylabel ){
  
  p <- ggplot( data= dt, aes(x= factor( xcol , levels= monthOrder), y= ycol)) +
   # geom_point()+
    geom_line(aes(group=1) , color="#A02141", size=0.5)+
    ggtitle( paste0(title) ) +
    labs(y= paste0(ylabel))+
    scale_x_discrete(name ="")+
    theme_bw()+
    theme( axis.text.x=element_text(size=7, angle=90,hjust=0.95,vjust=0.2) 
          , axis.text.y=element_text(size=7) 
          , axis.title.y=element_text(size=8)
          , panel.border = element_blank() , 
          plot.title = element_text(size = 8))
  
  return(p)
  
}











