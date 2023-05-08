findminmax <- function(data, minimise = TRUE){
    minmax <- NA
    if (minimise) minmax <- min(data[,2])
    else minmax <- max(data[,2])
      
    rownum <- which(data[,2] == minmax)
    if (length(rownum) > 1) rownum <- rownum[1]
    
    if (minimise)
      return (minmax - data [rownum,3])
    else return (minmax + data [rownum,3])
}

plotbars<- function(data1, 
                    cap1 = "GA1"){
  data = data1
  hues = c("red","violet","darkblue","cyan")
  
  min1 = findminmax(data1)   #min(data1) - data1 [which(data1 == min(data1))+2*nrow(data1)]
  
  
  max1 = findminmax(data1, FALSE)   #max(data1) + data1 [which(data1 == max(data1))+nrow(data1)]

  
  minn = min(min1)
  maxx = max(max1)

  
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar
  plot(df$x, df$y, type = "l", col = hues[1],  ylim=c(minn, maxx), #ylim = c(0.96, 0.985),   #choose ylim CAREFULLY as per your data ranges
        main = "Best Fitness Values", xlab = "Generations", ylab = "Fitness")  #plot the line (mean values)
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[3]);    #plot the error bars mean-errorbar, mean+errorbar
  
   
  

  
  legend("bottomright", legend = c(cap1), col = hues, lwd = 1,
         cex = 0.5)
}


