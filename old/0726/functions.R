
# Data Input --------------------------------------------------------------



# data input of Sentex files
dataSentex <- function(fileName){
  temp <- read.csv2(fileName, stringsAsFactors = F)
  for (i in 2:ncol(temp)){
    temp[,i] <- as.numeric((temp[,i]))
  }
  temp[,1] <- as.Date(temp[,1], format = "%d.%m.%Y")
  return(temp)
}

# update data
## take data, reduced dates, indexes where duplicated
updateDataSentex <- function(dat, dateRed){
  temp <- dat[dat$Datum %in% dateRed,]
  temp <- unique(temp)
  return(temp)
}



# Plot --------------------------------------------------------------------



# plot index with sentiment
plotIndexSent <- function(dat){
  ### in same graph
  # https://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
  
  pdf(file = paste0(folderPlotIndexSent, "/", deparse(substitute(dat)), ".pdf"), 
      width = 12, height = 8)
  
  # parameter for second y-axis (for sentiment)
  ylim2 = c(0.3, 0.9)
  # add extra space to right margin of plot within frame
  par(mar=c(5, 4, 4, 5) + 0.1)
  
  plot(dat$Datum, dat$Index, type = "l", xlab = "Date", ylab = "Index",  lwd = 2)
  par(new=T)
  plot(dat$Datum, dat$Sent_Is, type = "l",  axes = F, 
       xlab="", ylab="", col = "red", ylim = ylim2)
  par(new=T)
  plot(dat$Datum, dat$Sent_Ps, type = "l",  axes = F, 
       xlab="", ylab="", col = "blue", ylim = ylim2)
  
  title(deparse(substitute(dat)))
  legend("topleft",legend=c("Index","Is", "Ps"),
         text.col=c("black","red", "blue"),lty=c(1,1,1),col=c("black","red", "blue"))
  axis(side=4, at = pretty(ylim2))
  mtext("Sentiment",side=4,line=3) 
  
  dev.off()
}


# Optimization ------------------------------------------------------------

g <- function(x){
  return(c(1.0001-sum(x), sum(x) - 0.9999))
}

# goal function with 3 criteria
### give mu and S to speed up (do not need to calculate it every time out of R)
f <- function(x, w, mu, S, TargetRpm, TargetVolpa){
  y <- numeric(3)
  y[1] <- -1.0 * w[1] * drop(crossprod(x, mu)) / TargetRpm
  y[2] <- w[2] * drop(sqrt(t(x) %*% S %*% x)) * sqrt(12) / TargetVolpa
  y[3] <- w[3] * sum((mrc(x, S) / 100)^2)
}

library(mco)
minSentiment <- function(){
  
}