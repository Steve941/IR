source("parameters.R")

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


# ** Upgrade Sentex Data --------------------------------------------------

library(FSelector) # to generate formula easily

### developped further to regSent
# dispersionSent <- function(dat){
#   temp <- dat
#   
#   for(i in 1:nrow(dat)){
#     for(k in colnames(dat)){
#       # generate formula (regress one column on all the others while using all the previous data (including today))
#       form <- as.simple.formula(setdiff(colnames(dat), k), k)
#       temp[i, k] <- unname(lm(form, data = dat[1:i,])$coefficients[1])
#     }
#   }
#   
#   return(temp)
# }

regSent <- function(dat, consider = 50){
    temp <- dat
    
    for(i in 1:nrow(dat)){
        for(k in colnames(dat)){
            # generate formula (regress one column on all the others while using 'consider' previous points)
            form <- as.simple.formula(setdiff(colnames(dat), k), k)
            temp[i, k] <- unname(lm(form, data = dat[max((i-consider),1):i,])$coefficients[1])
        }
    }
    
    return(temp)
}



# ** Actual Optimization -----------------------------------------------------



g <- function(x){
  return(c(1.0001-sum(x), sum(x) - 0.9999))
}

# goal function with 3 criteria
### give mu and S to speed up (do not need to calculate it every time out of R)
### TODO: somehow not working -> have to give global parameters
f <- function(x, w, muStock, SStock, targetRpm, targetVolpa, SSent){
  y <- numeric(3)
  y[1] <- -1.0 * w[1] * drop(crossprod(x, muStock)) / targetRpm
  y[2] <- w[2] * drop(sqrt(t(x) %*% SStock %*% x)) * sqrt(12) / targetVolpa
  y[3] <- w[3] * sum((mrc(x, SSent))^2)
  return(y)
}

h <- function(x){
  y <- numeric(3)
  y[1] <- -1.0 * w[1] * drop(crossprod(x, muStock)) / targetRpm
  y[2] <- w[2] * drop(sqrt(t(x) %*% SStock %*% x)) * sqrt(12) / targetVolpa
  y[3] <- w[3] * sum((mrc(x, SSent))^2)
  return(y)
}

hWeighted <- function(x){
  return(sum(h(x)))
}

library(mco)
library(FRAPO)
# minimize sentiment via marginal contribution to risk 
### TODO take closer look if this is really how Sentiment can be included (-> portfolio analysis)
# minSentiment <- function(muStock, SStock, targetRpm, targetVolpa, SSent, w = w, f = f, g = g){
#   NAsset = length(muStock)
#   ans <- nsga2(f, NAsset, 3,
#                w = w, muStock = muStock, SStock = SStock, 
#                targetRpm = targetRpm, targetVolpa = targetVolpa, SSent = SSent,
#                lower.bounds = rep(0, NAsset), upper.bounds = rep(1, NAsset), 
#                constraints = g, cdim = 2, 
#                popsize = 500)
#   return(ans)
# }

minSentiment <- function(SSent){
  NAsset = length(muStock)
  ans <- nsga2(h, NAsset, 3,
               lower.bounds = rep(0, NAsset), upper.bounds = rep(1, NAsset),
               constraints = g, cdim = 2,
               popsize = 500)
  return(ans)
}


# Analysis of Portfolio --------------------------------------------------

## Take weights in each date of datesEval and calculate the portfolio return
evolve <- function(x){
    retFac <- ret[rownames(ret)%in%datesEval[2:length(datesEval)],]*
        x[1:(length(datesEval)-1),] # weight at time t and on these returns to time t+1 made
    retFac <- 1+rowSums(retFac)
    retFac <- c(100, retFac) # initialize with 100 and multiply the returns (next line)
    return(timeSeries(cumprod(retFac), charvec = datesEval))
}

## Take list (each element is another portfolio) as input and return portfolio characteristics
## uses global variables: ret, datesEval
analysePortfolio <- function(l){
    RsClassic <- matrix(unlist(lapply(l, Return.calculate)), ncol = length(l)) # return from time to time
    RsTSClassic <- na.omit(xts(RsClassic, order.by = as.Date(datesEval)))
    bench <- xts(rep(0, nrow(RsTSClassic)), order.by = as.Date(datesEval)[-1]) # benchmark 0 everytime
    
    S1 <- as.matrix(table.AnnualizedReturns(RsTSClassic, Rf = bench)) # scale is chosen automatically to 'weekly'
    S2 <- -1*VaR(RsTSClassic) # 95% as default (outputs small negative value)
    
    ans <- rbind(S1, S2)
    colnames(ans) <- names(l)
    rownames(ans) <- c("Return (p.a.)", "StdDev. Risk (p.a.)", "Sharpe Ratio", "VaR 95% (p.a.)")
    return(round(ans, 3))
}

# Spielwiese --------------------------------------------------------------


library(Rdonlp2) ## needed for donlp2NLP
optMinSent <- function(SSent){
  
}