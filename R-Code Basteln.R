# Call files with R-Code
source("parameters.R")
source("functions.R")


# Data Import -------------------------------------------------------------


# ** SentVar  -------------------------------------------------------------
SentVar_I_1M <- dataSentex(file.path(folderData, "Sentix", "Var_Is_2001_2014_1M.csv"))
SentVar_P_1M <- dataSentex(file.path(folderData, "Sentix", "Var_Ps_2001_2014_1M.csv"))
SentVar_I_6M <- dataSentex(file.path(folderData, "Sentix", "Var_Is_2001_2014_6M.csv"))
SentVar_P_6M <- dataSentex(file.path(folderData, "Sentix", "Var_Ps_2001_2014_6M.csv"))

dates <- as.Date(SentVar_I_1M[,1], format = "%d.%m.%Y")
### Testing other dates (works)
# dates2 <- as.Date(SentVar_P_1M[,1], format = "%d.%m.%Y")
# sum(!(dates %in% dates2))
# sum(!(dates2 %in% dates))

# aus irgendeinem Grund taucht "2013-04-05" zweimal auf
dates[duplicated(dates)]
sum(dates==as.Date("2013-04-05"))
dates <- unique(dates)






# ** Index ----------------------------------------------------------------

# install.packages("quantmod")
library(quantmod)
?getSymbols



# as some indexes appear with na's, we store the indexes of dates, on which not all data is provided
datNA <- numeric()

### S&P 500
sp500 <- new.env()
getSymbols("^GSPC", env = sp500, src = "yahoo", from = dateMin, to = dateMax)
SP500 <- as.matrix(sp500$GSPC[dates,"GSPC.Close"])

sum(is.na(SP500[,"GSPC.Close"]))


### DAX
dax <- new.env()
getSymbols("^GDAXI", env = dax, src = "yahoo", from = dateMin, to = dateMax)
DAX <- as.matrix(dax$GDAXI[dates,"GDAXI.Close"])

# DAX contains (still) missing values
sum(is.na(DAX[,"GDAXI.Close"]))
datNA <- c(datNA, rownames(DAX)[which(is.na(DAX[,"GDAXI.Close"]))])

### ESX50
esx50 <- new.env()
getSymbols("^STOXX50E", env = esx50, src = "yahoo", from = dateMin, to = dateMax)
ESX50 <- as.matrix(esx50$STOXX50E[dates,"STOXX50E.Close"])

# ESX50 cotains (still) missing values
sum(is.na(ESX50[,"STOXX50E.Close"]))
datNA <- c(datNA, rownames(ESX50)[which(is.na(ESX50[,"STOXX50E.Close"]))])


### Nasdaq
nasdaq <- new.env()
getSymbols("^NDX", env = nasdaq, src = "yahoo", from = dateMin, to = dateMax)
NASDAQ <- as.matrix(nasdaq$NDX[dates,"NDX.Close"])
sum(is.na(NASDAQ[,"NDX.Close"]))


### Nikkei
nikkei <- new.env()
getSymbols("^N225", env = nikkei, src = "yahoo", from = dateMin, to = dateMax)
NIKKEI <- as.matrix(nikkei$N225[dates,"N225.Close"])

# NIKKEI cotains (still) missing values
sum(is.na(NIKKEI[,"N225.Close"]))
datNA <- c(datNA, rownames(NIKKEI)[which(is.na(NIKKEI[,"N225.Close"]))])

datNA <- as.Date(unique(datNA))

### Now, remove the dates, on which there are NAs
dates <- as.Date(setdiff(dates, datNA))

### doesn't work somehow (alternative below)
### wanted to just get rid of some values, but it converted to named 
### vector and we got problems downwards (when excluding the dates with no information)
### => better to use positive statement ("just the data that we want" instead of deleting data)
SP500 <- SP500[as.Date(rownames(SP500)) %in% dates,]
DAX <- DAX[as.Date(rownames(DAX)) %in% dates,]
ESX50 <- ESX50[as.Date(rownames(ESX50)) %in% dates,]
NASDAQ <- NASDAQ[as.Date(rownames(NASDAQ)) %in% dates,]
NIKKEI <- NIKKEI[as.Date(rownames(NIKKEI)) %in% dates,]

### check, if ok
sum(is.na(SP500))+ 
  sum(is.na(DAX))+
  sum(is.na(ESX50))+
  sum(is.na(NASDAQ))+
  sum(is.na(NIKKEI))


# ** Adoptation -----------------------------------------------------------



############## Problem:
# nicht gleich lang (nicht alle Dates in SP500 angegeben)
length(dates)
length(SP500)

dates[!(dates %in% as.Date(names(SP500)))]
sum(!(as.Date(names(SP500)) %in% dates))

### dates with all Indexes having reported a value
datIndexAll <- dates[dates %in% as.Date(intersect(names(SP500), 
                                                  intersect(names(DAX),
                                                            intersect(names(ESX50),
                                                                      intersect(names(NASDAQ), names(NIKKEI))))))]
### Werte weiter einschränken, sodass an diesen Daten alle Werte vorhanden sind
as.Date(setdiff(dates, datIndexAll))

SP500 <- SP500[as.Date(names(SP500)) %in% datIndexAll]
DAX <- DAX[as.Date(names(DAX)) %in% datIndexAll]
ESX50 <- ESX50[as.Date(names(ESX50)) %in% datIndexAll]
NASDAQ <- NASDAQ[as.Date(names(NASDAQ)) %in% datIndexAll]
NIKKEI <- NIKKEI[as.Date(names(NIKKEI)) %in% datIndexAll]

# jetzt alle gleich lang
(length(datIndexAll) == length(SP500) &&
    length(datIndexAll) == length(DAX) &&
    length(datIndexAll) == length(ESX50) &&
    length(datIndexAll) == length(NASDAQ) &&
    length(datIndexAll) == length(NIKKEI))



SentVar_I_1M <- updateDataSentex(SentVar_I_1M, datIndexAll)
SentVar_P_1M <- updateDataSentex(SentVar_P_1M, datIndexAll)
SentVar_I_6M <- updateDataSentex(SentVar_I_6M, datIndexAll)
SentVar_P_6M <- updateDataSentex(SentVar_P_6M, datIndexAll)






# Visualisation -----------------------------------------------------------


### IndexPreparation
# Collect data in dataframe as matrix would mess up with date format (just numeric values)
SP500_1M <- data.frame(Datum = datIndexAll, Index = as.vector(SP500), Sent_Is = SentVar_I_1M[,"SP500_Is"], Sent_Ps <- SentVar_P_1M[,"SP500_Ps"])
plotIndexSent(SP500_1M)
SP500_6M <- data.frame(Datum = datIndexAll, Index = as.vector(SP500), Sent_Is = SentVar_I_6M[,"SP500_Is"], Sent_Ps <- SentVar_P_6M[,"SP500_Ps"])
plotIndexSent(SP500_6M)

DAX_1M <- data.frame(Datum = datIndexAll, Index = as.vector(DAX), Sent_Is = SentVar_I_1M[,"Dax_Is"], Sent_Ps <- SentVar_P_1M[,"Dax_Ps"])
plotIndexSent(DAX_1M)
DAX_6M <- data.frame(Datum = datIndexAll, Index = as.vector(DAX), Sent_Is = SentVar_I_6M[,"Dax_Is"], Sent_Ps <- SentVar_P_6M[,"Dax_Ps"])
plotIndexSent(DAX_6M)

ESX50_1M <- data.frame(Datum = datIndexAll, Index = as.vector(ESX50), Sent_Is = SentVar_I_1M[,"ESX50_Is"], Sent_Ps <- SentVar_P_1M[,"ESX50_Ps"])
plotIndexSent(ESX50_1M)
ESX50_6M <- data.frame(Datum = datIndexAll, Index = as.vector(ESX50), Sent_Is = SentVar_I_6M[,"ESX50_Is"], Sent_Ps <- SentVar_P_6M[,"ESX50_Ps"])
plotIndexSent(ESX50_6M)

NASDAQ_1M <- data.frame(Datum = datIndexAll, Index = as.vector(NASDAQ), Sent_Is = SentVar_I_1M[,"NASDAQ_Is"], Sent_Ps <- SentVar_P_1M[,"NASDAQ_Ps"])
plotIndexSent(NASDAQ_1M)
NASDAQ_6M <- data.frame(Datum = datIndexAll, Index = as.vector(NASDAQ), Sent_Is = SentVar_I_6M[,"NASDAQ_Is"], Sent_Ps <- SentVar_P_6M[,"NASDAQ_Ps"])
plotIndexSent(NASDAQ_6M)

NIKKEI_1M <- data.frame(Datum = datIndexAll, Index = as.vector(NIKKEI), Sent_Is = SentVar_I_1M[,"Nikkei_Is"], Sent_Ps <- SentVar_P_1M[,"Nikkei_Ps"])
plotIndexSent(NIKKEI_1M)
NIKKEI_6M <- data.frame(Datum = datIndexAll, Index = as.vector(NIKKEI), Sent_Is = SentVar_I_6M[,"Nikkei_Is"], Sent_Ps <- SentVar_P_6M[,"Nikkei_Ps"])
plotIndexSent(NIKKEI_6M)




# Optimize Portfolios -----------------------------------------------------


# ** Formatting Data ------------------------------------------------------


# ** Sentex ---------------------------------------------------------------

### update them to just see the relevant data
SentVar_I_1M <- data.frame(SP500 = SentVar_I_1M$SP500_Is, DAX = SentVar_I_1M$Dax_Is,
                           ESX50 = SentVar_I_1M$ESX50_Is, NASDAQ = SentVar_I_1M$NASDAQ_Is,
                           NIKKEI = SentVar_I_1M$Nikkei_Is,
                           row.names = SentVar_I_1M$Datum)
SentVar_I_6M <- data.frame(SP500 = SentVar_I_6M$SP500_Is, DAX = SentVar_I_6M$Dax_Is,
                           ESX50 = SentVar_I_6M$ESX50_Is, NASDAQ = SentVar_I_6M$NASDAQ_Is,
                           NIKKEI = SentVar_I_6M$Nikkei_Is,
                           row.names = SentVar_I_6M$Datum)
SentVar_P_1M <- data.frame(SP500 = SentVar_P_1M$SP500_Ps, DAX = SentVar_P_1M$Dax_Ps,
                           ESX50 = SentVar_P_1M$ESX50_Ps, NASDAQ = SentVar_P_1M$NASDAQ_Ps,
                           NIKKEI = SentVar_P_1M$Nikkei_Ps,
                           row.names = SentVar_P_1M$Datum)
SentVar_P_6M <- data.frame(SP500 = SentVar_P_6M$SP500_Ps, DAX = SentVar_P_6M$Dax_Ps,
                           ESX50 = SentVar_P_6M$ESX50_Ps, NASDAQ = SentVar_P_6M$NASDAQ_Ps,
                           NIKKEI = SentVar_P_6M$Nikkei_Ps,
                           row.names = SentVar_P_6M$Datum)



SentDispI1 <- dispersionSent(SentVar_I_1M)
SentDispP1 <- dispersionSent(SentVar_P_1M)
SentDispI6 <- dispersionSent(SentVar_I_6M)
SentDispP6 <- dispersionSent(SentVar_P_6M)

SI1 <- cov(SentDispI1)
SI6 <- cov(SentDispI6)
SP1 <- cov(SentDispP1)
SP6 <- cov(SentDispP6)

# ** Index ----------------------------------------------------------------

PricesOUR <- data.frame(SP500 = as.vector(SP500), DAX = as.vector(DAX), ESX50 = as.vector(ESX50),
                        NASDAQ = as.vector(NASDAQ), NIKKEI = as.vector(NIKKEI),
                        row.names = names(SP500))

### discrete returns
ROUR <- PricesOUR[2:nrow(PricesOUR),]/PricesOUR[1:(nrow(PricesOUR)-1),] - 1

# TODO: Anpassen, dass manchmal Daten fehlen (nicht äquidistant) -> unterschiedliche Zeitspanne für Returns
### Versuch: mit timeSeries() und returns()
library(timeSeries)
testTimeSeries <- function(){
  PricesOURTime <- timeSeries(PricesOUR, charvec = row.names(PricesOUR))
  ROURTime <- returns(PricesOURTime, method = "discrete")
  return(sum(abs(timeSeries(ROUR) - ROURTime)>10^-15))
}
testTimeSeries()
### => schade, returns() passt nicht an

muOUR <- colMeans(ROUR)
SOUR <- cov(ROUR)


# ** Efficient Frontier ---------------------------------------------------



### nicht ganz so schick, aber unteres (sauberes) funktioniert iwie nicht
muStock <- muOUR
SStock <- SOUR


NAsset = length(muStock)
SSent <- SI1
nsga2(h, NAsset, 3,
      lower.bounds = rep(0, NAsset), upper.bounds = rep(1, NAsset), 
      constraints = g, cdim = 2, 
      popsize = 40)

### kann keine Argumente an Funktion weitergeben -> setze Argumente global
# NAsset = length(muOUR)
# nsga2(f, NAsset, 3,
#       w = w, muStock = muOUR, SStock = SOUR, targetRpm = targetRpm, targetVolpa = targetVolpa, 
#       SSent = SI1,
#       lower.bounds = rep(0, NAsset), upper.bounds = rep(1, NAsset), 
#       constraints = g, cdim = 2, 
#       popsize = 500)
# f(x = rep(2, 5), w = w, muStock = muOUR, SStock = SOUR, targetRpm = targetRpm, targetVolpa = targetVolpa, 
#   SSent = SI1)



# Optimal Portfolios ------------------------------------------------------



# Spielstube --------------------------------------------------------------


library(Rdonlp2) ## needed for donlp2NLP
library(fPortfolio)

### Find optimal portfolios (via different methods) with explicitly looking at last nDates 
### and with including information about just sLookBehind of last Sentiment
nDates <- 100
sLookBehind <- 50
datesOpt <- datIndexAll[(length(datIndexAll)-nDates+1):length(datIndexAll)]
datesOptStart <- min(datIndexAll)

### store weights for each Asset at each Date for each portfolio optimizing
WmcoI1 <- matrix(NA, nrow = nDates, ncol = NAsset)
WmcoI6 <- WmcoP1 <- WmcoP6 <- Wmsr <- Wmdp <- Wgmv <- Werc <- WmcoI1

### TODO: adjust weight for a good one
w = c(.1, .3, .6) 

ew <- rep(1 / NAsset, NAsset) # starte mit gleichverteilter Gewichtung
IneqA <- matrix(1, nrow = 1, ncol = NAsset)


for(i in 1:nDates){
  d <- datesOpt[i]
  
  rdat <- ROUR[row.names(ROUR)<=d,]
  muStock <- colMeans(rdat)
  SStock <- cov(rdat)
  
  ### TODO look sLookBehindDates in past for Sentiment to ignore sentiment long ago (maybe just weight actual sentiment higher)
  # [(datIndexAll<=d) * (datIndexAll>=datIndexAll[which(datIndexAll==d)-sLookBehind+1]),]
  
  SSentI1 <- cov(SentDispI1[datIndexAll<=d,])
  SSentI6 <- cov(SentDispI6[datIndexAll<=d,])
  SSentP1 <- cov(SentDispP1[datIndexAll<=d,])
  SSentP6 <- cov(SentDispP6[datIndexAll<=d,])
  
  ### donlp2NLP cannot handle parameter input for the function to minimize
  ### => use global parameters
  SSent <- SSentI1
  WmcoI1[i, ] <- donlp2NLP(start = ew, obj = hWeighted, 
                         par.lower = rep(0, NAsset), ineqA = IneqA, 
                         ineqA.lower = 1.0, ineqA.upper = 1.0)$solution
  SSent <- SSentI6
  WmcoI6[i, ] <- donlp2NLP(start = ew, obj = hWeighted, 
                           par.lower = rep(0, NAsset), ineqA = IneqA, 
                           ineqA.lower = 1.0, ineqA.upper = 1.0)$solution
  SSent <- SSentP1
  WmcoP1[i, ] <- donlp2NLP(start = ew, obj = hWeighted, 
                           par.lower = rep(0, NAsset), ineqA = IneqA, 
                           ineqA.lower = 1.0, ineqA.upper = 1.0)$solution
  SSent <- SSentP6
  WmcoP6[i, ] <- donlp2NLP(start = ew, obj = hWeighted, 
                           par.lower = rep(0, NAsset), ineqA = IneqA, 
                           ineqA.lower = 1.0, ineqA.upper = 1.0)$solution
  
  rdatTime <- timeSeries(ROUR, charvec = row.names(ROUR))
  ans <- tangencyPortfolio(rdatTime)
  Wmsr[i, ] <- getWeights(ans)
  
  ### most diversified
  ans <- PMD(rdatTime)
  Wmdp[i, ] <- FRAPO::Weights(ans) / 100
  
  ### global minimum variance
  ans <- PGMV(rdat)
  Wgmv[i, ] <- FRAPO::Weights(ans) / 100
  
  ### risk parity optimization
  ans <- rp(ew, SStock, ew, optctrl = ctrl(trace = FALSE))
  Werc[i, ] <- c(getx(ans))
}

W <- list("MCOI1" = WmcoI1, "MCOI6" = WmcoI6, "MCOP1" = WmcoP1, "MCOP6" = WmcoP6,  
          "MSR" = Wmsr, "MDP" = Wmdp,
          "GMV" = Wgmv, "ERC" = Werc)

### use lapply as W is a list
E <- lapply(W, function(x){
  wTs <- timeSeries(x, charvec = datesOpt)
  wTsL1 <- lag(wTs, 1)
  RetFac <- 1 + rowSums(timeSeries(ROUR[as.Date(row.names(ROUR))%in%datesOpt, ]) * wTsL1)
  RetFac[1] <- 100
  timeSeries(cumprod(RetFac), charvec = datesOpt)
})

cols <- topo.colors(8)
pdf(file = paste0(getwd(), "/Plot/", "optimal Portfolios", ".pdf"), 
    width = 12, height = 8)
plot(E[[1]], lwd = 2,
     ylab = "Index", xlab = "", col = cols[1],
     ylim = c(min(E[[1]], E[[2]], E[[3]], E[[4]], E[[5]], E[[6]], E[[7]], E[[8]]),
              max(E[[1]], E[[2]], E[[3]], E[[4]], E[[5]], E[[6]], E[[7]], E[[8]])),
     main = "Comparison of Allocation Strategies")
for(i in 2:8){
  lines(E[[i]], col = cols[i])
}
legend("topleft", legend = c("MCOI1", "MCOI6", "MCOP1", "MCOP6", "MSR", "MDP", "GMW", "ERC"), col = cols, lty = 1, lwd = 2)
abline(h = 100, col = "gray")
dev.off()