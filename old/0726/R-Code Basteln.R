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



# ** Index ----------------------------------------------------------------

PricesOUR <- data.frame(SP500 = as.vector(SP500), DAX = as.vector(DAX), ESX50 = as.vector(ESX50),
                        NASDAQ = as.vector(NASDAQ), NIKKEI = as.vector(NIKKEI),
                        row.names = names(SP500))

### discrete returns
ROUR <- PricesOUR[2:nrow(PricesMat),]/PricesMat[1:(nrow(PricesMat)-1),] - 1

# TODO: Anpassen, dass manchmal Daten fehlen (nicht äquidistant) -> unterschiedliche Zeitspanne für Returns
### Versuch: mit timeSeries() und returns()
testTimeSeries <- function(){
  PricesOURTime <- timeSeries(PricesOUR, charvec = row.names(PricesOUR))
  ROURTime <- returns(PricesOURTime, method = "discrete")
  return(sum(abs(timeSeries(ROUR) - ROURTime)>10^-15))
}
testTimeSeries()
### => schade, returns() passt nicht an

muOUR <- colMeans(ROUR)
SOUR <- cov(ROUR)
