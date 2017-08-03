# Files
### Data
folderData <- file.path(getwd(), "..", "Data")
### Plots
folderPlotIndexSent <- file.path(getwd(), "Plot_Index_Sent")


# Return
targetRpa <- 0.06 ## targeted return of 6 % p.a.
targetRpm <- ((1 + targetRpa / 100)^(1 / 12) - 1) # % per month

# Volatility
targetVolpa <- 0.04 ## % p.a.

# Further parameters
w <- rep(1, 3) ## goal weighting

# Dates
dateMin <- as.Date("23.02.2001", format = "%d.%m.%Y")
dateMax <- as.Date("02.09.2016", format = "%d.%m.%Y")

# Assets
numAsset <- 8

