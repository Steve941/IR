install.packages("FRAPO","mco")
library(FRAPO)
library(mco)
## Loading of data
data(MultiAsset)
Prices <- timeSeries(MultiAsset, charvec = rownames(MultiAsset))
NAssets <- ncol(Prices)
R <- returns(Prices, method = "discrete", percentage = TRUE)
## Defining parameters
TargetRpa <- 6 ## percentage p.a.

TargetR <- 100 * ((1 + TargetRpa / 100)^(1 / 12) - 1)
TargetVol <- 4 ## percentage p.a.
l <- rep(1, 3) ## goal weighting, alle gleichgewichtet
WeightedSum <- FALSE
mu <- colMeans(R)
S <- cov(R)
  
#x sind die weights
f <- function(x){
  y <- numeric(3)
  y[1] <- -1.0 * l[1] * drop(crossprod(x, mu)) / TargetR
  y[2] <- l[2] * drop(sqrt(t(x) %*% S %*% x)) * sqrt(12) / TargetVol
   
  #die MRC-Funktion berechnet die Marginal contributions zum Portfolio Risk, x sind die weights
  y[3] <- l[3] * sum((mrc(x, S) / 100)^2)
   if(WeightedSum){
    return(sum(y))
     } else {
     return(y)
      }
  }
g <- function(x){
  c(1.01 - sum(x), sum(x) - 0.99)
}

ans <- nsga2(f, NAssets, 3, lower.bounds = rep(0, NAssets), upper.bounds = rep(1, NAssets), constraints = g, cdim = 2, popsize = 500)

mco <- data.frame(ans$value[ans$pareto.optimal, ])

mco[, 1] <- ((1 + (-1.0 * mco[, 1] * TargetR) / 100)^12- 1.0) * 100
colnames(mco) <- c("Return", "Risk", "Diversification")

install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(mco,
              main = "Pareto Efficient Solutions",
              sub = "Pareto Frontier (Surface)",
              xlab = "Return Objective",
              ylab = "Risk Objective",
              zlab = "Dispersion of MRC",
              angle = 15,
              highlight.3d = FALSE,
              box = TRUE,
              color = "steelblue",
              pch = 19, type = "p",
              cex.symbols = 0.6)



install.packages("akima")
library(akima)
install.packages("fields")
library(fields)

install.packages("fPortfolio")
library(fPortfolio)
install.packages("Rdonlp2", repos="http://R-Forge.R-project.org")
library(Rdonlp2)
install.packages("PerformanceAnalytics")
install.packages("ggtern")
library(ggtern)

s <- interp(mco[, 2], mco[, 1], mco[, 3],xo = seq(min(mco[, 2]), max(mco[, 2]), length = 100),
            yo = seq(min(mco[, 1]), max(mco[, 1]), length = 100),
            duplicate = "mean"
            )

par(mar = c(5, 6, 5, 6))

image.plot(s, nlevel = 50,
           main = "Image plot of efficient set",
           legend.lab = "Dispersion of MRC",
           xlab = "Risk Objective",
           ylab = "Return Objective",
           legend.mar = 4,
           horizontal = TRUE,
           legend.shrink = 0.7,
           col = topo.colors(50))

contour(s, add = TRUE, nlevels = 20, labcex = 0.8)


grid <- expand.grid(x = seq(0.05, 0.95, by = 0.05),
                    y = seq(0.05, 0.95, by = 0.05))
grid <- grid[which(rowSums(grid) <= 1.0), ]
wobj <- as.matrix(cbind(grid, 1 - rowSums(grid)),
                  nrow = nrow(grid), ncol = 3)
#Wobj ist eine Art Gitterverfahren und W ist die Lösung des Optimierers, wie viel jedes Asset zum Ziel beisteuert ??


W <- matrix(NA, nrow = nrow(wobj), ncol = NAssets)
WeightedSum <- TRUE
IneqA <- matrix(1, nrow = 1, ncol = NAssets)
ew <- rep(1 / NAssets, NAssets)
library(fPortfolio) ## for donlp2NLP
for(i in 1:nrow(wobj)){
  l <- c(wobj[i, ])
  W[i, ] <- donlp2NLP(start = ew, objective = f,
                      par.lower = rep(0, NAssets),
                      ineqA = IneqA, ineqA.lower = 1.0,
                      ineqA.upper = 1.0)$solution
}

#ES ist aus PerformanceAnalytics
library(PerformanceAnalytics)

Es95Mod <- apply(W, 1, function(x){
  r <- timeSeries(R %*% x / 100, time(R))
  -100 * ES(r)
  print(ES(r))
}) 

#Das print(ES(r)) habe ich hinzugefügt, da steckt schon etwas drin


terndat <- data.frame(cbind(wobj, Es95Mod))
colnames(terndat) <- c("x", "y", "z", "value")
#Theme for ternary plot
terntheme <- function(){
  list(theme_rgbg(), theme(legend.position = c(0, 1),legend.justification = c(0, 1), plot.margin=unit(c(0, 2,0, 2), "cm")))
  }

## ternary plot, Geht noch nicht

ggtern(terndat, aes(x = x, y = y, z = z, value = value)) +
  geom_interpolate_tern(aes(value = value, color = ..level..), binwidth = 1.0) +
  terntheme() +
  theme_hidegrid_minor() +
  theme_showgrid_major() +
  Lline(0.2, color = "blue", linetype = 2) + ## x
  Tline(0.3, colour = "red2", linetype = 2) + ## y
  Rline(0.5, color = "brown", linetype = 2) + ## z
  scale_color_gradient(low = "green", high = "red") +
  labs(x = "Return", y = "Risk", z = "MRC",
         title = "Ternary Plot with ES Contour Lines",
         color = "Level")



