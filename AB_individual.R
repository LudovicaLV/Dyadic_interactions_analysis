#library used to calculate the different angles
library(geosphere)
#library used to calculate the CI
library(binom)

#distance-dependent model (add appropriate directory)
#dfA <- read.csv(paste(".../ModelDistance_A.csv", sep = ""), header=TRUE)  
#dfB <- read.csv(paste(".../ModelDistance_B.csv", sep = ""), header=TRUE)

#time-dependent model (add appropriate directory)
#dfA <- read.csv(paste(".../ModelTime_A.csv", sep = ""), header=TRUE) 
#dfB <- read.csv(paste(".../ModelTime_B.csv", sep = ""), header=TRUE)

#visualize the outputs
plot(dfA$xA, dfA$yA, type = "l", xlim = c(-400,400), ylim = c(-400,400), xlab = "x", ylab = "y")
points(dfB$xB, dfB$yB, type = "l", col = "red")

#initialise parameters and vectors
beg <- 1
end <- nrow(dfA) - 1
dist <- vector()
date_vector <- vector()
xA <- vector()
yA <- vector()
xB <- vector()
yB <- vector()
time <- vector()
dist <- vector()
head_A <- vector()
head_B <- vector()
head_AB <- vector()
head_BA <- vector()

#for loop to calculate the pair distance
#we used the Euclidean distance for the NMB models
for (i in beg:end){
  print(i)
  j <- i + 1
  div <- 1
  lon_A <- dfA$xA[i]/div
  lat_A <- dfA$yA[i]/div
  
  lon_B <- dfB$xB[i]/div
  lat_B <- dfB$yB[i]/div
  
  time <- c(time, i)
  di1 <- sqrt(((lon_A-lon_B)^2) + ((lat_A - lat_B)^2))
  dist <- c(dist, di1)
  
}

#loop to calculate the angles of interest
#we scale the values to be able to use the function bearing
for (i in beg:end){
  print(i)
  j <- i + 1
  div <- 10000
  lon_A <- dfA$xA[i]/div
  lat_A <- dfA$yA[i]/div
  
  lon_B <- dfB$xB[i]/div
  lat_B <- dfB$yB[i]/div
  
  lon_A1 <- dfA$xA[j]/div
  lat_A1 <- dfA$yA[j]/div
  
  lon_B1 <- dfB$xB[j]/div
  lat_B1 <- dfB$yB[j]/div
  
  b1 <- bearing(c(lon_A,lat_A), c(lon_A1,lat_A1))
  b2 <- bearing(c(lon_B,lat_B), c(lon_B1,lat_B1))
  b3 <- bearing(c(lon_A,lat_A), c(lon_B,lat_B))
  b4 <- bearing(c(lon_B,lat_B), c(lon_A,lat_A))
  
  if (b1 < 0){
    b1 <- 360 + b1
  }
  
  if (b2 < 0){
    b2 <- 360 + b2
  }
  
  if (b3 < 0){
    b3 <- 360 + b3
  }
  
  if (b4 < 0){
    b4 <- 360 + b4
  }
  
  head_A <- c(head_A, b1)
  head_B <- c(head_B, b2)
  head_AB <- c(head_AB, b3)
  head_BA <- c(head_BA, b4)
  
}

#calculate the angle differences
diffA <- vector()
diffB <- vector()
diff_both <- vector()

for (i in 1:length(head_A)){
  a <- head_A[i] - head_AB[i]
  b <- head_B[i] - head_BA[i]
  c <- head_A[i] - head_B[i]
  
  diffA <- c(diffA, a)
  diffB <- c(diffB, b)
  diff_both <- c(diff_both,c)
}

#and the absolute values
diffA_abs <- vector()
for (i in 1:length(diffA)){
  diffA_abs[i] <- abs(diffA[i])
}

diffB_abs <- vector()
for (i in 1:length(diffB)){
  diffB_abs[i] <- abs(diffB[i])
}

###############################################################################
# individual analysis

#distance bins: every 5 units, up to 80
br <- c(5)
tot <- c(80)

#names for plots and statistical analysis output
pairA <- "A"
pairB <- "B"

#for loop: analysis
for(r in 1:length(br)){
  
  ##pairA - list to record the counts
  list_breaks <- list()
  
  for (k in 1:16){
    list_breaks[[k]] <- c(0,0,0)
  }
  
  breakValue <- br[r]
  
  for (i in 1:length(diffA_abs)){
    if (dist[i] < tot[r]){
      j <- dist[i]%/%breakValue + 1
      if (diffA_abs[i] <= 67.5 | diffA_abs[i] > 292.5){
        list_breaks[[j]][1] <- list_breaks[[j]][1] + 1
      }else{
        if (diffA_abs[i] <= 112.5 & diffA_abs[i] > 67.5){
          list_breaks[[j]][2] <- list_breaks[[j]][2] + 1
        }else{
          if (diffA_abs[i] <= 247.5 & diffA_abs[i] > 112.5){
            list_breaks[[j]][3] <- list_breaks[[j]][3] + 1
          }else{
            if (diffA_abs[i] <= 292.5 & diffA_abs[i] > 247.5){
              list_breaks[[j]][2] <- list_breaks[[j]][2] + 1
            }
          }
        }
      }
    }
  }
  
  #barplot
  v <- unlist(list_breaks)
  m <- matrix(v, ncol = 3, byrow = TRUE)
  mt <- t(m)
  v1 <- seq(br[r], tot[r], by=br[r])
  #blue: "#648FFF"
  #purple: "#785EF0"
  #red "#DC267F"
  c <- c("#648FFF", "#785EF0", "#DC267F")
  barplot(mt, main = paste("Difference (heading ", pairA, ", direction ", pairA, "->", pairB , ") -", tot[r], "units",  sep = ""), names.arg = v1,
          col = c, xlab = paste("Interval in u [x-", br[r], "u,x)", sep = ""), ylab = "Instances")
  
  
  # plot ratio approach/(approach+retreat)
  ratio <- vector()
  for (k in 1:length(list_breaks)){
    sum <- list_breaks[[k]][1] + list_breaks[[k]][3]
    ratio[k] <- (list_breaks[[k]][1])/sum
  }
  
  plot(ratio, main = paste("Ratio approach/(approach+retreat) areas for ", pairA, " - ", tot[r], "units", sep = ""), xlab = paste(br[r], "u interval", sep = ""), ylim = c(0,1), xaxt = "n")
  axis(side = 1, at = c(1:16), labels = v1)
  abline(h = 0.5, lty = 2)
  
  # statistical analysis
  sum <- 0
  total <- 0
  check <- 0
  for (k in 1:length(list_breaks)){
    sum <- sum + list_breaks[[k]][1]
    total <- total + list_breaks[[k]][1] + list_breaks[[k]][3]
    check <- check + list_breaks[[k]][1] + list_breaks[[k]][3]  + list_breaks[[k]][2]
  }
  
  CI_exact <- binom.confint(sum, total, conf.level = 0.95, methods = "exact")
  print(paste(pairA, " - ", tot[r], "m", " - ", total, " - ", sum, " - ", round(CI_exact$lower,4), " - ", round(CI_exact$upper,4)))
  
  #for extra ratio plot
  ratioA <- vector()
  upperA <- vector()
  lowerA <- vector()
  for (k in 1:length(list_breaks)){
    sum <- list_breaks[[k]][1] + list_breaks[[k]][3]
    ratioA[k] <- (list_breaks[[k]][1])/sum
    CI_exact <- binom.confint(list_breaks[[k]][1], sum, conf.level = 0.95, methods = "exact")
    upperA <- c(upperA, CI_exact$upper)
    lowerA <- c(lowerA, CI_exact$lower)
  }
  
  
  ##pairB - list to record the counts
  list_breaks <- list()
  
  for (k in 1:16){
    list_breaks[[k]] <- c(0,0,0)
  }
  
  breakValue <- br[r]
  
  for (i in 1:length(diffB_abs)){
    if (dist[i] < tot[r]){
      j <- dist[i]%/%breakValue + 1
      if (diffB_abs[i] <= 67.5 | diffB_abs[i] > 292.5){
        list_breaks[[j]][1] <- list_breaks[[j]][1] + 1
      }else{
        if (diffB_abs[i] <= 112.5 & diffB_abs[i] > 67.5){
          list_breaks[[j]][2] <- list_breaks[[j]][2] + 1
        }else{
          if (diffB_abs[i] <= 247.5 & diffB_abs[i] > 112.5){
            list_breaks[[j]][3] <- list_breaks[[j]][3] + 1
          }else{
            if (diffB_abs[i] <= 292.5 & diffB_abs[i] > 247.5){
              list_breaks[[j]][2] <- list_breaks[[j]][2] + 1
            }
          }
        }
      }
    }
  }
  
  #barplot
  v <- unlist(list_breaks)
  m <- matrix(v, ncol = 3, byrow = TRUE)
  mt <- t(m)
  v1 <- seq(br[r], tot[r], by=br[r])
  c <- c("#648FFF", "#785EF0", "#DC267F")
  barplot(mt, main = paste("Difference (heading ", pairB, ", direction ", pairB, "->", pairA , ")", " - ", tot[r], "units",  sep = ""), names.arg = v1,
          col = c, xlab = paste("Interval in u [x-", br[r], "u,x)", sep = ""), ylab = "Instances")
  
  # plot ratio approach/(approach+retreat)
  ratio <- vector()
  for (k in 1:length(list_breaks)){
    sum <- list_breaks[[k]][1] + list_breaks[[k]][3]
    ratio[k] <- (list_breaks[[k]][1])/sum
  }
  
  plot(ratio, main = paste("Ratio approach/(approach+retreat) areas for ", pairB, " - ", tot[r], "units", sep = ""), xlab = paste(br[r], "u interval", sep = ""), ylim = c(0,1), xaxt = "n")
  axis(side = 1, at = c(1:16), labels = v1)
  abline(h = 0.5, lty = 2)
  
  #statistical analysis
  sum <- 0
  total <- 0
  check <- 0
  for (k in 1:length(list_breaks)){
    sum <- sum + list_breaks[[k]][1]
    total <- total + list_breaks[[k]][1] + list_breaks[[k]][3]
    check <- check + list_breaks[[k]][1] + list_breaks[[k]][3]  + list_breaks[[k]][2]
  }
  
  CI_exact <- binom.confint(sum, total, conf.level = 0.95, methods = "exact")
  print(paste(pairB, " - ", tot[r], "m", " - ", total, " - ", sum, " - ", round(CI_exact$lower,4), " - ", round(CI_exact$upper,4)))
  
  #for extra ratio plot
  ratioB <- vector()
  upperB <- vector()
  lowerB <- vector()
  for (k in 1:length(list_breaks)){
    sum <- list_breaks[[k]][1] + list_breaks[[k]][3]
    ratioB[k] <- (list_breaks[[k]][1])/sum
    CI_exact <- binom.confint(list_breaks[[k]][1], sum, conf.level = 0.95, methods = "exact")
    upperB <- c(upperB, CI_exact$upper)
    lowerB <- c(lowerB, CI_exact$lower)
  }
}

#ratio plot with CI - A
require(plotrix)
plotCI(c(1:16), ratioA, ui=upperA, li=lowerA, ylim = c(0,1), xaxt = "n", xlab = "Distance intervals", ylab = "CI")
abline(h = 0.5, lty = 2)

i <- c(2:6)
points(i, ratioA[i], col = "#DC267F", pch = 19)
i <- c(7:12)
points(i, ratioA[i], col = "#648FFF", pch = 19)

#ratio plot with CI - B
require(plotrix)
plotCI(c(1:16), ratioB, ui=upperB, li=lowerB, ylim = c(0,1), xaxt = "n", xlab = "Distance intervals", ylab = "CI")
abline(h = 0.5, lty = 2)

i <- c(2:6)
points(i, ratioB[i], col = "#DC267F", pch = 19)
i <- c(7:12)
points(i, ratioB[i], col = "#648FFF", pch = 19)

