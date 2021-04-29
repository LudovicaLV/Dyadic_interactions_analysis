#Note: the first part of this code is identical to the code in
#AB_individual.R, since the calculation of distance and angles is the same.
#The classification changes, and that is described in the second part of the code.

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

diffN_abs <- vector()
for (i in 1:length(diffB)){
  diffN_abs[i] <- abs(diffB[i])
}

###############################################################################
# dyadic analysis

#distance intervals
pair_distMin <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)
pair_distMax <- c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) 

#list to record the counts 
list_breaks <- list()
for (i in 1:16){
  list_breaks[[i]] <- c(0,0,0,0,0)
}

#for loop for the analysis
for (pair_dist in 1: length(pair_distMin)){
  
  distMin <- pair_distMin[pair_dist]
  distMax <- pair_distMax[pair_dist]
  
  for (i in 1:length(diffN_abs)){
    if (isTRUE(dist[i] < distMax & dist[i] >= distMin)){
      if ((diffA_abs[i] <= 67.5 | diffA_abs[i] > 292.5) & (diffN_abs[i] <= 67.5 | diffN_abs[i] > 292.5)){
        list_breaks[[pair_dist]][1] <- list_breaks[[pair_dist]][1] + 1
      }else{
        if (diffA_abs[i] <= 247.5 & diffA_abs[i] > 112.5 & diffN_abs[i] <= 247.5 & diffN_abs[i] > 112.5){
          list_breaks[[pair_dist]][2] <- list_breaks[[pair_dist]][2] + 1
        }else{
          if ((diffA_abs[i] <= 67.5 | diffA_abs[i] > 292.5) & diffN_abs[i] <= 247.5 & diffN_abs[i] > 112.5){
            list_breaks[[pair_dist]][3] <- list_breaks[[pair_dist]][3] + 1
          }else{
            if (diffA_abs[i] <= 247.5 & diffA_abs[i] > 112.5 & (diffN_abs[i] <= 67.5 | diffN_abs[i] > 292.5)){
              list_breaks[[pair_dist]][4] <- list_breaks[[pair_dist]][4] + 1
            }else{
              list_breaks[[pair_dist]][5] <- list_breaks[[pair_dist]][5] + 1
            }
          }
        }
      }
    }
  }
  
#statistical analysis (with a checkmark if the result is statistically significant)
  sum1 <- list_breaks[[pair_dist]][1]
  sum2 <- list_breaks[[pair_dist]][2]
  sum3 <- list_breaks[[pair_dist]][3]
  sum4 <- list_breaks[[pair_dist]][4]
  total <- list_breaks[[pair_dist]][1] + list_breaks[[pair_dist]][2] + list_breaks[[pair_dist]][3] + list_breaks[[pair_dist]][4]  

  final_sum <- c(sum1, sum2, sum3, sum4)
  
  for (chosen_sum in 1:4){
    sum <- final_sum[chosen_sum]
    CI_exact <- binom.confint(sum, total, conf.level = 0.95, methods = "exact")
      if (round(CI_exact$lower,4) > 0.25 | round(CI_exact$upper,4) < 0.25){
        print(paste(pairA, " and ", pairB, "- [", distMin , "," , distMax, ")", " - ", total, " - ", sum, " - ", chosen_sum, " - ", round(CI_exact$lower,4), " - ", round(CI_exact$upper,4), " - checkmark", sep = ""))
      }else{
        print(paste(pairA, " and ", pairB, "- [", distMin , "," , distMax, ")", " - ", total, " - ", sum, " - ", chosen_sum, " - ", round(CI_exact$lower,4), " - ", round(CI_exact$upper,4), " -", sep = ""))
      }
    
  }
}

#barplot for dyadic analysis
v1 <- seq(5, 80, 5)
v <- unlist(list_breaks)
m <- matrix(v, ncol = 5, byrow = TRUE)
mt <- t(m)
#blue: "#648FFF"
#red "#DC267F"
#light orange: "#FFB000"
#dark orange: "#FE6100"
#gray: #B2B0B5
c <- c("#648FFF", "#DC267F", "#FFB000", "#FE6100", "#B2B0B5")
barplot(mt, names.arg = v1, col = c, ylab = "Instances")
