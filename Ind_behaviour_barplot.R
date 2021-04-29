require(plotrix)
library(binom)

# add appropriate directory
load(paste("../DataFrame_Pair.RData", sep = ""))

#consider the angle difference for individual A and B - for classification
diffA_abs <- vector()
for (i in 1:length(data_to_save$diffA)){
  diffA_abs[i] <- abs(data_to_save$diffA[i])
}

diffB_abs <- vector()
for (i in 1:length(data_to_save$diffB)){
  diffB_abs[i] <- abs(data_to_save$diffB[i])
}

# distance intervals
pair_distMin <- c(0, 50, 100, 200, 500, 1000, 2000, 3000, 5000)
pair_distMax <- c(50, 100, 200, 500, 1000, 2000, 3000, 5000, 10000) 

# lists used to keep track of counts
list_breaksA <- list()
for (k in 1:length(pair_distMin)){
  list_breaksA[[k]] <- c(0,0,0)
}

list_breaksB <- list()
for (k in 1:length(pair_distMin)){
  list_breaksB[[k]] <- c(0,0,0)
}

#for loop for the analysis
for (pair_dist in 1: length(pair_distMin)){
  
  distMin <- pair_distMin[pair_dist]
  distMax <- pair_distMax[pair_dist]
  
  ##individual A - classification (1: towards, 2: side, 3: away)
  for (i in 1:length(diffA_abs)){
    #if pair distance in distance interval
    if (data_to_save$dist[i] < distMax & data_to_save$dist[i] >= distMin){
      if (diffA_abs[i] <= 67.5 | diffA_abs[i] > 292.5){
        list_breaksA[[pair_dist]][1] <- list_breaksA[[pair_dist]][1] + 1
      }else{
        if (diffA_abs[i] <= 112.5 & diffA_abs[i] > 67.5){
          list_breaksA[[pair_dist]][2] <- list_breaksA[[pair_dist]][2] + 1
        }else{
          if (diffA_abs[i] <= 247.5 & diffA_abs[i] > 112.5){
            list_breaksA[[pair_dist]][3] <- list_breaksA[[pair_dist]][3] + 1
          }else{
            if (diffA_abs[i] <= 292.5 & diffA_abs[i] > 247.5){
              list_breaksA[[pair_dist]][2] <- list_breaksA[[pair_dist]][2] + 1
            }
          }
        }
      }
    }
  }

  
  ###individual B - classification (1: towards, 2: side, 3: away)
  
  for (i in 1:length(diffB_abs)){
    if (data_to_save$dist[i] < distMax & data_to_save$dist[i] >= distMin){
      if (diffB_abs[i] <= 67.5 | diffB_abs[i] > 292.5){
        list_breaksB[[pair_dist]][1] <- list_breaksB[[pair_dist]][1] + 1
      }else{
        if (diffB_abs[i] <= 112.5 & diffB_abs[i] > 67.5){
          list_breaksB[[pair_dist]][2] <- list_breaksB[[pair_dist]][2] + 1
        }else{
          if (diffB_abs[i] <= 247.5 & diffB_abs[i] > 112.5){
            list_breaksB[[pair_dist]][3] <- list_breaksB[[pair_dist]][3] + 1
          }else{
            if (diffB_abs[i] <= 292.5 & diffB_abs[i] > 247.5){
              list_breaksB[[pair_dist]][2] <- list_breaksB[[pair_dist]][2] + 1
            }
          }
        }
      }
    }
  }
}

#for labelling x-axis
inter <- c("0-50m", "50-100m", "100-200m", "200-500m", "500m-1km", "1-2km", "2-3km", "3-5km", "5-10km")

## barplot for individual A
v <- unlist(list_breaksA)
m <- matrix(v, ncol = 3, byrow = TRUE)
mt <- t(m)
v1 <- inter
#blue: "#648FFF"
#purple: "#785EF0"
#red "#DC267F"
c <- c("#648FFF", "#785EF0", "#DC267F")
barplot(mt, names.arg = v1, col = c, ylab = "Instances")

## barplot for individual B
v <- unlist(list_breaksB)
m <- matrix(v, ncol = 3, byrow = TRUE)
mt <- t(m)
v1 <- inter
c <- c("#648FFF", "#785EF0", "#DC267F")
barplot(mt, names.arg = v1, col = c, ylab = "Instances")

## CI plot for individual A
ratioA <- vector()
upper <- vector()
lower <- vector()
for (k in 1:length(list_breaksA)){
  sum <- list_breaksA[[k]][1] + list_breaksA[[k]][3]
  ratioA[k] <- (list_breaksA[[k]][1])/sum
  CI_exact <- binom.confint(list_breaksA[[k]][1], sum, conf.level = 0.95, methods = "exact")
  upper <- c(upper, CI_exact$upper)
  lower <- c(lower, CI_exact$lower)
}

plotCI(c(1:9), ratioA, ui=upper, li=lower, ylim = c(0,1), xaxt = "n", xlab = "Distance intervals", ylab = "CI")
axis(side = 1, at = c(1:9), labels = inter)
abline(h = 0.5, lty = 2)

#2,4,5,6,7: entries with statistically significant result (below 0.5)
for (i in c(2,4,5,6,7)){
points(i, ratioA[i], col = "#DC267F", pch = 19)
}

## CI plot for individual B
ratioB <- vector()
upper <- vector()
lower <- vector()
for (k in 1:length(list_breaksB)){
  sum <- list_breaksB[[k]][1] + list_breaksB[[k]][3]
  ratioB[k] <- (list_breaksB[[k]][1])/sum
  CI_exact <- binom.confint(list_breaksB[[k]][1], sum, conf.level = 0.95, methods = "exact")
  upper <- c(upper, CI_exact$upper)
  lower <- c(lower, CI_exact$lower)
}

plotCI(c(1:9), ratioB, ui=upper, li=lower, ylim = c(0,1), xaxt = "n", xlab = "Distance intervals", ylab = "CI")
axis(side = 1, at = c(1:9), labels = inter)
abline(h = 0.5, lty = 2)

#1,3,4,5,6,7,9: entries with statistically significant result (above 0.5)
for (i in c(1,3,4,5,6,7,9)){
points(i, ratioB[i], col = "#648FFF", pch = 19)
}



