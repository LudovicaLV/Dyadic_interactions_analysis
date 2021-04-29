library(binom)

# add appropriate directory
load(paste("../DataFrame_Pair.RData", sep = ""))

pairA <- "C"
pairB <- "B"

# list used to keep track of counts
list_breaks <- list()
for (i in 1:9){
  list_breaks[[i]] <- c(0,0,0,0,0)
}

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

#for loop for the analysis
for (pair_dist in 1: length(pair_distMin)){
  
  distMin <- pair_distMin[pair_dist]
  distMax <- pair_distMax[pair_dist]
  
  for (i in 1:length(diffB_abs)){
    #if pair distance in distance interval
    if (isTRUE(data_to_save$dist[i] < distMax & data_to_save$dist[i] >= distMin)){
      
      #classification: 1 (both towards), 2 (both away), 3 (A towards, B away)
      # 4 (A away, B towards), 5 (all the other possibilities)
      if ((diffA_abs[i] <= 67.5 | diffA_abs[i] > 292.5) & (diffB_abs[i] <= 67.5 | diffB_abs[i] > 292.5)){
        list_breaks[[pair_dist]][1] <- list_breaks[[pair_dist]][1] + 1
      }else{
        if (diffA_abs[i] <= 247.5 & diffA_abs[i] > 112.5 & diffB_abs[i] <= 247.5 & diffB_abs[i] > 112.5){
          list_breaks[[pair_dist]][2] <- list_breaks[[pair_dist]][2] + 1
        }else{
          if ((diffA_abs[i] <= 67.5 | diffA_abs[i] > 292.5) & diffB_abs[i] <= 247.5 & diffB_abs[i] > 112.5){
            list_breaks[[pair_dist]][3] <- list_breaks[[pair_dist]][3] + 1
          }else{
            if (diffA_abs[i] <= 247.5 & diffA_abs[i] > 112.5 & (diffB_abs[i] <= 67.5 | diffB_abs[i] > 292.5)){
              list_breaks[[pair_dist]][4] <- list_breaks[[pair_dist]][4] + 1
            }else{
              list_breaks[[pair_dist]][5] <- list_breaks[[pair_dist]][5] + 1
            }
          }
        }
      }
    }
  }
  
  ## we consider only behaviours 1-4 
  sum1 <- list_breaks[[pair_dist]][1]
  sum2 <- list_breaks[[pair_dist]][2]
  sum3 <- list_breaks[[pair_dist]][3]
  sum4 <- list_breaks[[pair_dist]][4]
  total <- list_breaks[[pair_dist]][1] + list_breaks[[pair_dist]][2] + list_breaks[[pair_dist]][3] + list_breaks[[pair_dist]][4]  
  
  final_sum <- c(sum1, sum2, sum3, sum4)
  
  ## statistical analysis
  for (chosen_sum in 1:4){
    sum <- final_sum[chosen_sum]
    
    #print results, with "checkmark" if statistically significant
    CI_exact <- binom.confint(sum, total, conf.level = 0.95, methods = "exact")
    if (chosen_sum == 1){
      if (round(CI_exact$lower,4) > 0.25 | round(CI_exact$upper,4) < 0.25){
        print(paste(pairA, " and ", pairB, "& [", distMin , "," , distMax, ")", " & ", total, " & ", sum, " & ", chosen_sum, " & ", round(CI_exact$lower,4), " & ", round(CI_exact$upper,4), " & checkmark", sep = ""))
      }else{
        print(paste(pairA, " and ", pairB, "& [", distMin , "," , distMax, ")", " & ", total, " & ", sum, " & ", chosen_sum, " & ", round(CI_exact$lower,4), " & ", round(CI_exact$upper,4), " &", sep = ""))
      }}else{
        if (round(CI_exact$lower,4) > 0.25 | round(CI_exact$upper,4) < 0.25){
          print(paste(pairA, " and ", pairB, "& ", " & ", total, " & ", sum, " & ", chosen_sum, " & ", round(CI_exact$lower,4), " & ", round(CI_exact$upper,4), " & checkmark", sep = ""))
        }else{
          print(paste(pairA, " and ", pairB, "& ", " & ", total, " & ", sum, " & ", chosen_sum, " & ", round(CI_exact$lower,4), " & ", round(CI_exact$upper,4), " &", sep = ""))
        }
      }
  }
}


#for label x-axis
inter <- c("0-50m", "50-100m", "100-200m", "200-500m", "500m-1km", "1-2km", "2-3km", "3-5km", "5-10km")

#barplot pair analysis
v <- unlist(list_breaks)
m <- matrix(v, ncol = 5, byrow = TRUE)
mt <- t(m)
#c <- c("blue", "red","chartreuse2", "chartreuse4", "gray")
c <- c("#648FFF", "#DC267F", "#FFB000", "#FE6100", "#B2B0B5")
barplot(mt, names.arg = inter, col = c, ylab = "Instances")

