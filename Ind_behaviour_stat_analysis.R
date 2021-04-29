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

pairA <- "C"
pairB <- "B"
    
###############################################################################

# distance intervals
pair_distMin <- c(0, 50, 100, 200, 500, 1000, 2000, 3000, 5000)
pair_distMax <- c(50, 100, 200, 500, 1000, 2000, 3000, 5000, 10000) 

#for loop for the analysis
for (pair_dist in 1: length(pair_distMin)){
  
  distMin <- pair_distMin[pair_dist]
  distMax <- pair_distMax[pair_dist]
  
  ##individual A - classification (1: approach, 2: orthogonal, 3: retreat)
  list_breaks <- c(0,0,0)
  
  for (i in 1:length(diffA_abs)){
    #if pair distance in distance interval
    if (isTRUE(data_to_save$dist[i] < distMax & data_to_save$dist[i] >= distMin)){
      if (diffA_abs[i] <= 67.5 | diffA_abs[i] > 292.5){
        list_breaks[1] <- list_breaks[1] + 1
      }else{
        if (diffA_abs[i] <= 112.5 & diffA_abs[i] > 67.5){
          list_breaks[2] <- list_breaks[2] + 1
        }else{
          if (diffA_abs[i] <= 247.5 & diffA_abs[i] > 112.5){
            list_breaks[3] <- list_breaks[3] + 1
          }else{
            if (diffA_abs[i] <= 292.5 & diffA_abs[i] > 247.5){
              list_breaks[2] <- list_breaks[2] + 1
            }
          }
        }
      }
    }
  }
  
# consider behaviour 1 (approach) and 3 (retreat) 
total <- list_breaks[1] + list_breaks[3]
#calculate CI
CI_exact <- binom.confint(list_breaks[1], total, conf.level = 0.95, methods = "exact")

#print results with "checkmark" if statistically significant
if (round(CI_exact$lower,4) > 0.5 | round(CI_exact$upper,4) < 0.5){
  print(paste(pairA, " & [", distMin , "," , distMax, ")", " & ", total, " & ", list_breaks[1], " & ", round(CI_exact$lower,4), " & ", round(CI_exact$upper,4), " & checkmark", sep = ""))
}else{
  print(paste(pairA, " & [", distMin , "," , distMax, ")", " & ", total, " & ", list_breaks[1], " & ", round(CI_exact$lower,4), " & ", round(CI_exact$upper,4), " & ", sep = ""))
}
      
### individual B - classification (1: approach, 2: orthogonal, 3: retreat)
list_breaks <- c(0,0,0)

  for (i in 1:length(diffB_abs)){
    if (isTRUE(data_to_save$dist[i] < distMax & data_to_save$dist[i] >= distMin)){
      if (diffB_abs[i] <= 67.5 | diffB_abs[i] > 292.5){
        list_breaks[1] <- list_breaks[1] + 1
      }else{
        if (diffB_abs[i] <= 112.5 & diffB_abs[i] > 67.5){
          list_breaks[2] <- list_breaks[2] + 1
        }else{
          if (diffB_abs[i] <= 247.5 & diffB_abs[i] > 112.5){
            list_breaks[3] <- list_breaks[3] + 1
          }else{
           if (diffB_abs[i] <= 292.5 & diffB_abs[i] > 247.5){
            list_breaks[2] <- list_breaks[2] + 1
            }
          }
        }
      }
    }
  }

# consider behaviour 1 (approach) and 3 (retreat) 
total <- list_breaks[1] + list_breaks[3]

#calculate CI
CI_exact <- binom.confint(list_breaks[1], total, conf.level = 0.95, methods = "exact")

#print results with "checkmark" if statistically significant
if (round(CI_exact$lower,4) > 0.5 | round(CI_exact$upper,4) < 0.5){
  print(paste(pairB, " & ", " & ", total, " & ", list_breaks[1], " & ", round(CI_exact$lower,4), " & ", round(CI_exact$upper,4), " & checkmark", sep = ""))
}else{
  print(paste(pairB, " & ", " & ", total, " & ", list_breaks[1], " & ", round(CI_exact$lower,4), " & ", round(CI_exact$upper,4), " & ", sep = ""))
}
}


