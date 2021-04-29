##frequency
library(binom)

# add appropriate directory
load(paste("../DataFrame_Pair.RData", sep = ""))

#lower bound for speed proportion 
s_l <- 1/1.5
#upper bound for speed proportion 
s_u <- 1.5

# distance intervals
pair_distMin <- c(0, 50, 100, 200, 500, 1000, 2000, 3000, 5000)
pair_distMax <- c(50, 100, 200, 500, 1000, 2000, 3000, 5000, 10000) 

# list used to keep track of counts
list_breaks <- list()
for (k in 1:length(pair_distMin)){
  list_breaks[[k]] <- rep(0,8)
}

#consider the angle difference for individual A and B and their heading difference - for classification
diffA_abs <- vector()
for (i in 1:length(data_to_save$diffA)){
  diffA_abs[i] <- abs(data_to_save$diffA[i])
}

diffB_abs <- vector()
for (i in 1:length(data_to_save$diffB)){
  diffB_abs[i] <- abs(data_to_save$diffB[i])
}

diff_both_abs <- vector()
for (i in 1:length(data_to_save$diff_both)){
  diff_both_abs[i] <- abs(data_to_save$diff_both[i])
}

#for loop for the analysis
for (pair_dist in 1: length(pair_distMin)){
  
  distMin <- pair_distMin[pair_dist]
  distMax <- pair_distMax[pair_dist]
  
  for (i in 1:length(diff_both_abs)){
    #if pair distance in distance interval
    if (isTRUE(data_to_save$dist[i] < distMax & data_to_save$dist[i] >= distMin)){ 
      
      sP <- data_to_save$speedA[i]/data_to_save$speedB[i]
      
      #classification: 1 (behaviour a), 2 (behaviour b), 3 (behaviour c), 4 (behaviour d)
      # 5 (behaviour e), 6 (behaviour f), 7 (behaviour g), 8 (behaviour h)
      
      if (diff_both_abs[i] <= 10 | diff_both_abs[i] > 350){
        if ((diffA_abs[i] <= 67.5 | diffA_abs[i] > 292.5) & (diffB_abs[i] <= 247.5 & diffB_abs[i] > 112.5)){
          if (sP > s_u){
            list_breaks[[pair_dist]][1] <- list_breaks[[pair_dist]][1] + 1
          }else{
            if (sP > s_l & sP < s_u){
              list_breaks[[pair_dist]][2] <- list_breaks[[pair_dist]][2] + 1
            }else{
              if (sP < s_l){
                list_breaks[[pair_dist]][6] <- list_breaks[[pair_dist]][6] + 1
              }
            }
          }
        }else{
          if ((diffA_abs[i] <= 247.5 & diffA_abs[i] > 112.5) & (diffB_abs[i] <= 67.5 | diffB_abs[i] > 292.5)){
            if (sP < s_l){
              list_breaks[[pair_dist]][4] <- list_breaks[[pair_dist]][4] + 1
            }else{
              if (sP > s_l & sP < s_u){
                list_breaks[[pair_dist]][5] <- list_breaks[[pair_dist]][5] + 1
              }else{
                if (sP > s_u){
                  list_breaks[[pair_dist]][3] <- list_breaks[[pair_dist]][3] + 1
                }
              }
            }
          }else{
            if ((((diffA_abs[i] > 67.5)&(diffA_abs[i] <= 112.5))|((diffA_abs[i] > 247.5)&(diffA_abs[i] <= 292.5))) & (((diffB_abs[i] > 67.5)&(diffB_abs[i] < 112.5))|((diffB_abs[i] > 247.5)&(diffB_abs[i] < 292.5)))){     
              if (sP > s_l & sP < s_u){
                list_breaks[[pair_dist]][7] <- list_breaks[[pair_dist]][7] + 1
              }
            }
          }
        }
      }else{
        if (diff_both_abs[i] <= 190 & diff_both_abs[i] > 170){
          if ((diffA_abs[i] <= 67.5 | diffA_abs[i] > 292.5) & (diffB_abs[i] <= 67.5 | diffB_abs[i] > 292.5)){
            if (sP > s_l & sP < s_u){
              list_breaks[[pair_dist]][8] <- list_breaks[[pair_dist]][8] + 1
            }
          }
        }
      }
  }
  }
}

#for label x-axis
inter <- c("0-50m", "50-100m", "100-200m", "200-500m", "500m-1km", "1-2km", "2-3km", "3-5km", "5-10km")

#barplot for the analysis
v <- unlist(list_breaks)
m <- matrix(v, ncol = 8, byrow = TRUE)
mt <- t(m)
v1 <- inter
c <- c("deeppink3", "deeppink1", "pink", "royalblue1", "deepskyblue", "cadetblue1", "mediumpurple1", "dodgerblue4")
barplot(mt, names.arg = v1, col = c, ylab = "Instances")

#legend for barplot
c_rev <- rev(c)
letters <- c("a", "b", "c", "d", "e", "f", "g", "h")
lett_rev <- rev(letters)
legend(7.3, 380, legend=lett_rev, col=c_rev, fill=c_rev, text.font=4)

