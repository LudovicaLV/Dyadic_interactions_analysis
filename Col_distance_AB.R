#library used to calculate the different angles
library(geosphere)

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

#dataframe used to store corresponding colors
data <- data.frame(time = time, dist = dist)

#blue: "#648FFF"
#red "#DC267F"
#light orange: "#FFB000"
#dark orange: "#FE6100"
#gray: #B2B0B5

for (i in 1:length(data_to_save$dist)){
  if ((diffA_abs[i] <= 67.5 | diffA_abs[i] > 292.5) & (diffB_abs[i] <= 67.5 | diffB_abs[i] > 292.5)){
    data_to_save$colour[i]<- "#648FFF"
  }else{
    if (diffA_abs[i] <= 247.5 & diffA_abs[i] > 112.5 & diffB_abs[i] <= 247.5 & diffB_abs[i] > 112.5){
      data_to_save$colour[i]<- "#DC267F"
    }else{
      if ((diffA_abs[i] <= 67.5 | diffA_abs[i] > 292.5) & diffB_abs[i] <= 247.5 & diffB_abs[i] > 112.5){
        data_to_save$colour[i]<- "#FFB000"
      }else{
        if (diffA_abs[i] <= 247.5 & diffA_abs[i] > 112.5 & (diffB_abs[i] <= 67.5 | diffB_abs[i] > 292.5)){
          data_to_save$colour[i]<- "#FE6100"
        }else{
          data_to_save$colour[i]<- "#B2B0B5"
        }
      }
    }
  }
}

#plot of distance over time, colored with color corresponding to dyadic behaviour
plot(data_to_save$time, data_to_save$dist, col=data_to_save$colour, pch = 20, xlab = "Time units", ylab = "Distance units")

