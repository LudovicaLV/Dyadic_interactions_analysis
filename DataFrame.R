library(geosphere)

#interval in seconds (20 min)
inter <- 1200

#load data for male and female
load("Male.RData")
load("Female.RData")

#transform timpestamp as a numeric object for both trajectories
my_dataA$dateNumber <- as.numeric(my_dataA$timestamp)
my_dataB$dateNumber <- as.numeric(my_dataB$timestamp) 

#intersection of time points in trajectories of individual A and B 
dates <- intersect(my_dataA$dateNumber, my_dataB$dateNumber)
ld <- length(dates)
print(ld)

#define parameters and vectors for the for loop

#beginning and end of the for loop
beg <- 1
end <- ld - 1

#vectors to store positions,time,distance,headings,directions towards the other,
#angle differences and heading difference

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

diffA <- vector()
diffB <- vector()
diff_both <- vector()

#for loop for the analysis
for (i in beg:end){
    print(i)
    j <- i + 1
    #if the time interval between consecutive points is 20 minutes
    if (isTRUE(dates[j] - dates[i] == inter)){
      row_A <- which(grepl(dates[i], my_dataA$dateNumber))
      row_B <- which(grepl(dates[i], my_dataB$dateNumber))
      
      lon_A <- my_dataA$longitude[row_A][1]
      lat_A <- my_dataA$latitude[row_A][1]
      
      lon_B <- my_dataB$longitude[row_B][1]
      lat_B <- my_dataB$latitude[row_B][1]
      
      row_A1 <- which(grepl(dates[j], my_dataA$dateNumber))
      row_B1 <- which(grepl(dates[j], my_dataB$dateNumber))
      
      lon_A1 <- my_dataA$longitude[row_A1][1]
      lat_A1 <- my_dataA$latitude[row_A1][1]
      
      lon_B1 <- my_dataB$longitude[row_B1][1]
      lat_B1 <- my_dataB$latitude[row_B1][1]
      
      if ( identical(lon_A, numeric(0)) |identical(lat_A, numeric(0)) |
           identical(lon_B, numeric(0)) | identical(lat_B, numeric(0)) |
           identical(lon_A1, numeric(0)) | identical(lat_A1, numeric(0)) |
           identical(lon_B1, numeric(0)) | identical(lat_B1, numeric(0)) ){}else{
             if(is.na(lon_A) | is.na(lat_A) |  is.na(lon_B) | is.na(lat_B) |
                is.na(lon_A1) | is.na(lat_A1) |  is.na(lon_B1) | is.na(lat_B1)){}else{
                  #store all values
                  xA <- c(xA, lon_A)
                  yA <- c(yA, lat_A)
                  xB <- c(xB, lon_B)
                  yB <- c(yB, lat_B)
                  time <- c(time, dates[i])
                  di1 <- distm(c(lon_A, lat_A), c(lon_B, lat_B), fun = distHaversine)
                  dist <- c(dist, di1)
                  
                  b1 <- bearing(c(lon_A,lat_A), c(lon_A1,lat_A1))
                  b2 <- bearing(c(lon_B,lat_B), c(lon_B1,lat_B1))
                  b3 <- bearing(c(lon_A,lat_A), c(lon_B,lat_B))
                  b4 <- bearing(c(lon_B,lat_B), c(lon_A,lat_A))
                  
                  #to have bearings in the interval [0,360)
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
                  
                  a <- b1 - b3
                  b <- b2 - b4
                  c <- b1 - b2
                  
                  diffA <- c(diffA, a)
                  diffB <- c(diffB, b)
                  diff_both <- c(diff_both,c)
                  
                }}
    }
  }
  
#dataframe with all the data
data_to_save <- data.frame(dist = dist, time = time, xA = xA, yA = yA, xB = xB, yB = yB, headA = head_A, headB = head_B, headAB = head_AB, headBA = head_BA, diffA = diffA, diffB = diffB, diff_both = diff_both)
#save dataframe to a given directory (insert appropriate)
save(data_to_save, file=paste(".../DataFrame_Pair.RData", sep = ""))
