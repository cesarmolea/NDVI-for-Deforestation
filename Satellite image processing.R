
# AUTHOR: Cesar Montiel Olea
# Date: December 2017
# Purpose: Image processing by hand for one year

library(magrittr)
library(dplyr)
library(plyr)

################################################
# STEP 0. We start by calling out the daily files of 2001,
#   and converting the negative values into zeros.
#   We also set the date and store our rasters into a list of matrices
################################################ 

setwd("C:/Users/cesarmo/Inter-American Development Bank Group/Montilla Linares, Abraham Rainiero - CESAR MONTIEL - AVH13C1_data_1999-2012_subset5000010/2001")      
rasters <- list.files(pattern = ".tif$") 
stack <- stack(rasters)

## Setting negative values in each raster to zeros
for (i in 1:length(rasters)){
  stack[[i]][stack[[i]] < 0] = 0
}

date <- c(seq(as.Date("2001/1/1"), by = "day", length.out = length(rasters)))

## Loop for all years by month
outlist <- list()

for (i in 1:length(rasters)){
  outlist [[i]] <- as.matrix(stack[[i]]) 
}


################################################
# STEP 1. First we loop to store the rows of each matrix
#   into new 421 matrices 
#   Now we have 421 matrices of dimension 365 X 461
################################################ 

### Create multiple matrices
y <- matrix(0, length(rasters), 421)
rep <- 461
matList <- lapply(1:ncol(y), function(i) replicate(rep, y[,i])) 
  
for (i in 1:length(rasters)) {
  for (j in 1:421) {
      matList[[j]][i, ] <- as.matrix(t(outlist[[i]][j, ]))
    }
  } 
  
##################################################
## STEP 2. We make a loop to add a date column ###
##################################################

  for (i in 1:length(matList)) { 
    matList[[i]] <- cbind.data.frame(matList[[i]], date)
    matList[[i]]$month <- as.factor(substr(matList[[i]][, 462], 6, 7))
  }
  
##################################################
## STEP 3. We use the group_by function to take the max per
#    month. Now, we are gonna have 421 matrices of dimension
#    12 X 461
##################################################

mat <- matrix(0, 12, 421)
rep2 <- 463
new.matList <- lapply(1:ncol(mat), function(i) replicate(rep2, mat[,i])) 
  
for (i in 1:421){
        new.matList[[i]] <- matList[[i]] %>% 
        group_by(month) %>% 
        summarise_all(max, na.rm = T)
    }
  
##################################################
## STEP 4. We regroup our files in an array 
##      with dimensions 421 X 463 (two more columns for date
##      and monnth) X 12
##################################################


## List to array
arr <- array(0, dim = c(421, 461, 12))

for (i in 1:12) {
  for(j in 1:421) { 
      arr[j,,i] <- as.matrix(t(new.matList[[j]][i, c(2:462)])) 
  }
}

##################################################
## STEP 5. We take the mean of the each element
##        (cell) of the array
##################################################
 
result2001 <- apply(arr, 1:2, mean, na.rm = T)
write.csv(result2001, file = "prueba.csv")

##################################################
## STEP 6. We convert back to a raster file
##################################################

rast.2001 <- raster(result2001, crs = CRS("+proj=longlat +ellps=clrk66 +no_defs")) # set the coordinate system
ext <- extent(c(-86, -63, -20, 1)) # set the coordinates
extent(rast.2001) <- ext
 
plot(rast.2001)  

##################################################
## STEP 7. We merge with the .shp from PERU
##################################################

peru <- readShapeSpatial("C:/Users/cesarmo/Inter-American Development Bank Group/Schling, Maja - Daily NDVI/Polygons/PER_adm1.shp") # read the polygons of Peru at departmental level
r_extract <- extract(rast.2001, peru, fun = mean, sp = TRUE, weights = TRUE, small = TRUE, na.rm = TRUE)


##################################################
## STEP 8. We make the values for the year
##################################################

table2001 <- r_extract@data[, c(5, 13)]
cols <- c("Department", "NDVI")
colnames(table2001) <- cols
write.csv(table2001, file = "table2001byhand.csv")



############################################3
## Using lists in liue of arrays
############################################
test <- list()
test <- names(stack)
test<- filter(list, substr(names(outlist)[1], 6, 7)
                
# code that works
    
# For one row this is the code
              
mat[1, ] <- as.matrix(t(outlist[[1]][1, ]))
mat2 <- matrix(0, length(rasters), 461)
                
for (i in 1:length(rasters)) {
      mat2[i, ] <- as.matrix(t(outlist[[i]][1, ])) 
              }
data <- cbind.data.frame(mat2, date)
data$month <- as.factor(substr(data[, 462], 6, 7))

# Take the maximim per month for the first row of each matrix
     month.max <- data %>% 
      group_by(month) %>% 
      summarise_all(max, na.rm = T)
                