## Specdata functions part 1

pollutantmean <- function(directory = "/Users/guldstjrna/datasciencecoursera/datasciencecoursera/specdata/", pollutant, id = 1:332){
  
  file_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  values <- numeric() #Empty vector to fill later
  
  for (i in id){
    data <- read.csv(file_list[i])
    values <- c(values, data[[pollutant]])  #Iteratively add to values vector
  }
  
 mean(values, na.rm = TRUE)
 
}

pollutantmean("specdata", "sulfate", 55:80)  ## Done!


## Specdata functions part 2

complete <- function(directory = "/Users/guldstjrna/datasciencecoursera/datasciencecoursera/specdata/", id = 1:332){
  
  file_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  nobs <- numeric()
  
  for (i in id){
    data <- read.csv(file_list[i])
    nobs <- c(nobs, sum(complete.cases(data)))
  }
  
  data.frame(id, nobs)
  
}

complete("specdata", 1:15)  ## Done!


## Specdata functions part 3

corr <- function(directory = "/Users/guldstjrna/datasciencecoursera/datasciencecoursera/specdata/", threshold = 0){
  
  file_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  corr_vect <- numeric() #Empty vector to fill later
  
  for (i in 1:332){
    
    data <- read.csv(file_list[i])
    values <- data[complete.cases(data),]
    
    if (nrow(values) > threshold) {
      corr_vect <- c(corr_vect, cor(values[,"sulfate"], values[, "nitrate"]))
    }
  }

  return(corr_vect)
  
}

