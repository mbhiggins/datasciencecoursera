pollutantmean <- function(directory,pollutant,id = 1:332)
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculat the 
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant accross all monitors list
  ## in the 'id' vector (ignoring NA values)

  zerosid <- formatC(id, width=3, flag="0")
  ##place 0's at begining for id value 1 -> 001
 
  filename = paste(directory, zerosid, sep = "/")
  filename1 = paste(filename, "csv", sep = ".")

  tabAll <- do.call("rbind",lapply(filename1,read.csv,header = TRUE))
  ## Create full table
  
  selectpollut <- tabAll[pollutant]
  ## collect values for pollutant in question
  
  polmean <- colMeans(selectpollut,na.rm=TRUE)
  ##capture mean of field
  
  return(polmean)
  ##return value
}