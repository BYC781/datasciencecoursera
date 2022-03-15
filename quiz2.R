pollutantmean <- function(directory, pollutant, id = 1:332){
  j <- 0
  for (i in id){
    j <- j+1
    if (i<10){
      id[j] <- paste0('00', i)
    }else if (i>=10 & i <100){
      id[j] <- paste0('0',i)
    }
  } # 在數字前面補0
  
  dir = paste('/Users/bychen/Documents/GitHub/datasciencecoursera/', directory, sep='')
  setwd(dir)
  filenames <- paste(id, '.csv', sep='')

  j <- 0
  for (i in filenames){
    j <- j+1
    fil <- read.csv(i)
    pol[j] <- sum(fil[pollutant][!is.na(fil[pollutant])])
    len[j] <- length(fil[pollutant][!is.na(fil[pollutant])])
  }
  sum(pol)/sum(len)
}
complete <- function(directory, id = 1:332){
  j <- 0
  num <- 0
  for (i in id){
    j <- j+1
    if (i<10){
      num[j] <- paste0('00', i)
    }else if (i>=10 & i <100){
      num[j] <- paste0('0',i)
    }else {
      num[j] <- i
    }
  } # 在數字前面補0
  
  dir = paste('/Users/bychen/Documents/GitHub/datasciencecoursera/', directory, sep='')
  setwd(dir)
  filenames <- paste(num, '.csv', sep='')
  
  j <- 0
  n <- 0
  for (i in filenames){
    j <- j+1
    fil <- read.csv(i)
    comp_rows <- fil[complete.cases(fil),]
    n[j] <- nrow(comp_rows)
  }
  df <- data.frame("id" = id,
                  "nobs" = n)
  df
}
corr <- function(directory, threshold = 0){
  dir = paste('/Users/bychen/Documents/GitHub/datasciencecoursera/', directory, sep='')
  setwd(dir)
  
  
  id_seq <- 1:332
  j <- 0
  num <- 0
  for (i in id_seq){
    j <- j+1
    if (i<10){
      num[j] <- paste0('00', i)
    }else if (i>=10 & i <100){
      num[j] <- paste0('0',i)
    }else{
      num[j] <- i
    }
  } # 在數字前面補0
  filenames <- paste(num, '.csv', sep='')
  
  j <- 0
  correlation <- vector()
  for (i in filenames){
    j <- j+1
    fil <- read.csv(i)
    comp_rows <- fil[complete.cases(fil),]
    if (nrow(comp_rows) >= threshold){
      monitor_c <- cor(comp_rows$sulfate, comp_rows$nitrate)
      correlation <- append(correlation, monitor_c)
    }
  }
  correlation
}