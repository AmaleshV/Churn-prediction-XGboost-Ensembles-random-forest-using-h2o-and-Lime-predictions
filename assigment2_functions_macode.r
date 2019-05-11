prop_na <- function(xv) {
    sum(is.na(xv))/length(xv)
}

io_scale <- function(var_not=c(), data) {
  ##Scale variables between 0 and 1. 
  ##Can't have missing values.
  ##Return the dataset with the variables marked as 
  ##no.dep scaled. 
  no.dep <- !colnames(data) %in% var_not
  bool <- any(!complete.cases(data[, no.dep]))
  if(bool) {
    warning('Incomplete cases')
  }
  data[, no.dep] <- apply(data[, no.dep], 2,
                               function(x) {
                                 (x - min(x))/(max(x)-min(x))
                               })
  return(data)
}

get_maxcors <- function(var_aimed, data, ncors=10, na.rm=TRUE) {
  ##Compute correlations of var_aimed with every other variables
  ##in data. Return the ncors largest correlations. 
  if(ncol(data[, -1]) < ncors) {
    ncors <- ncol(data[, -1])
  }
  if(na.rm) {
    data <- data[complete.cases(data), ]
  }
  
  Y <- data[[var_aimed]]
  Xcols <- data[, colnames(data) != var_aimed]
  corv <- sapply(Xcols, function(x) cor(Y, x))
  corv.names <- paste0(var_aimed, ':', colnames(Xcols))
  names(corv) <- corv.names
  corv <- corv[order(-corv)]
  corv <- corv[1:ncors]
  return(corv)
}

is_outlier <- function(x, thres.mult=2.5) {
  q1 <- quantile(x, 0.25, na.rm=TRUE)
  xmed <- quantile(x, 0.5, na.rm=TRUE)
  q3 <- quantile(x, 0.75, na.rm=TRUE)
  range <- q3 - q1
  finder <- sapply(x, function(i) {
    if(is.na(i)) {
      FALSE
    }
    else if(abs(i) > xmed + thres.mult * range) {
      TRUE
    }
    else {
      FALSE
    }
  })
  return(finder)
}
