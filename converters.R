upsetVenneuler <- function(vd){
  vd <- list(vd)
  intersections <- lapply(vd, function(x) strsplit(names(unlist(x)), "&"))
  intersections <- lapply(intersections[[1]], function(x) unlist(as.list(x)))
  sets <- unique(unlist(intersections))
  data <- na.omit(data.frame(matrix(NA, ncol = length(sets))))
  names(data) <- sets
  counts <- lapply(vd, function(x) unlist(x))
  names(counts[[1]]) <- NULL
  counts[[1]] <- na.omit(as.numeric(counts[[1]]))
  if(length(unlist(counts)) != length(intersections)){
    return(NULL)
  }
  
  for(i in seq(intersections)) {
    cols <- match(names(data), intersections[[i]])
    cols[!is.na(cols)] <- 1
    cols[is.na(cols)] <- 0
    cols <- rep(cols, times = counts[[1]][i])
    cols <- matrix(cols, ncol = length(sets), byrow = T)
    cols <- data.frame(cols)
    names(cols) <- sets
    data <- rbind(data, cols)
  }
  return(data)
}