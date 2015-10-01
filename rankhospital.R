rankhospital <- function(state,outcome,num) {
  x <- read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")
  st <- x[x$State == state,]
  if (nrow(st) == 0) stop("invalid state")
  header <- make.names(paste("Hospital.30.Day.Death..Mortality..Rates.from", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", outcome, perl=TRUE)))
  if (!(header %in% colnames(x))) stop("invalid outcome")
  col <- as.numeric(st[,header])
  o <- order(col,na.last=NA)
  if (num == "best") num <- 1
  if (num == "worst") num <- length(o)
  st[o[num],2]
}
