best <- function(state,outcome) {
  x <- read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")
  st <- x[x$State == state,]
  if (nrow(st) == 0) stop("invalid state")
  header <- make.names(paste("Hospital.30.Day.Death..Mortality..Rates.from", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", outcome, perl=TRUE)))
  if (!(header %in% colnames(x))) stop("invalid outcome")
  col <- st[,header]
  st[which.min(as.numeric(col)),2]
}
