rankall <- function(outcome,num = "best") {
  x <- read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")
  header <- make.names(paste("Hospital.30.Day.Death..Mortality..Rates.from", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", outcome, perl=TRUE)))
  if (!(header %in% colnames(x))) stop("invalid outcome")
  states = sort(unique(x$State))
  if (num == "best") num <- 1
  hospitals <- rep(NA,length(states))
  i <- 1
  for (state in states) {
    st <- x[x$State == state,]
    col <- as.numeric(st[,header])
    o <- order(col,st$Hospital.Name,na.last=NA)
    if (num == "worst") num1 <- length(o)
    else                num1 <- num
    hospitals[i] <- st[o[num1],2]
    i <-i+1
  }
  data.frame(hospital=hospitals,state=states)
}
