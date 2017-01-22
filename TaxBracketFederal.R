 #### Calculate for Federal Tax
require(xlsx)
rate = read.xlsx("TaxBrackets.xlsx", sheetIndex = 2)
tax.fun.federal <- function(Marital.Status, Asking.Salary, Gross){
  if (Marital.Status == "Single")
    rate$Married<- -1
  if (Marital.Status == "Married")
    rate$Single <- -1
  rate$income <- 0
  for (i in 1:nrow(rate)){
    if (rate$Married[i] == -1) 
      rate$income[i] <- rate$Single[i]
    else if (rate$Single[i] == -1)
      rate$income[i] <- rate$Married[i]
  }
  rate.vec.fed <- as.data.frame(Gross)
  class(rate$income)
  class(rate.vec.fed$Gross)
  min(rate.vec.fed$Gross)
  #rate <- subset(rate, rate$income< max(rate.vec.fed.fed$Gross))
  rate.vec.fed$Federal.Tax <- c(1:nrow(rate.vec.fed))
  for (i in 1:nrow(rate.vec.fed)){
    if (max(rate$income) == 0)
      rate.vec.fed$Federal.Tax[i] <- rate$Rates[1]
    else if (rate.vec.fed$Gross[i]< rate$income[1])
      rate.vec.fed$Federal.Tax[i] <- rate$Rates[1]
    else if (rate.vec.fed$Gross[i]< rate$income[2])
      rate.vec.fed$Federal.Tax[i] <- rate$Rates[1]
    else if (rate.vec.fed$Gross[i]< rate$income[3])
      rate.vec.fed$Federal.Tax[i] <- rate$Rates[2]
    else if (rate.vec.fed$Gross[i]< rate$income[4])
      rate.vec.fed$Federal.Tax[i] <- rate$Rates[3]
    else if (rate.vec.fed$Gross[i]< rate$income[5])
      rate.vec.fed$Federal.Tax[i] <- rate$Rates[4]
    else if (rate.vec.fed$Gross[i]< rate$income[6])
      rate.vec.fed$Federal.Tax[i] <- rate$Rates[5]
    else if (rate.vec.fed$Gross[i]< rate$income[7])
      rate.vec.fed$Federal.Tax[i] <- rate$Rates[6]
    else if (rate.vec.fed$Gross[i]< rate$income[8])
      rate.vec.fed$Federal.Tax[i] <- rate$Rates[7]
    else if (rate.vec.fed$Gross[i]< rate$income[9])
      rate.vec.fed$Federal.Tax[i] <- rate$Rates[8]
    else if (rate.vec.fed$Gross[i]< rate$income[10])
      rate.vec.fed$Federal.Tax[i] <- rate$Rates[9]
    else rate.vec.fed$Federal.Tax[i] <- rate$Rates[10]
  }
print(as.vector(rate.vec.fed$Federal.Tax))
}


