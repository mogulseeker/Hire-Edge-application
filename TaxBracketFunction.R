rm(list=ls())
#Read in Data
setwd("/Users/nathananderson/Documents/R Files/Salary Script")
require(xlsx)
bracket = read.xlsx("TaxBrackets.xlsx", sheetIndex = 1)
tax.fun.state <- function(State, Marital.Status, Asking.Salary, Gross, Simulations){
rate <- subset(bracket, bracket$TaxState == State)
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

#rate <- rate[,c(2,5)]
rate.vec <- as.data.frame(Gross)
class(rate$income)
class(rate.vec$Gross)
min(rate.vec$Gross)
#rate <- subset(rate, rate$income< max(rate.vec$Gross))
rate.vec$state.Tax <- c(1:nrow(rate.vec))
for (i in 1:nrow(rate.vec)){
  if (max(rate$income) == 0)
    rate.vec$state.Tax[i] <- rate$Rates[1]
  else if (rate.vec$Gross[i] > max(rate$Rates))
    rate.vec$state.Tax[i] <- max(rate$Rates)
  else if (rate.vec$Gross[i]< rate$income[1])
    rate.vec$state.Tax[i] <- rate$Rates[1]
  else if (rate.vec$Gross[i]< rate$income[2])
    rate.vec$state.Tax[i] <- rate$Rates[1]
  else if (rate.vec$Gross[i]< rate$income[3])
    rate.vec$state.Tax[i] <- rate$Rates[2]
  else if (rate.vec$Gross[i]< rate$income[4])
    rate.vec$state.Tax[i] <- rate$Rates[3]
  else if (rate.vec$Gross[i]< rate$income[5])
    rate.vec$state.Tax[i] <- rate$Rates[4]
  else if (rate.vec$Gross[i]< rate$income[6])
    rate.vec$state.Tax[i] <- rate$Rates[5]
  else if (rate.vec$Gross[i]< rate$income[7])
    rate.vec$state.Tax[i] <- rate$Rates[6]
  else if (rate.vec$Gross[i]< rate$income[8])
    rate.vec$state.Tax[i] <- rate$Rates[7]
  else if (rate.vec$Gross[i]< rate$income[9])
    rate.vec$state.Tax[i] <- rate$Rates[8]
  else if (rate.vec$Gross[i]< rate$income[10])
    rate.vec$state.Tax[i] <- rate$Rates[9]
  else rate.vec$state.Tax[i] <- max(rate$Rates)
  }
print(rate.vec$state.Tax)
}
