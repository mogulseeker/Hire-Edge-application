 #Set up raw input values
State <- "Colorado" #Must Spell out the whole state
coerce.tax.rate <- .19
Simulations <- 10000
IRA.percent <- .08
Rent.Low <- 1700
Rent.High <- 1900
Other.Costs.Low <- 300 
Other.Costs.High <- 500
Health.Care.Monthly<-150
Food.Monthly <- 650
Asking.Salary<-65000 #I would like to add a scale function to factor in a range of outputs for the user to control (right now standard deviation = value*.03)
#I will add a slight skewness coefficient to the final algorithym because people tend to overestimate salary
Marital.Status<- "Single" #Must Be "Single" or "Married"
#There is an additional bracket for head of household, it's complicated to put in , so we'll leave it blank for now


dist<-salary.monty.carlo(Simulations, Asking.Salary, IRA.percent, Rent.Low, Rent.High, Food.Monthly, Health.Care.Monthly, 
  Other.Costs.Low, Other.Costs.High, Marital.Status, State)


# optimx(dist, fn = salary.monty.carlo()
Sal <- (65000*(1-coerce.tax.rate))/13
Sal - 1700-300-150-650
