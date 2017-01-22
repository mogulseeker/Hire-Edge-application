salary.monty.carlo <- function(Simulations, Asking.Salary, IRA.percent, Rent.Low, Rent.High, Food.Monthly, Health.Care.Monthly, 
                             Other.Costs.Low, Other.Costs.High, Marital.Status, State){
  
dependencies <- c('XML', 'optimx', 'fGarch', 'reshape2', "xlsx")
#install.packages(dependencies) #Make sure you have these packages
require(XML)
require(optimx)
require(fGarch)
require(reshape2)
require(xlsx) #Read the comment in the bracket function - I would like to make this a live sql query rather than a readXL query
Gross <- rnorm(Simulations, Asking.Salary, Asking.Salary*.03) #Create a large vectore around the gross salary
  #hist(Gross)
IRA.percent <- rsnorm(Simulations, mean = IRA.percent, sd = .03, xi = -8) 
  hist(IRA.percent)  

#Rent.percent <- runif(1000, min=.4, max = .45)
#  hist(Rent.percent)
Rent <- runif(Simulations, min = Rent.Low*12, max=Rent.High*12)
 #hist(Rent)
Food <- rsnorm(Simulations, mean = Food.Monthly*12, sd = .03, xi = 7)
 #hist(Food)  
Health <- rsnorm(Simulations, mean = Health.Care.Monthly*12, sd = .03, xi = -8)
 #hist(Health)
Other.Costs.Yearly <- rnorm(Simulations, (Other.Costs.High*12+Other.Costs.Low*12)/2, Other.Costs.Low*1.67)
  #hist(Other.Costs.Yearly)

#Calculate the distribution of IRA Values, and make sure they are non-negative
IRA.Dist <- expand.grid(Gross*IRA.percent)
for (i in 1:nrow(IRA.Dist)){if (IRA.Dist$Var1[i] < Asking.Salary*.01) #Non-negative constraint
  IRA.Dist$Var1[i] <- Asking.Salary*.01}
min(IRA.Dist$Var1)
hist(IRA.Dist$Var1)

#Factor for tax
#Tax rate bracket function:
#url <- 'http://taxfoundation.org/article/state-individual-income-tax-rates-and-brackets-2016'
#tax.table <- as.data.frame(readHTMLTable(url)) #For some reason, this isn't importing the table
#For the sake of continuity, I imported the set manually as a .csv:
Federal.Tax<-tax.fun.federal(Marital.Status, Asking.Salary, Gross)
State.Tax <- tax.fun.state(State, Marital.Status, Asking.Salary, Gross)
Tax.Rate <- expand.grid(Federal.Tax+State.Tax)
            #coerce.tax.rate #alternatively, you can coerce the raw input
Tax.Dist <-expand.grid(Tax.Rate*Gross) #How nuch will you pay in taxes given various scenarios
Take.Home<-expand.grid(Gross-IRA.Dist-Tax.Dist) #What is your net take home after tax and savings

# Finally, we get to run the simulation :) Run Monte Carlo Simulation on Net Discretionary####
Net.Discretionary.Monthly.Income <- expand.grid(Take.Home-Rent-Food-Health-Other.Costs.Yearly)/12
  colnames(Net.Discretionary.Monthly.Income) <- c("Net Discretionary Monthly Income")
print(Net.Discretionary.Monthly.Income$`Net Discretionary Monthly Income`)
print(hist(Net.Discretionary.Monthly.Income$'Net Discretionary Monthly Income'), main = "Distribution of Monthly Income")
print(c("Average Tax", median(Tax.Rate$Var1)))
}


