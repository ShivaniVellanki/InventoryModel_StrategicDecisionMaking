AnnualDemand <- 15000
OrderCosts<-220
UnitCosts<- 80
CostofHoldingItem <- 18
HoldingCosts<- (CostofHoldingItem/100)*UnitCosts
HoldingCosts
EconomicOrderQuantityEOQ<-round(sqrt((2*AnnualDemand*OrderCosts)/HoldingCosts), digits = 0)
EconomicOrderQuantityEOQ
QuantityQ<-2*EconomicOrderQuantityEOQ
QuantityQ
AnnualOrderingCosts<-round((AnnualDemand*OrderCosts)/QuantityQ,digits = 0)
AnnualOrderingCosts
AnnualHoldingCosts<-round((QuantityQ*HoldingCosts)/2, digits = 0)
AnnualHoldingCosts
TotalCosts <-AnnualOrderingCosts+AnnualHoldingCosts
TotalCosts

# Part 2 
install.packages('EnvStats')
library('EnvStats')

# (i)	calculating a 95% confidence interval to estimate the minimum total cost

orders <- sample(450:1000,1000,replace = T)  # 1000 random Order Quantities are used in the simulation.
orders

TRI <- rtri(1000 , 13000, 17000, 15000) # Making a triangle out of the annual demand with the given minimum, maximum, and mode
TRI

plot(density(TRI))  # Triangular Distribution plot

plot(orders , totalCost)

orderDataFrame <- data.frame(orders ,TRI) 
orderDataFrame

totalCost <- as.numeric( (OrderCosts*TRI/orders) + (HoldingCosts*orders)/2 ) # Total price for each Order Quantity
print(totalCost)

hist(totalCost)      

d <- density(totalCost)  
plot(d)

meanCostSample <- mean(totalCost)
meanCostSample
sdCostSample <- sd(totalCost)
sdCostSample

print(meanCostSample - 1.96*sdCostSample)
print(meanCostSample + 1.96*sdCostSample)

# (ii)	creating a 95% confidence interval to estimate the order quantity

orderQuantity <- as.numeric(sqrt(2*TRI*OrderCosts/HoldingCosts))  
orderQuantity <- round(orderQuantity,digits = 0)
orderQuantity

hist(orderQuantity)
d2 <- density(orderQuantity)  
plot(d2)

meanOrderQuantity <- mean(orderQuantity)
meanOrderQuantity
sdOrderQuantity <- sd(orderQuantity)

print(meanOrderQuantity - 1.96*sdOrderQuantity)  
print(meanOrderQuantity + 1.96*sdOrderQuantity)

# (iii)	Estimating the Annual Number of orders by constructing a 95% CI

numberOfOrders <- as.numeric(TRI/orderQuantity) 
numberOfOrders

hist(numberOfOrders)  
plot(density(numberOfOrders)) 

meanNumberOfOrders <- mean(numberOfOrders)
meanNumberOfOrders
sdNumberOfOrders <- sd(numberOfOrders)

print(meanNumberOfOrders - 1.96*sdNumberOfOrders) 
print(meanNumberOfOrders + 1.96*sdNumberOfOrders)