rm(list=ls())
#setwd(path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(readxl)
library(dplyr)

Lor_1 <- read_excel("Lorenz_1.xlsx", sheet = "2022")

# find minimum income for each bracket
min_inc<-as.numeric(gsub("\\D", "", sub(" .*", "", Lor_1$`Size of adjusted gross income`)))

# Add pareto coefficients to the table
Lor_1<-Lor_1%>%
  cbind(min_inc)%>%
  dplyr::mutate(beta=(sum(AGI)-lag(cumsum(Lor_1$AGI)))/(sum(Lor_1$`N. of returns`)-lag(cumsum(Lor_1$`N. of returns`)))/min_inc)

pdf(paste0("Rplot/Inv_Pareto_2022.pdf"), width = 7, height = 5)
plot(Lor_1$min_inc/100000, Lor_1$beta, type = "o", col = "darkgreen", 
     xlab = "Income scaled by 100.000 ", ylab = "Beta", main = "Inverted Pareto Coefficients - 2022")
dev.off()


### Inverted Pareto Coeff Over Time 

years <- c( "2022","2020","2017", "2014", "2011",
            "2008", "2005", "2002","1999", "1996", "1993")

MyData <- data.frame(
  Year = numeric(), Beta = numeric(), stringsAsFactors = FALSE )


for (year in years) {
  
  Lor_1 <- read_excel("Lorenz_1.xlsx", 
                      sheet = year )
  
  min_inc<-as.numeric(gsub("\\D", "", sub(" .*", "", Lor_1$`Size of adjusted gross income`)))
  
  Lor_1<-Lor_1%>%
    cbind(min_inc)%>%
    dplyr::mutate(beta=(sum(AGI)-lag(cumsum(Lor_1$AGI)))/(sum(Lor_1$`N. of returns`)-lag(cumsum(Lor_1$`N. of returns`)))/min_inc)
  
  n_row <- as.numeric(nrow(Lor_1))
  
  MyData <- rbind(MyData, data.frame(
    Year = year,
    Beta = Lor_1$beta[n_row]
  ))
}

MyData <- MyData[order(MyData$Year), ]

# Find Pareto Coeff - i.e. alpha 
MyData$Pareto_Coeff <- MyData$Beta/(MyData$Beta-1)

# Plots: 
pdf(paste0("Rplot/Inv_Pareto_Coeff.pdf"), width = 7, height = 5)
plot(MyData$Year, MyData$Beta, type = "o", col = "blue", 
     xlab = "Year", ylab = "Beta", main = "Inverted Pareto Coefficients - 1993-2022")
dev.off()

pdf(paste0("Rplot/Pareto_Coeff.pdf"), width = 7, height = 5)
plot(MyData$Year, MyData$Pareto_Coeff, type = "o", col = "red", 
     xlab = "Year", ylab = "Pareto Coefficient", main = "Pareto Coefficients - 1993-2022")
dev.off()


###OPTIMAL TAXATION 

g = 0.5
el = 0.25


# Formula for optimal taxation: opt_tax <- (1-g)/(1-g+b*el)

for (i in 1:11) { 
  MyData$Opt_tax[i] <-(1-g)/(1- g + (MyData$Pareto_Coeff[i]*el))
}

pdf(paste0("Rplot/Opt_tax.pdf"), width = 7, height = 5)
plot(MyData$Year, MyData$Opt_tax, type = "o", col = "blue", 
     xlab = "Year", ylab = "Tax Rate", main = "Optimal Tax Rate for top income earners - 1993-2022")
dev.off()





