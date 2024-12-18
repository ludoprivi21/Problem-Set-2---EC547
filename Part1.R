rm(list=ls())
#setwd(path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ineq)
library(readxl)
library(xtable)

years <- c( "2022","2020","2017", "2014", "2011",
           "2008", "2005", "2002","1999", "1996", "1993")


MyData <- data.frame(
  Year = numeric(), Gini = numeric(), Top10 = numeric(),
  Top1 = numeric(), P90_P10 = numeric(), stringsAsFactors = FALSE )


for (year in years) {
  
  Lor_1 <- read_excel("Lorenz_1.xlsx", 
                      sheet = year )

  red_cum <- cumsum(Lor_1$AGI) / sum(Lor_1$AGI)
  pop_cum <- cumsum(Lor_1$`N. of returns`) / sum(Lor_1$`N. of returns`)
  
  # Aggiungere (0, 0) per la curva
  pop_cum <- c(0, pop_cum)
  red_cum <- c(0, red_cum)
  
  # Calcolare la curva di Lorenz
  pdf(paste0("Rplot/", year, ".pdf"), width = 7, height = 5)
  plot(pop_cum, red_cum, type = "l", col = "blue", lwd = 2,
       xlab = "Percentiles",
       ylab = "Cumulative income share",
       main = paste0("Lorenz Curve - ", year))
  abline(0, 1, col = "red", lty = 2)  # Linea di uguaglianza perfetta
  dev.off()
  
  ## Gini, Top 10%-share, Top 1%-share, p90/p10
  
  # 1. Applicare la formula del coefficiente di Gini
  gini <- 1 - sum((pop_cum[-1] - pop_cum[-length(pop_cum)]) * 
                    (red_cum[-1] + red_cum[-length(red_cum)]))
  
  # 2. Top 10%-share e Top 1%-share
  
  tot_red <- sum(Lor_1$AGI)
  Lor_1$pop_cum_percent <- pop_cum[-1]
  Lor_1$reddito_cum_percent <- red_cum[-1]
  
  # Identificare fasce con più del 90% e 99% della popolazione cumulativa
  top10 <- Lor_1$AGI[Lor_1$pop_cum_percent > 0.9]
  quota_top10 <- sum(top10) / tot_red
  
  top1<- Lor_1$AGI[Lor_1$pop_cum_percent > 0.99]
  quota_top1 <- sum(top1) / tot_red
  
  # 3. p90/p10
  # Individuare i percentili della popolazione
  p90_threshold <- 0.9 * sum(Lor_1$`N. of returns`)
  p10_threshold <- 0.1 * sum(Lor_1$`N. of returns`)
  
  # Reddito al 90° percentile (p90)
  p90 <- NA
  pop_cum_temp <- 0
  for (i in 1:nrow(Lor_1)) {
    pop_cum_temp <- pop_cum_temp + Lor_1$`N. of returns`[i]
    if (pop_cum_temp >= p90_threshold) {
      p90 <- Lor_1$AGI[i] / Lor_1$`N. of returns`[i] # Reddito medio della fascia
      break
    }
  }
  
  # Reddito al 10° percentile (p10)
  p10 <- NA
  pop_cum_temp <- 0
  for (i in 1:nrow(Lor_1)) {
    pop_cum_temp <- pop_cum_temp + Lor_1$`N. of returns`[i]
    if (pop_cum_temp >= p10_threshold) {
      p10 <- Lor_1$AGI[i] / Lor_1$`N. of returns`[i] # Reddito medio della fascia
      break
    }
  }
  
  # Rapporto p90/p10
  p90_p10_ratio <- p90 / p10
  
  # Add data to the final dataframe 
  MyData <- rbind(MyData, data.frame(
    Year = year,
    Gini = gini,
    Top10 = quota_top10,
    Top1 = quota_top1,
    P90_P10 = p90_p10_ratio
  ))
  
}

MyData <- MyData[order(MyData$Year), ]

# Create table with inequality statistics for 2022
latex_code <- xtable(MyData[11,], caption = "Inequality Statistics 2022")
print(latex_code, type = "latex", file = "ineq_stats.tex")

### Plot Inequality Statistics Over time

pdf(paste0("Rplot/Gini.pdf"), width = 7, height = 5)
plot(MyData$Year, MyData$Gini, type = "o", col = "blue", 
     xlab = "Year", ylab = "Gini Coefficient", main = "Gini Coefficient - 1993-2022")
dev.off()

pdf(paste0("Rplot/Top10.pdf"), width = 7, height = 5)
plot(MyData$Year, MyData$Top10, type = "o", col = "orange", 
     xlab = "Year", ylab = "Top 10%", main = "Top 10% - 1993-2022")
dev.off()


pdf(paste0("Rplot/Top1.pdf"), width = 7, height = 5)
plot(MyData$Year, MyData$Top1, type = "o", col = "red", 
     xlab = "Year", ylab = "Top 1%", main = "Top 1% - 1993-2022")
dev.off()

pdf(paste0("Rplot/P90_P10.pdf"), width = 7, height = 5)
plot(MyData$Year, MyData$P90_P10, type = "o", col = "darkgreen", 
     xlab = "Year", ylab = "P90/P10", main = "P90/P10 - 1993-2022")
dev.off()




