rm(list=ls())
#setwd(path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(plm)
library(tidyverse)
library(readxl)
library(dplyr)
library(broom)            
library(ggplot2)
library(xtable)



data_epl<-read.csv("EPL.csv")%>%
  select(REF_AREA,MEASURE,TIME_PERIOD,OBS_VALUE)

data_pop <- read.csv("Pop_25_64.csv")%>%
  select(REF_AREA,MEASURE,AGE,TIME_PERIOD,OBS_VALUE)

data_empl <- read.csv("Empl_rate.csv")%>%
  select(LOCATION,SUBJECT,MEASURE,TIME_PERIOD,OBS_VALUE)

### Find the Working Population by age group 
names <- unique(data_pop$REF_AREA)
years <- unique(data_pop$TIME_PERIOD)

df_pop <- data.frame(
  REF_AREA = character(),
  Year = numeric(), Pop_25_54 = numeric(), 
  Pop_55_64= numeric(), Pop_25_64 = numeric(),
  stringsAsFactors = FALSE )

for (name in names) {
    
  MySub <- subset(data_pop, REF_AREA == name)
  
  for (year in years){
    
    MySub_2 <- subset(MySub, TIME_PERIOD == year)
    
    Pop_25_64 <- sum(MySub_2$OBS_VALUE)
    Pop_55_64 <- MySub_2[MySub_2$AGE == "Y55T59", "OBS_VALUE"] + MySub_2[MySub_2$AGE == "Y60T64", "OBS_VALUE" ]
    
    Pop_25_54 <- Pop_25_64 - Pop_55_64
    
    df_pop <- rbind(df_pop, data.frame(
      REF_AREA = name, 
      Year = year,
      Pop_25_54 = Pop_25_54,
      Pop_55_64 = Pop_55_64,
      Pop_25_64 = Pop_25_64))
  }
}

rm(MySub, MySub_2)


### Find the employment/population ratio for ages 25-64


df_empl_ratio <- data.frame(
  REF_AREA = character(),
  TIME_PERIOD = numeric(), Empl_25_64 = numeric(),
  stringsAsFactors = FALSE )

for (name in names) {
  
  empl <- subset(data_empl, LOCATION == name)
  pop <- subset(df_pop, REF_AREA == name)
  
  years <- unique(empl$TIME_PERIOD)
  years <- sort(years)
  
  for (year in years){
    
    empl_year <- subset(empl, TIME_PERIOD == year)
    pop_year <- subset(pop, Year == year)
    
    Empl_ratio <- ((empl_year[empl_year$SUBJECT == "55_64", "OBS_VALUE"]*pop_year$Pop_55_64 +
      + empl_year[empl_year$SUBJECT == "25_54", "OBS_VALUE"]*pop_year$Pop_25_54)/ pop_year$Pop_25_64)/100
  
    
    df_empl_ratio <- rbind(df_empl_ratio, data.frame(
      REF_AREA = name, 
      TIME_PERIOD = year,
      Empl_25_64 = Empl_ratio ))
    
    }
}

rm(empl, empl_year, pop, pop_year)


MyData <- merge(data_epl, df_empl_ratio, by = c("REF_AREA", "TIME_PERIOD"))

MyData <- MyData[,-3]

colnames(MyData) <- c("REF_AREA", "TIME_PERIOD","EPL","EMPL")

### Scatter Plot for employment-population ratio against EPL-strictness - 2018

MyData_2018 <- MyData%>%filter(TIME_PERIOD==2018)

pdf(paste0("Rplot_2/Scatter_plot.pdf"), width = 7, height = 5)
ggplot(MyData_2018, aes(x = EPL, y = EMPL)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm",se = FALSE) +
  labs(
    title = "EPL Strictness vs Employment/Population ages 25-64 in 2018",
    x = "EPL Strictness",
    y = "Employment/Population"
  ) +
  theme_minimal()

dev.off()


### Relationship over time

# Subset for countries that are available in all years

country_year_count <- table(MyData$REF_AREA, MyData$TIME_PERIOD)
valid_countries <- names(which(rowSums(country_year_count == 1) == 20))

subset_df <- MyData[MyData$REF_AREA %in% valid_countries, ]


# Employment ratio vs EPL over time 

regression_results <- subset_df %>%
  group_by(TIME_PERIOD) %>%
  do(tidy(lm(EMPL ~ EPL, data = .))) %>%
  filter(term == "EPL") %>%
  mutate(conf_5_P =estimate-1.96*std.error,
         conf_95_P=estimate+1.96*std.error) %>%
  select(TIME_PERIOD, estimate, conf_5_P, conf_95_P)


# pdf(paste0("Rplot_2/Est_coeff.pdf"), width = 7, height = 5)
# ggplot(regression_results, aes(x = TIME_PERIOD, y = estimate)) +
#   geom_point(color = "darkgreen") +
#   geom_line(color = "darkgreen") +
#   geom_ribbon(aes(ymin = conf_5_P, ymax = conf_95_P), fill = "darkgreen",alpha = 0.1) +
#   labs(
#     title = "Association Between EPL-Strictness and Employment/Population",
#     x = "Year",
#     y = "Estimated Regression Coefficient"
#   ) +
#   theme_minimal()
# 
# dev.off()

pdf(paste0("Rplot_2/Est_coeff.pdf"), width = 7, height = 5)
ggplot(regression_results, aes(x = TIME_PERIOD, y = estimate)) + 
  geom_point(color = "darkblue") + 
  geom_line(aes(color = "Estimate"), size = 1) +  # Linea solida per la stima
  geom_line(aes(y = conf_5_P, color = "95% Confidence Interval"), linetype = "dashed", size = 1) +  # Limite inferiore dell'IC
  geom_line(aes(y = conf_95_P, color = "95% Confidence Interval"), linetype = "dashed", size = 1) +  # Limite superiore dell'IC
  labs(
    title = "Association Between EPL-Strictness and Employment/Population",
    x = "Year",
    y = "Estimated Regression Coefficient"
  ) + 
  scale_color_manual(values = c("Estimate" = "darkblue", "95% Confidence Interval" = "blue")) +  # Impostazioni colori
  theme_minimal() +
  theme(
    legend.position = "top",   # Posiziona la legenda in alto
    legend.justification = "right",  # Allinea la legenda a sinistra
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Bordo e sfondo della legenda
    legend.title = element_blank()  # Rimuove il titolo della legenda
  )

dev.off()





# Fixed Effect Regression

# Convert to panel data format
pdata <- pdata.frame(subset_df, index = c("REF_AREA", "TIME_PERIOD"))

# Fixed effects model - v1
model <- plm(EMPL ~ EPL, data = pdata, effect = "twoways", model = "within")
summary(model)

# Table with regression results 
summary_table <- tidy(model)

summary_table <- tidy(model) %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE            ~ ""
  )) %>%
  mutate(estimate_with_stars = paste0(round(estimate, 4), significance))


clean_table <- summary_table %>%
  select(term, estimate_with_stars, std.error, p.value) %>%
  rename(
    Variable = term,
    Estimate = estimate_with_stars,
    `Std. Error` = std.error,
    `P-Value` = p.value
  )

latex_code <- xtable(clean_table, caption = "Model Summary")
print(latex_code, type = "latex", file = "table_latex.tex")




