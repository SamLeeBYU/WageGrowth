library(tidyverse)
library(lubridate)
library(zoo)
library(promises)

#My hourly wages
Wages = tibble(
  Date = c(
    seq.Date(as.Date(as.yearmon("2019-07")), as.Date(as.yearmon("2019-11")), by="months"),
    seq.Date(as.Date(as.yearmon("2020-03")), as.Date(as.yearmon("2020-08")), by="months"),
    seq.Date(as.Date(as.yearmon("2022-07")), as.Date(as.yearmon("2022-12")), by="months"),
    seq.Date(as.Date(as.yearmon("2023-01")), as.Date(as.yearmon("2023-02")), by="months"),
    seq.Date(as.Date(as.yearmon("2023-03")), as.Date(as.yearmon("2023-04")), by="months"),
    seq.Date(as.Date(as.yearmon("2023-05")), as.Date(as.yearmon("2023-06")), by="months"),
    seq.Date(as.Date(as.yearmon("2023-07")), as.Date(as.yearmon("2023-08")), by="months")
  ),
  NominalWages = c(
    rep(9, 5),
    rep(10, 6),
    rep(14, 6),
    rep(12, 2),
    rep(13, 2),
    rep(13.125, 2),
    rep(13.5, 2)
  ),
  JobTitle = c(
    rep("Subway", 5),
    rep("Mathnasium", 6),
    rep("MTC Gym", 6),
    rep("Research, TA Jobs", 8)
  )
)

#We will use data from BLS to estimate real wages

library(RSelenium)
library(netstat)

rs_driver_object <- rsDriver(
  browser = "firefox",
  chromever = NULL,
  verbose = F,
  port = free_port(), #4555L
)

real_wage_calculator_url = "https://data.bls.gov/cgi-bin/cpicalc.pl"

remDr <- rs_driver_object$client
remDr$open()
remDr$navigate(real_wage_calculator_url)

base_month = remDr$findElement(using = "css selector", "#year2-month")
base_month = remDr$executeScript("return arguments[0].options[arguments[0].selectedIndex].text;", list(base_month))[[1]]
base_year = remDr$findElement(using = "css selector", "#year2-year")
base_year = remDr$executeScript("return arguments[0].options[arguments[0].selectedIndex].text;", list(base_year))[[1]]
base_date = as.Date(as.yearmon(str_c(base_year, "-", which(month.name == base_month))))

get_real_wage = function(wage, date){
  month = month.name[date %>% month()]
  year = date %>% year()
  
  #Clear the span
  loaded_wage = NA
  remDr$executeScript("arguments[0].textContent = '';", list(remDr$findElement(using="css selector", "#answer")))
  
  #Clear the input
  remDr$findElement(using="css selector", "#cost1")$clearElement()
  
  remDr$findElement(using="css selector", "#year1-month")$sendKeysToElement(list(month))
  remDr$findElement(using="css selector", "#year1-year")$sendKeysToElement(list(as.character(year)))
  
  remDr$findElement(using="css selector", "#cost1")$sendKeysToElement(list(as.character(wage)))
  
  if(date > base_date){
    real_wage = NA
  } else {
    remDr$findElement(using="css selector", "#submit")$submitElement()
    Sys.sleep(0.5)
    while(is.na(loaded_wage)){
      check_visibility <- function() {
        return(remDr$findElement(using="css selector", "#answer")$getElementAttribute("style") != "display: none;")
      }
      while(!check_visibility()){
        Sys.sleep(0.5)
      }
      real_wage = remDr$findElement(using="css selector", "#answer")$getElementText()[[1]] %>% parse_number()
      loaded_wage = real_wage
      Sys.sleep(0.5)
    } 
  }
  
  return(real_wage)
}

Wages$RealWages = mapply(get_real_wage, Wages$NominalWages, Wages$Date)

Wages %>% group_by(JobTitle) %>%
  summarize(
    RealWageMean = mean(RealWages, na.rm=T),
    NominalWageMean = mean(NominalWages, na.rm=T),
    Difference = RealWageMean-NominalWageMean
  )

#Calculate the variation in inflation where wages stayed the same
changes = c()
TotalChanges = 1
previous_wage = Wages$NominalWages[1]
alpha = 0
for(i in 1:length(Wages$NominalWages)){
  wage = Wages$NominalWages[i]
  #Alternatively if the equivalence is too strict, we could group it by some alpha-neighborhood, i.e. if it's within $0.10, we could say those wages are equivalent.
  if(abs(wage - previous_wage) <= alpha){
    TotalChanges = TotalChanges + 1
    previous_wage = wage
  }
  changes[i] = TotalChanges
}

change_at_kth_index = 1
Wages$SS = rep(NA, nrow(Wages))
for(i in 1:length(unique(changes))){
  RealWageIndex = change_at_kth_index:(change_at_kth_index+length(which(i == changes))-1)
  RealWageMean = mean(Wages$RealWages[RealWageIndex], na.rm=T)
  print(sum((Wages$RealWages[RealWageIndex]-RealWageMean)^2))
  Wages$SS[RealWageIndex] = rep(sum((Wages$RealWages[RealWageIndex]-RealWageMean)^2), length(RealWageIndex))
  change_at_kth_index = change_at_kth_index+length(which(i == changes))
}

#We can find the months that our real wage was most sporadic.
Wages %>% arrange(desc(SS))

#Nominal Wage Average
mean(Wages$NominalWages)
mean(Wages$RealWages, na.rm=T)

Wages = Wages %>% pivot_longer(cols = c("NominalWages", "RealWages"), names_to="WageType", values_to = "Wage")

WageGrowth = Wages %>% ggplot(mapping = aes(x=Date, y=Wage, color=WageType))+
  geom_line(aes(group=interaction(JobTitle, WageType)))+
  geom_smooth(aes(group=WageType, color=WageType), se=F)+
  geom_point(position="jitter")+
  labs(
    title = "Real vs. Nominal Wage Growth Over Time",
    subtitle = str_c("Real wages are based in ", base_month, ", ", base_year, " US $"),
    x = "Month",
    y = "Wage ($)",
    color = "Wage Type"
  ) +
  scale_color_manual(
    values = c("#CF1259", "#5C7AFF"),
    labels = c("Nominal", "Real")
  )+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
WageGrowth

WageGrowth %>% ggsave("Wage Growth.png", .)
