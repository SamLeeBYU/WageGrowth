library(tidyverse)
library(lubridate)
library(RSelenium)
library(netstat)
library(rvest)
library(zoo)

#Student Wages
rs_driver_object <- rsDriver(
  browser = "firefox",
  chromever = NULL,
  verbose = F,
  port = free_port(), #4555L
)

student_jobs = "https://hrms.byu.edu/psc/ps/EMPLOYEE/HRMS/c/HRS_HRAM_EMP.HRS_APP_SCHJOB.GBL?Page=HRS_APP_SCHJOB&Action=U&FOCUS=Employee&SiteId=50&PortalActualURL=https%3a%2f%2fhrms.byu.edu%2fpsc%2fps%2fEMPLOYEE%2fHRMS%2fc%2fHRS_HRAM_EMP.HRS_APP_SCHJOB.GBL%3fPage%3dHRS_APP_SCHJOB%26Action%3dU%26FOCUS%3dEmployee%26SiteId%3d50&PortalContentURL=https%3a%2f%2fhrms.byu.edu%2fpsc%2fps%2fEMPLOYEE%2fHRMS%2fc%2fHRS_HRAM_EMP.HRS_APP_SCHJOB.GBL%3fFOCUS%3dEmployee&PortalContentProvider=HRMS&PortalCRefLabel=Careers&PortalRegistryName=EMPLOYEE&PortalServletURI=https%3a%2f%2fhrms.byu.edu%2fpsp%2fps%2f&PortalURI=https%3a%2f%2fhrms.byu.edu%2fpsc%2fps%2f&PortalHostNode=HRMS&NoCrumbs=yes&PortalKeyStruct=yes"

remDr <- rs_driver_object$client
remDr$open()
remDr$navigate(student_jobs)

numResults = remDr$findElement(using="css selector", "#HRS_SCH_WRK_HRS_SES_CNTS_MSG")$getElementText() %>% .[[1]] %>% parse_number()

check_visibility <- function(using="css selector", id, index=1) {
  visible = F
  tryCatch({
    tryCatch({
      visible = remDr$findElements(using=using, id)[[index]]$getElementAttribute("style") != "display: none;"
    })
  }, error = function(e){
    visible = F
  })
  return(visible)
}

check_visibility2 <- function(el) {
  visible = F
  tryCatch({
    visible = el[[1]]$getElementAttribute("style") != "display: none;"
  }, error = function(e){
    # print(e)
    visible = F
  })
  el = NULL
  return(visible)
}

is_valid_link <- function(l){
  parsed = l %>% str_split("[javascript:submitAction_win0(document.win0,]") %>% .[[1]] %>% str_trim()
  parsed = parsed[nchar(parsed) > 0]
  "'#ICS" %in% parsed[1]
}

StudentJobs = tibble()
counter_text = remDr$findElements(using="css selector", ".PSGRIDCOUNTER")[[1]]$getElementText() %>% .[[1]]
for(page in 1:(ceiling(numResults / 25))){ #There are 25 results per page
  print(str_c("Scraping page#", page))
  if(page > 1){
    #Check to see if the counter is visible
    while(!check_visibility2(remDr$findElements(using="css selector", ".PSGRIDCOUNTER"))){
      Sys.sleep(0.5)
    }
    print("Going to next page...")
    remDr$executeScript("javascript:submitAction_win0(document.win0,'HRS_AGNT_RSLT_I$hdown$0');")
    while(remDr$findElements(using="css selector", ".PSGRIDCOUNTER")[[1]]$getElementText() %>% .[[1]] == counter_text){
      Sys.sleep(1)
    }
    counter_text = remDr$findElements(using="css selector", ".PSGRIDCOUNTER")[[1]]$getElementText() %>% .[[1]]
  }
  print("The counter changed.")
  results <- remDr$getPageSource()[[1]] %>% read_html() %>%
    html_nodes("table") %>% .[[16]] %>% html_nodes("tr")
  
  job_titles <- results %>%
    html_nodes("td:nth-child(2)") %>%
    html_nodes("a.PSHYPERLINK") %>%
    html_text() %>%
    strsplit(" - ") %>%
    sapply(function(x) ifelse(length(x) > 0, x[[1]], NA))
  
  job_titles <- na.omit(job_titles) %>% str_trim()
  
  departments <- results %>%
    html_nodes("span") %>%
    html_text() %>%
    str_extract("(?<=Department: )[^|]+") %>% na.omit() %>% str_trim()
  
  links = remDr$findElements(using="tag name", "table")[[16]]$getPageSource()[[1]] %>% read_html() %>%
    html_nodes("a.PSHYPERLINK") %>% html_attr("href")
  valid_links = links[sapply(links, is_valid_link)]
  
  #print(job_titles)
  #print(departments)

  date_posted = c()
  openings = c()
  start_dates = c()
  wages = c()
  for(i in 1:(length(valid_links))){ #The first two and the last <tr> tags contain no relevant information
    
    #Redefine the table so it doesn't go stale
    while(!check_visibility(using="tag name", id="table", index=16)){
      Sys.sleep(0.5)
    }
    print("The table on the main page is visible.")
    table = remDr$findElements(using="tag name", "table")[[16]]

    links = remDr$findElements(using="tag name", "table")[[16]]$getPageSource()[[1]] %>% read_html() %>%
      html_nodes("a.PSHYPERLINK") %>% html_attr("href")
    valid_links = links[sapply(links, is_valid_link)]
    link = valid_links[i]
    remDr$executeScript(link)
    
    
    while(!check_visibility2(remDr$findElements(using="css selector", "#okbutton")) && 
          !check_visibility2(remDr$findElements(using="tag name", "table")[[6]]$findElements(using="tag name", "tr")[[3]]$findElements(using="tag name", "td")[[2]]$findElements(using="css selector", ".PSLONGEDITBOX")[[1]]$findElements(using="tag name", "p"))){
      Sys.sleep(0.5)
    }
    if(check_visibility2(remDr$findElements(using="tag name", "table")[[6]]$findElements(using="tag name", "tr")[[3]]$findElements(using="tag name", "td")[[2]]$findElements(using="css selector", ".PSLONGEDITBOX")[[1]]$findElements(using="tag name", "p"))){
      #Wait for information to load
      while(!check_visibility2(remDr$findElements(using="tag name", "table")[[6]]$findElements(using="tag name", "tr")[[3]]$findElements(using="tag name", "td")[[2]]$findElements(using="css selector", ".PSLONGEDITBOX")[[1]]$findElements(using="tag name", "p"))){
        Sys.sleep(0.5)
      }
      print("The information is visible")
      
      information = remDr$findElements(using="tag name", "table")[[6]]$findElements(using="tag name", "tr")[[3]]$findElements(using="tag name", "td")[[2]]$findElements(using="css selector", ".PSLONGEDITBOX")[[1]]$findElements(using="tag name", "p")[[1]]$getElementText() %>% 
        .[[1]] %>% str_split("\n") %>% .[[1]]
      date_posted[i] = information[2] %>% str_split(": ") %>% .[[1]] %>% .[2] %>% mdy()
      openings[i] = information[3] %>% str_split(": ") %>% .[[1]] %>% .[2] %>% parse_number()
      start_dates[i] = information[4] %>% str_split(": ") %>% .[[1]] %>% .[2] %>% mdy()
      partitions = information[6] %>% str_split(": ") %>% .[[1]]
      wages[i] =  paste0(partitions[str_detect(partitions, "\\d")], collapse = "") %>% parse_number()
      if(!is.na(wages[i]) && wages[i] >= 1000){
        #This is a typo where they forgot to put in the decimal
        wages[i] = wages[i]/100
      }
      
      #After scraping the data go back and continue scraping the next row
      goBack = remDr$findElement(using = "css selector", "#HRS_SCH_WRK_HRS_SCH_LNK06")
      goBack$clickElement()
    } else {
      date_posted[i] = NA_real_
      openings[i] = NA_real_
      start_dates[i] = NA_real_
      wages[i] = NA_real_
      remDr$findElement(using="css selector", "#okbutton")$clickElement()
    }
  }
  
  page_data = tibble(
    JobTitle = job_titles,
    Department = departments,
    DatePosted = date_posted,
    StartDate = start_dates,
    Openings = openings,
    Wage = wages
  )
  if(nrow(StudentJobs) <= 0){
    StudentJobs <<- page_data
  } else {
    StudentJobs <<- rbind(StudentJobs, page_data)
  }
}
  
CleanedStudentJobs <- StudentJobs %>% mutate(
  DatePosted = DatePosted %>% as.Date(),
  StartDate = DatePosted %>% as.Date(),
  ID = 1:nrow(StudentJobs),
  DateScraped = Today()
) %>% filter(!is.na(Wage))

CleanedStudentJobs

JobStats = CleanedStudentJobs %>% mutate(
  TotalOpenings = sum(Openings),
  PropWage = (Openings/TotalOpenings)*Wage
)

#Mean Wage (weighted by the number of openings (i.e. number of jobs))
JobStats %>% pull(PropWage) %>% sum()

#Number of jobs
N = JobStats %>% pull(TotalOpenings) %>% first()

RecentJobs = JobStats[JobStats$DatePosted >= ymd("2023-07-01"),]
OldJobs = JobStats[JobStats$DatePosted < ymd("2023-07-01"),]

#Mean wage of old jobs
sum((OldJobs$Openings/sum(OldJobs$Openings))*OldJobs$Wage)

#Mean wage of new jobs
sum((RecentJobs$Openings/sum(RecentJobs$Openings))*RecentJobs$Wage)

#Get real wages for Old Jobs ######################################################################
real_wage_calculator_url = "https://data.bls.gov/cgi-bin/cpicalc.pl"

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
  
  remDr$findElement(using="css selector", "#year1-month")$sendKeysToElement(list(base_month))
  remDr$findElement(using="css selector", "#year1-year")$sendKeysToElement(list(as.character(base_year)))
  
  remDr$findElement(using="css selector", "#year2-month")$sendKeysToElement(list(month))
  remDr$findElement(using="css selector", "#year2-year")$sendKeysToElement(list(as.character(year)))
  
  remDr$findElement(using="css selector", "#cost1")$sendKeysToElement(list(as.character(wage)))
  
  if(date > base_date){
    real_wage = NA
  } else {
    remDr$findElement(using="css selector", "#submit")$submitElement()
    Sys.sleep(0.5)
    while(is.na(loaded_wage)){
      check_visibility <- function() {
        rw = F
        tryCatch({
          rw = remDr$findElement(using="css selector", "#answer")$getElementAttribute("style") != "display: none;"
        }, error = function(e){
          rw = F
        })
        return(rw)
      }
      visibility = check_visibility()
      while(is.na(visibility) || !check_visibility()){
        Sys.sleep(0.5)
        visibility = check_visibility()
      }
      tryCatch({
        real_wage = remDr$findElement(using="css selector", "#answer")$getElementText()[[1]] %>% parse_number()
      },error=function(e){
        print(e)
      })
      loaded_wage = real_wage
    } 
    Sys.sleep(0.5)
  }
  
  return(real_wage)
}

RecentJobs$RealWages = RecentJobs$Wage
OldJobs$RealWages = mapply(get_real_wage, OldJobs$Wage, OldJobs$DatePosted)

#Now recalculate the mean wage
sum((OldJobs$Openings/sum(OldJobs$Openings))*OldJobs$RealWages)

JobStats$RealWages = JobStats$Wage
matching_indices <- match(OldJobs$ID, JobStats$ID)
JobStats$RealWages[matching_indices] <- OldJobs$RealWages

#What this means is when adjusted for inflation,
#the wages offered from earlier jobs are not enough to keep up with the wages of the recent jobs 

recent_wages = c()
old_wages = c()
#Are these two means statistically different?
for(i in 1:nrow(JobStats)){
  wage = JobStats$RealWages[i]
  openings = JobStats$Openings[i]
  date = JobStats$DatePosted[i]
  if(date >= base_date){
    recent_wages = c(recent_wages, rep(wage, openings))
  } else {
    wage = 
    old_wages = c(old_wages, rep(wage, openings))
  }
}

mean(recent_wages)
mean(old_wages)

t.test(recent_wages, old_wages, var.equal=T)

real_wages = c(recent_wages, old_wages)
mean(real_wages)

#95% CI for the true mean of the starting wage
mean(real_wages)+c(-1,1)*qt(0.975, df=length(real_wages)-1)*sd(real_wages)/sqrt(length(real_wages))
t.test(real_wages)

#ANOVA model
JobStatsDuplicated = JobStats %>%
  slice(rep(row_number(), times = Openings))
JobStatsDuplicatedSummarized = JobStatsDuplicated %>% group_by(Department) %>%
  mutate(
    DepartmentMeanRealWage = mean(RealWages),
    N = n()
  )
#5 or more repetitions for analysis
JobStatsDuplicatedSummarizedANOVA = JobStatsDuplicatedSummarized %>% filter(N >= 5)
unique(JobStatsDuplicatedSummarizedANOVA$Department) %>% length() #36 Departments with 5 or more listings
JobStatsDuplicatedSummarizedANOVA %>% group_by(Department) %>%
  summarize(
    DepartmentMeanRealWage = first(DepartmentMeanRealWage),
    NumberOfListings = first(N)
  ) %>% write_csv(str_c("DepartmentMeanWages","-",JobStats$DateScraped%>%first(),".csv"))
JobStatsDuplicatedSummarizedANOVA.lm = lm(RealWages ~ Department, data = JobStatsDuplicatedSummarizedANOVA)

#Does department have a significant effect on real wage outcome?
anova(JobStatsDuplicatedSummarizedANOVA.lm)

library(emmeans)
emmeans(JobStatsDuplicatedSummarizedANOVA.lm, ~Department) %>% as_tibble() %>% View()

emmeans(JobStatsDuplicatedSummarizedANOVA.lm, ~Department) %>% as_tibble() %>%
  write_csv(str_c("DepartmentalRealWagesCI","-",JobStats$DateScraped%>%first(),".csv"))

min_wage <- floor(min(JobStatsDuplicated$RealWages))
max_wage <- ceiling(max(JobStatsDuplicated$RealWages))

#Distribution of wages in Provo
wage_distribution = JobStatsDuplicated %>% ggplot()+
  geom_histogram(mapping=aes(x=RealWages), fill="#2364AA", binwidth = 0.50)+
  scale_x_continuous(breaks = seq(min_wage, max_wage, by = 0.50), labels = scales::dollar_format(prefix = "$"), limits = c(min_wage, max_wage)) +
  labs(x = str_c("Real Wages - (All Wages Adjusted to ", as.yearmon(base_date), " Prices) with data from the BLS"), y = "Number of Openings", title = "Distribution of Starting Wages for On Campus Student Jobs at BYU", subtitle = str_c("As of ", first(JobStatsDuplicated$DateScraped), ". N = ", N, ". Mean = ", round(mean(JobStatsDuplicated$RealWages), 2), ". Median = ", median(JobStatsDuplicated$RealWages))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
wage_distribution

wage_distribution %>% ggsave("BYU Campus Job Wage Distribution.png", ., width=12, height=6)

JobStats %>% select(JobTitle, Department, DatePosted, StartDate, Openings, Wage, RealWages, DateScraped) %>%
  write_csv(str_c("BYUCampusJobs","-",JobStats$DateScraped%>%first(),".csv"))


########################################################################################

#Wages in Provo