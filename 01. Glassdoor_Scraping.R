### Installing packages

# install.packages('httr')
# install.packages('xml2')
# install.packages('rvest')
# install.packages('tidyverse')

library(httr)  
library(xml2)  
library(rvest)
library(tidyverse)

### Glassdoor cuts job searches after 30 pages. Probablly to prevent scraping 
### One way to get behind this is to do the search by state and limit each
### State by 30 pages maximum, then can get more job pulls than 900

getwd()
setwd('E:/Data Science/01. Projects/Salary_Estimator/R')

### Pull in text file of Glassdoor Links by state and convert to list
URLstate = read.csv('URL.csv')
URLstate = as.list(URLstate$Truncated_URL)
URLjob = paste0(URLstate,'.htm')

### Find the number of total jobs per state search
totJobs = lapply(URLjob, function(i){
  read_html(i) %>%
    html_nodes('.jobsCount') %>%
    html_text() %>%
    gsub('[^0-9]','',.) %>%
    as.integer()
})

### 30 jobs per pages, want to max at 30 total.
totPages = lapply(totJobs,function(i){as.integer(min(ceiling(i/30),30))})
rm(totJobs)
totPages

### Loop along each state link to create the pages link
URLall = lapply(seq_along(URLstate), function(i) paste0(URLstate[[i]], '_IP', seq_len(totPages[[i]]),'.htm'))
URLall = unlist(URLall)

### Pull the individual elements into a dataframe
GD_Scraper = function(x=1:100) {map_df(x, function(i) {
  cat(' P', i, sep = '')

  pg = read_html(GET(URLall[i]))
  
  jobTitle = pg %>% html_nodes('#MainCol li.jl')  %>% html_node('a.jobInfoItem.jobTitle') %>% 
    html_text() %>% data.frame(Job_Title = ., stringsAsFactors = F)
  
  company = pg %>% html_nodes('#MainCol li.jl')  %>% html_node('div div a span') %>% 
     html_text() %>% data.frame(Company_Name = ., stringsAsFactors = F)
  
  cityState = pg %>% html_nodes('#MainCol li.jl')  %>% html_node('span.loc') %>% 
    html_text() %>% data.frame(City_State = ., stringsAsFactors = F)
  
  rating = pg %>% html_nodes('#MainCol li.jl')  %>% html_node('span.compactStars') %>% 
    html_text() %>% data.frame(Company_Rating = ., stringsAsFactors = F)
  
  salary = pg %>% html_nodes('#MainCol li.jl')  %>% html_node('span.gray.salary') %>% 
    html_text() %>% data.frame(salary_Range = ., stringsAsFactors = F)
  
  jobage = pg %>% html_nodes('#MainCol li.jl')  %>% html_node('div.d-flex.align-items-end.pl-std.css-mi55ob') %>% 
    html_text() %>% data.frame(Post_Date = ., stringsAsFactors = F)
  
  data.raw = cbind(jobTitle, company, cityState, rating, salary, jobage)
})}

### Do the pings as a batch to prevent errors
data.raw1 = GD_Scraper(1:100)
data.raw2 = GD_Scraper(101:200)
data.raw3 = GD_Scraper(201:length(URLall))


data.total = data.raw1 %>% 
  rbind(data.raw2,data.raw3)

### Export the data frame to csv
write.csv(data.total,"E:\\Data Science\\01. Projects\\Salary_Estimator\\R\\Glassdoor_JobData.csv", row.names = FALSE)

rm(data.raw1,data.raw2,data.raw3)
