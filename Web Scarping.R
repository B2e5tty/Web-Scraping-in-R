install.packages("httr")
install.packages("rvest")

library(httr)
library(rvest)

#Task 1
get_wiki_covid19_page <- function(){
  wiki_base_url <- "http://en.wikipedia.org/w/index.php"
  query_para <- list(title = "Template:COVID-19_testing_by_country")
  response <- GET(wiki_base_url,query=query_para)
  return(response)
}
get_wiki_covid19_page()

#Task 2
root_node <- read_html(get_wiki_covid19_page())
table_node <- html_node(root_node,'table')
table_frame <- html_table(table_node)
table_frame
summary(table_frame)

#Task 3
preprocess_covid_data_frame <- function(data_frame){
  shape <- dim(data_frame)
  data_frame <- data_frame[!(data_frame$'Country or region' == 'World'),]
  data_frame <- data_frame[1:172, ]
  data_frame["Ref."] <- NULL
  data_frame["Units[b]"] <- NULL
  
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$'confirmed.tested.ratio'))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$'tested.population.ratio'))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$'confirmed.population.ratio'))
  
  return(data_frame)
}

covid_frame <- preprocess_covid_data_frame(table_frame)
summary(covid_frame)
write.csv(covid_frame,file = "C:/Users/Samsung/Documents/Coursera/Final Project/covid_frame.csv",row.names = FALSE)

#Task 4
covid = read.csv("C:/Users/Samsung/Documents/Coursera/Final Project/covid_frame.csv")
covid_fifth_to_tenth <- covid[5:10, c("country",'confirmed')]  
covid_fifth_to_tenth

#Task 5
total_confirmed = sum(covid$confirmed)
total_tested = sum(covid$tested)
positive_ratio = total_confirmed / total_tested
positive_ratio
round(positive_ratio, digits = 3)

#Task 6
covid_country_factor = covid$country
class(covid_country_factor)
covid_country = as.character(covid_country_factor)
covid_country_atoz = sort(covid_country)
covid_country_ztoa = sort(covid_country, decreasing = TRUE)
covid_country_ztoa 

#Task 7
if_match_exist = regexpr('United.+',covid$country)
match_string = regmatches(covid$country,if_match_exist)
match_string

#Task 8
country_one = covid[1, c("country",'confirmed','confirmed.population.ratio')]
country_two = covid[2, c("country",'confirmed','confirmed.population.ratio')]
country_one
country_two

#Task 9
if(country_one$confirmed.population.ratio > country_two$confirmed.population.ratio){
  print(paste(country_one$country,'has higher COVID-19 infection risk'," "))
} else{
  print(paste(country_two$country,'has higher COVID-19 infection risk'," "))
}

#Task 10
all_countries = covid[, c('country','confirmed.population.ratio')]
countries_frame <- data.frame()
for(x in all_countries$confirmed.population.ratio){
  if(x < 0.01){
      countries_frame <- rbind(countries_frame,all_countries[(all_countries$confirmed.population.ratio)== x,])
    }
}
countries_frame



