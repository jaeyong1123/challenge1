install.packages("rvest")
install.packages("stringr")  
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")

library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)

######################################
#Run a housing search from Zillow
######################################
#Zillow is a web-based, leading real estate information service in the United States. I collect data from Zillow to analyze how to determine house prices using hedonic pricing model.
#I choose 1+ bedrooms, any bathrooms, and only single-family houses in Duluth, GA. 
#This is the url of my search below:
#Zillow link:https://www.zillow.com/homes/duluth,-ga_rb/

#############################
#Functions
#############################
#Define functions to get price, details, address, types of transaction
get_price <- function(html){
  html %>%
    html_nodes('.list-card-price') %>%
    html_text() %>%
    str_trim()
}

get_details <- function(html){
  html %>%
    html_nodes('.list-card-details') %>%
    html_text() %>%
    str_trim()
}

get_address <- function(html){
  html %>%
    html_nodes('.list-card-addr') %>%
    html_text() %>%
    str_trim()
}

get_type<- function(html){
  html %>%
    html_nodes('.list-card-type') %>%
    html_text() %>%
    str_trim()
}

#############################
#Scrape the webpages from 1 to 6
#############################
#There are 237 results spread over 6 pages.
#However, listings are changed every second with new listings or removal.
#Also, a different location has a diffrent number of results and pages. 
#Therefore, I create the general code that can apply to different locations and pages. 

num_list <- 40 # Each page includes 40 listings. 
first_pg <- read_html("https://www.zillow.com/duluth-ga/houses/1-_beds/?searchQueryState={%22pagination%22:{},%22usersSearchTerm%22:%22duluth,%20ga%22,%22mapBounds%22:{%22west%22:-84.33665031103516,%22east%22:-83.93496268896484,%22south%22:33.891083606886006,%22north%22:34.122182758016194},%22regionSelection%22:[{%22regionId%22:51757,%22regionType%22:6}],%22isMapVisible%22:true,%22mapZoom%22:12,%22filterState%22:{%22beds%22:{%22min%22:1},%22con%22:{%22value%22:false},%22apa%22:{%22value%22:false},%22mf%22:{%22value%22:false},%22land%22:{%22value%22:false},%22tow%22:{%22value%22:false},%22manu%22:{%22value%22:false}},%22isListVisible%22:true}")

get_total_list <- function(html){
  html %>%
    html_nodes('.result-count') %>%
    html_text() %>%
    str_remove(' results') %>%
    str_trim()
}

total_list <- get_total_list(first_pg)
total_list <-as.numeric(str_replace_all(total_list,"[^0-9]*",""))
no_page <- ceiling(total_list / num_list) # Find the number of pages

price<-c()
details<-c()
address<-c()
type<-c()

#Scrape the search results from the first page to the last page 
for (i in 1:no_page){
  if (i==1){
    zillow_pages <- "https://www.zillow.com/duluth-ga/houses/1-_beds/?searchQueryState={%22pagination%22:{},%22usersSearchTerm%22:%22duluth,%20ga%22,%22mapBounds%22:{%22west%22:-84.33665031103516,%22east%22:-83.93496268896484,%22south%22:33.891083606886006,%22north%22:34.122182758016194},%22regionSelection%22:[{%22regionId%22:51757,%22regionType%22:6}],%22isMapVisible%22:true,%22mapZoom%22:12,%22filterState%22:{%22beds%22:{%22min%22:1},%22con%22:{%22value%22:false},%22apa%22:{%22value%22:false},%22mf%22:{%22value%22:false},%22land%22:{%22value%22:false},%22tow%22:{%22value%22:false},%22manu%22:{%22value%22:false}},%22isListVisible%22:true}"
  }
  else{
    zillow_pages <- paste("https://www.zillow.com/duluth-ga/houses/1-_beds/",i,"_p/?searchQueryState={%22pagination%22:{%22currentPage%22:",i,"},%22usersSearchTerm%22:%22duluth,%20ga%22,%22mapBounds%22:{%22west%22:-84.33665031103516,%22east%22:-83.93496268896484,%22south%22:33.891083606886006,%22north%22:34.122182758016194},%22regionSelection%22:[{%22regionId%22:51757,%22regionType%22:6}],%22isMapVisible%22:true,%22mapZoom%22:12,%22filterState%22:{%22beds%22:{%22min%22:1},%22con%22:{%22value%22:false},%22apa%22:{%22value%22:false},%22mf%22:{%22value%22:false},%22land%22:{%22value%22:false},%22tow%22:{%22value%22:false},%22manu%22:{%22value%22:false}},%22isListVisible%22:true}",sep = "")
  }
  
  zillow_data<- read_html(zillow_pages)
  price <- c(price, get_price(zillow_data))
  details<- c(details,get_details(zillow_data))
  address <- c(address,get_address(zillow_data))
  type <- c(type, get_type(zillow_data))
}

#Check the length of the results
length(price); length(details); length(address); length(type)

# Obtain the raw dataset using the fuctions aboves
zillow_df <- data.frame(matrix(nrow=total_list, ncol=0))

#############################
# Conduct some data cleaning
#############################
zillow_df <- zillow_df %>%
  mutate(bedrooms = as.integer(str_trim(str_extract(details, "[\\d ]*(?=bd)")))) %>%
  mutate(bathrooms = as.integer(str_trim(str_extract(details, "[\\d ]*(?=ba)")))) %>%
  mutate(sqft = str_trim(str_extract(details, "[\\d ,]*(?=sqft)"))) %>%
  mutate(sqft = as.numeric(str_replace(sqft,",",""))) %>%
  mutate(price = as.numeric(str_replace_all(price,"[^0-9]*",""))) 

zillow_df$ type <- type

head(zillow_df)

#Create the new dataset w/o missing values
zillow_df <- na.omit(zillow_df)

nrow(zillow_df) # My final dataset has 228 observations. 

#############################
# Visual analysis
#############################
#Boxplot
#Below boxplot detects the outliers in the distribution of house prices. 
#There are outliers above the third quartiles of the distribution.
#I do not remove those outliers for this analysis.

boxplot(zillow_df$price,main="Boxplot of House Prices", 
        ylab = "House Price Listed ($)", yaxt = "n", 
        col = "yellow", medcol = "red", boxlty = 0, axes=FALSE,
        whisklty = 1,  staplelwd = 4, outpch = 8, outcex = 1)
axis(2, at = seq(0, max(zillow_df$price), 50000), las = 2, cex.axis=0.5)

#Scatter plot between price and square footage - Positive relationship
#We also find that there are some outliers from the scatterplot.
g1<-ggplot(zillow_df, aes(y=price, x=sqft, color=as.factor(bedrooms))) +
  geom_point() +
  labs(y="Price", x="Square Footage",
       title="Scatter plot between price and square footage")
g1 + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Boxplot between price and type of transasction - majority of types is House for sale
g2<-ggplot(zillow_df, aes(type, price))+
  geom_boxplot(outlier.color="red") +
  labs(x="Type", y="Price")

g2 + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#I create a dummy variable for foreclosure to examine the negative effect of foreclosure on house prices. 

zillow_df$foreclosure <- as.character(zillow_df$type)
zillow_df$foreclosure[zillow_df$foreclosure == "House for sale"] <- 0
zillow_df$foreclosure[zillow_df$foreclosure == "For sale by owner"] <- 0
zillow_df$foreclosure[zillow_df$foreclosure == "New construction"] <- 0
zillow_df$foreclosure[zillow_df$foreclosure == "Pre-foreclosure / Auction"] <- 1
zillow_df$foreclosure <- as.numeric(zillow_df$foreclosure)

summary(zillow_df)

##########################
# Regression Analysis
##########################
#Hedonic pricing model - linear-linear model
model <- lm(price ~ sqft+bedrooms+bathrooms+foreclosure, zillow_df)
summary(model)
model$coefficients

#Goodness of Fit
#R-squared is 0.794 (79.4%). It means that the 79.4% of the variation in house prices is explained by the independent variables in the model. 

#"sqft", "bedrooms", and "bathrooms" are statistically significant at 1% level. 
#However, "bedrooms" has a negative sign which I did not expect to have because I control for square footage.
#More rooms dividing up the same sapce is not necessarily more valuable here. 
#"foreclosure" has a negative sign which I expected to have, but it is not statistically significant. 

#On average, an additional bedroom in a house decreases the price of house by $148,733.
#On average, an additional bathroom in a house increases the price of house by $84,472.
#On average, one unit increase in sqft increases the price of house by $199.


#Hedonic pricing model - log-linear model
#Hedonic pricing model often uses log-linear model because of convienent interpretation. 
logmodel <- lm(log(price)~ sqft+bedrooms+bathrooms+foreclosure, zillow_df)
summary(logmodel)
logmodel$coefficients

#R-squared for log-linear model is 91.1% which is much better than linear model. 
#It is because "price" variable becomes normal with transformation. 
