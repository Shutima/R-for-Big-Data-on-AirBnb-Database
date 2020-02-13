
load('AirBnB.Rdata')
summary(L)
View(L)

# Changing the price columns from character to numeric by removing $ and consider the decimal point
L$price <- as.numeric(gsub('\\$|,', '', L$price))
L$weekly_price <- as.numeric(gsub('\\$|,', '', L$weekly_price))
L$monthly_price <- as.numeric(gsub('\\$|,', '', L$monthly_price))
#L$price
#L$weekly_price
#L$monthly_price

# Check missing values with below command in R console
# colSums(is.na(L))
# The result shows that column neighbourhood_group_cleansed and has_availability are completely empty therefore we drop these 2 columns
L$neighbourhood_group_cleansed <- NULL
L$has_availability <- NULL

# Replace missing numerical values with the column median
nums <- unlist(lapply(L, is.numeric))
for (nm in names(nums)) {
  #print(nm)
  #print(nums[[nm]])
  if (nums [[nm]]) {
    myna <- is.na(L[[nm]])
    mycol <- L[[nm]]
    mycol[myna] <- median(L[[nm]], na.rm = TRUE)
    L[[nm]] <- mycol
  }
}

# Recheck for missing values
colSums(is.na(L))

# Loading ggplot
library(ggplot2)
hist(L$price, breaks = 50, freq = FALSE)

library(tidyverse)
p <- L %>%
  group_by(neighbourhood) %>%
  summarize(price = mean(price)) %>%
  ggplot(aes(y = price, x = neighbourhood)) + 
  geom_col() +
  theme(axis.text.x = element_text(size = 8, angle = 90)) +
  ggtitle("Average Apt Price per Neighbourhood")
p

# Create new data frame with columns Price and Room Type

tmpRT<-data.frame(L$room_type, L$price)

tmpRT_new <- aggregate(x = tmpRT$L.price, by = list(tmpRT$L.room_type), function(x) mean(x))

p2 <- tmpRT_new %>%
  group_by(Group.1) %>%
  summarize(x = mean(x)) %>%
  ggplot(aes(y = x, x = Group.1)) + 
  geom_col() +
  theme(axis.text.x = element_text(size = 8, angle = 90)) + ylab("Price") + xlab("Room Type") +
  ggtitle("Average Apt Price per Room Type")
p2

# Show relationship between apartment price and the bed type using ggplot
# Create new data frame with columns Price and Bed Type
tmpBT<-data.frame(L$bed_type, L$price)

tmpBT_new <- aggregate(x = tmpBT$L.price, by = list(tmpBT$L.bed_type), function(x) mean(x))

p3 <- tmpBT_new %>%
  group_by(Group.1) %>%
  summarize(x = mean(x)) %>%
  ggplot(aes(y = x, x = Group.1)) + 
  geom_col() +
  theme(axis.text.x = element_text(size = 8, angle = 90)) + ylab("Price") + xlab("Bed Type") +
  ggtitle("Average Apt Price per Bed Type")
p3

count <- table(L$host_total_listings_count)
countHN <- table(L$host_name)
barplot(countHN, count, main = "Number of Apt per owner", xlab = "Host Name", ylab = "Host Total Listings count") 

# Create new data frame with columns Price and Zipcode

tmpAR<-data.frame(L$zipcode, L$price)

tmpAR_new <- aggregate(x = tmpAR$L.price, by = list(tmpAR$L.zipcode), function(x) mean(x))

p4 <- tmpAR_new %>%
  group_by(Group.1) %>%
  summarize(x = mean(x)) %>%
  ggplot(aes(y = x, x = Group.1)) + 
  geom_col() +
  theme(axis.text.x = element_text(size = 8, angle = 90)) + ylab("Price") + xlab("Arrondissement") +
  ggtitle("Average Apt Price per Arrondissement")
p4

p5 <- L %>%
  group_by(zipcode) %>%
  summarize(reviews_per_month = mean(reviews_per_month)) %>%
  ggplot(aes(y = reviews_per_month, x = zipcode)) + 
  geom_col() +
  theme(axis.text.x = element_text(size = 8, angle = 90)) + xlab("Arrondissement") +
  ggtitle("Visit Frequency of the Different Quarters according to Time")
p5


