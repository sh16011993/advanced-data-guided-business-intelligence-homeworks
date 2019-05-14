library(caret)
require(binr)
library(dummy)
library(readxl)
library(dplyr)
library(reshape)
library(reshape2)
library(qcc)

# Setting seed so that output is constant
set.seed(123)

# Reading excel file
df <- read_excel('eBayAuctions.xls')
print(df)

# Melting and Casting the Data to reduce the number of different values in 
# Categorical Variables
melted_df <- melt(df, id=c("Category", "currency", "Duration", "endDay"), "Competitive?")
print(melted_df)
casted_Category_df <- cast(melted_df, Category~variable, mean)
casted_currency_df <- cast(melted_df, currency~variable, mean)
casted_Duration_df <- cast(melted_df, Duration~variable, mean)
casted_endDay_df <- cast(melted_df, endDay~variable, mean)
print(casted_Category_df)
print(casted_currency_df)
print(casted_Duration_df)
print(casted_endDay_df)

# Reducing the number of Dummy Variables
for (i in 1:nrow(df)){
	# For Category
	if(df[i, "Category"] %in% c("Automotive", "Coins/Stamps", "EverythingElse", "Health/Beauty", "Jewelry", "Pottery/Glass")){
		df[i, "Category"] <- "Coins/Stamps"	
	}else if(df[i, "Category"] %in% c("Antique/Art/Craft", "Books", "Clothing/Accessories", "Collectibles", "Music/Movie/Game", "Toys/Hobbies")){
		df[i, "Category"] <- "Clothing/Accessories"
	}else{
		df[i, "Category"] <- "Photography"
	}
	# For Currency, by observation, EUP and US have almost similar means
	# Hence, replacing the EURO with US currency	
	if(df[i, "currency"] %in% c("EUR")){
		df[i, "currency"] <- "US"
	}
	# For Duration
	if(df[i, "Duration"] %in% c("3", "7")){
		df[i, "Duration"] <- "3"	
	}
	else{
		df[i, "Duration"] <- "1"	
	}
	# For endDay
	if(df[i, "endDay"] %in% c("Fri", "Sat", "Sun", "Tue", "Wed")){
		df[i, "endDay"] <- "Sat"
	}
	else{
		df[i, "endDay"] <- "Thu"
	}
}

# Creating dummy Variables
df <- cbind(df, dummy(df["Category"]))
df <- cbind(df, dummy(df["currency"]))
df <- cbind(df, dummy(df["Duration"]))
df <- cbind(df, dummy(df["endDay"]))
print(df)

# Splitting the data into 60 to 40 ratio
classes <- paste(df$Category, df$currency, df$Duration, df$endDay)
print(classes)
train.indexes <- createDataPartition(classes, p = .6, list = FALSE)
print(nrow(df))
print(nrow(train.indexes))
train <- df[train.indexes,]
test  <- df[-train.indexes,]
print(train)
print(test)

## Removing the original Categorical Columns
train$Category = NULL
train$currency = NULL
train$Duration = NULL
train$endDay = NULL

test$Category = NULL
test$currency = NULL
test$Duration = NULL
test$endDay = NULL

## Now data is ready for fitting the model
#### Q1
fit.all <- glm(`Competitive?` ~ ., family = binomial(link='logit'), data = train)
summary(fit.all)
# The best predictor variable is Category_Coins.Stamps
fit.single <- glm(`Competitive?` ~ Category_Coins.Stamps, family = binomial(link='logit'), data = train)
summary(fit.single)

#### Q2
# The top four predictor variables are Category_Coins.Stamps, endDay_Sat, currency_GBP, Duration_1
# In PDF

#### Q3
# In PDF

#### Q4
fit.reduced <- glm(`Competitive?` ~ ClosePrice + OpenPrice + endDay_Sat + Category_Coins.Stamps + sellerRating, family = binomial(link='logit'), data = train)
summary(fit.reduced)
## Comparing the reduced and the full model
anova(fit.reduced, fit.all, test="Chisq")

#### Q5
size <- rep(2, length(train["Competitive?"]))
qcc.overdispersion.test(train["Competitive?"], size, type = "binomial")
