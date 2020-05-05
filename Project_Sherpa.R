# Run these three functions to get a clean test of code
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!



setwd("C:/Users/sonam/Desktop/Rcodes/Rcodes")


library(RCurl)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(imputeTS)
library(kernlab)
library(arules)
library(arulesViz)
library(ggplot2)
airURL <- "https://s3.us-east-1.amazonaws.com/blackboard.learn.xythos.prod/5956621d575cd/9644551?response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27Spring2020-survey-02%25281%2529.json&response-content-type=application%2Fjson&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20200501T205644Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAIL7WQYDOOHAZJGWQ%2F20200501%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=5c53e1a7e18680a05c4cffba895cb45f7bef9ef628c4f43ad8fe70ba1a14b21f"
apiresult<- getURL(airURL)
results <- fromJSON(apiresult)

head(results)
str(results)
view(results)


colSums(is.na(results))
sum(is.na(results))

###### Phase 01 - DATA MUNGING ##############################333

sum(is.na(results))

## Regular expressions to remove statename initials from the column names - destination city and state city
results$Destination.City <- gsub("(.*),.*", "\\1",results$Destination.City)
results$Origin.City <- gsub("(.*),.*", "\\1",results$Origin.City)

## Now we extract month from flight dates and create a new column to show months
results$Flight.date <- as.Date(results$Flight.date, format = "%m/%d/%y") # convert flight date from chr to date
library(data.table)
setDT(results)[, Flight_Month := format(as.Date(Flight.date), "%m") ]
results$Flight_Month <- as.numeric(results$Flight_Month)
# We now have a new column for months and we can do analysis on a monthly basis
table(results$Flight_Month)

## dealing with numeric missing values with interpolation
library(imputeTS)
results$Loyalty <- na_interpolation(results$Loyalty)





colSums(is.na(results))
sum(is.na(results))
table(results$Gender)
##  dealing missing values in necesesary categorical attributes except column 'free text'
 results <- results%>%
   mutate(Destination.City = replace(Destination.City, is.na(Destination.City), "N/A"))

 results <- results%>%
   mutate(Origin.City = replace(Origin.City, is.na(Origin.City), "N/A"))
 
 
 results <- results%>%
   mutate(Partner.Code = replace(Partner.Code, is.na(Partner.Code), "N/A"))
 
 results <- results%>%
   mutate(Partner.Name = replace(Partner.Name, is.na(Partner.Name), "N/A"))
 
 results <- results%>%
   mutate(Origin.State = replace(Origin.State, is.na(Origin.State), "N/A"))
 
 results <- results%>%
   mutate(Destination.State = replace(Destination.State,is.na(Destination.State), "N/A"))
 
results <- results%>%
   mutate(Flight.cancelled = replace(Flight.cancelled, is.na(Flight.cancelled), "N/A"))
 
 results <- results%>%
   mutate(freeText = replace(freeText, is.na(freeText), "N/A"))
 
 results <- results%>%
   mutate(Flight.cancelled = replace(Flight.cancelled, is.na(Flight.cancelled), "No")) 
 



 

#To check number of missing values in entire dataset
sum(is.na(results))
colSums(is.na(results))
results <- na.omit(results)
sum(is.na(results))


# 0 missing values in the dataset
# Previously, 11435 missing values were present.
# Therefore, 100% of missing values have been dealt with.

## FINAL

# The Destination City and the Origin city columns have been cleaned using gsub to replace state name initials eg:- CO,NY,CA
# All the numeric missing values have been interpolated
# All the missing values in the categorical variables for the regression model are replaced with "unavailable" Or required values
# The data type are mostly chr and num. The flight date column has been cconverted from chr to date
# A new Flight_month column has been created. Since, the dataset is of the year 2014 only and analysis can be done from months

# In the above code, all the necessary categorical variables have been encoded, missing numerical values have been interpolated
# and this attribute is not included in the linear regression model:-
# 1.) freetext - due to large number of missing values


############## DATA TRANSFORMATIONS#####################################
str(results)


## Now we encode the categorical variables into a factor format for application in Predictive models

results<- results %>%
  mutate(Airline.Status = as.factor(Airline.Status),Gender = as.factor(Gender), Flight.cancelled= as.factor(Flight.cancelled),
         Type.of.Travel = as.factor(Type.of.Travel),Class = as.factor(Class),Price.Sensitivity = as.factor(Price.Sensitivity),
                      Partner.Name = as.factor(Partner.Name),Flight_Month = as.factor(Flight_Month))
str(results)

#Create age group and nps group

results <- results %>% mutate(agegroup = case_when(Age >= 60  & Age <= 85 ~ 'Senior Citizens',
                                                         Age >= 31  & Age <= 59 ~ 'Middle Aged',
                                                         Age >= 15  & Age <= 30 ~ 'Young'))
results <- results %>% mutate(npsgroup = case_when(Likelihood.to.recommend >= 9 ~ 'Promoter',
                                                         Likelihood.to.recommend >= 7  & Likelihood.to.recommend <= 8 ~ 'Neutral',
                                                         Likelihood.to.recommend >= 0  & Likelihood.to.recommend <= 6 ~ 'Detractor'))









####### phase 2 (VISUALIZATIONS)############################################################
str(results)

# numerical variables(Histograms)

#1
hist_age <- ggplot(results,aes(x=Age)) +
  geom_histogram(binwidth = 10,color="black", fill = "white") +
  ggtitle("NPS Histogram")
hist_age

#2
hist(results$Loyalty)
g_da <- filter(results,Loyalty == 1)
table(g_da$npsgroup)



#3
hist(results$Scheduled.Departure.Hour)
hist(results$Flight.time.in.minutes)
mean(results$Flight.time.in.minutes)
median(results$Flight.Distance)
hist(results$Flight.Distance)

#4
hist_NPS <- ggplot(results,aes(x=Likelihood.to.recommend)) +
  geom_histogram(binwidth = 1,color="black", fill = "white") +
  ggtitle("NPS Histogram")
hist_NPS



# factor variables(data tables)

table(results$Price.Sensitivity)
table(results$Gender)
table(results$Type.of.Travel)
table(results$Class) 
table(results$Partner.Code)


############## # Box Plot Visualisations
summary(results)

ggplot(data=results)+
  geom_boxplot(mapping = aes(x=Type.of.Travel,y=Likelihood.to.recommend))
table(results$Type.of.Travel)
# Here we can see that Passengers who travel for Personal reasons are the most dissatisfied and 50% of 
# customers rate it less than 5(around 1550 passengers).

ggplot(data=results)+
  geom_boxplot(mapping = aes(x=Price.Sensitivity,y=Likelihood.to.recommend))
table(results$Price.Sensitivity)


##### EXPLORATORY DATA ANALYSIS

# Split into data frames according to nps scores 
low_nps <- filter(results,Likelihood.to.recommend <= 6)
mid_nps <- filter(results,Likelihood.to.recommend <= 8 & Likelihood.to.recommend >=7)
high_nps <- filter(results,Likelihood.to.recommend <= 10 & Likelihood.to.recommend >=9)



## 1.) Ridge Line Plot
library(ggridges)

plotdata <- filter(results, Loyalty==1)
table(plotdata$npsgroup)

ggplot(plotdata, 
       aes(x = Likelihood.to.recommend, 
           y = Airline.Status, 
           fill = Airline.Status)) +
  geom_density_ridges() + 
  theme_ridges() +
  labs("Highway mileage by auto class") +
  theme(legend.position = "none")

##2.)

ggplot(results, 
       aes(x = Gender, 
           fill = npsgroup)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
# less number of female promoters, more female detractors. Therefore, females prefer less


#3.)
ggplot(results, 
       aes(x = Flights.Per.Year, 
           y = Loyalty)) +
  geom_point(color = "steelblue") +
  geom_smooth(mehod ="lm")
# As flights per year decreases, loyalty decreases. Therefore, frequent flyers are less loyal to the airlines, while
# people who fly less frequently in a year prefer Southeast airlines

#4.)calculate mean nps for each airline status

Status_viz <- results %>%
  group_by(Airline.Status) %>%
  summarize(mean_NPS = mean(Likelihood.to.recommend))

ggplot(Status_viz, 
       aes(x = Airline.Status, 
           y = mean_NPS)) +
  geom_bar(stat = "identity")
# The mean NPS of passengers with Blue airline status provide less scores

#5.)calculate mean nps for each type of travel
TOT_viz <-  results %>%
  group_by(Type.of.Travel) %>%
  summarize(mean_NPS = mean(Likelihood.to.recommend))

ggplot(TOT_viz, 
       aes(x = Type.of.Travel, 
           y = mean_NPS)) +
  geom_bar(stat = "identity")
# The mean NPS of paseengers who travel for personal reasons provide less scores

#6.)
newest_data <- filter(results,Airline.Status == "Blue" )
table(newest_data$Gender)
# 3970 females/2849 male 
newest2_data <- filter(newest_data,Type.of.Travel == "Personal Travel")
table(newest2_data$Gender)
# 1555 females / 767 male


#5.)faceting

ggplot(results, aes(x = Likelihood.to.recommend)) +
  geom_histogram(color = "white",
                 fill = "cornflowerblue") +
  facet_grid(Class ~ Type.of.Travel) +
  labs(title = "NPS histograms by type of travel and Class",
       x = "NPS Score(1-10)")
# facet histogram determining highest travellers in economy class travelling for business purposes
# Using the two above viz, we notice that most number of eco passengers travel for business reasons and
# the distribution is left skewed. Most of the passengers give positive reviews.
# Hence, this can be focused

#6.)  viz on partner airlines
partnerdata <- results %>%
  count(Partner.Name) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

ggplot(partnerdata, 
       aes(x = reorder(Partner.Name, -pct), 
           y = pct)) + 
  geom_bar(stat = "identity",
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  labs(x = "Partner name", 
       y = "Frequency", 
       title  = "Customers by Partner Name") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))
# Cheapseats Airline has the maximum number of passengers(21%) followed by Sigma Airlines(16%)
# West airways has lowest number of passengers(12 passengers)
















##############        PHASE 3           ################################################
###### Splitting dataset into training and test data



reg_results <- results[-c(1,2,16,18:20,28:32)] # dropping all categorical variables from dataset to build linear model

library(caTools)
set.seed(131)
split = sample.split(reg_results$Likelihood.to.recommend,SplitRatio = 0.8)
training_set = subset(reg_results,split==TRUE)
test_set = subset(reg_results,split==FALSE)

##### Scale numeric variables

#raining_set[,c(4,8,9,12,13,21,22,23,25,26,27)] <- scale(training_set[,c(4,8,9,12,13,21,22,23,25,26,27)])
#est_set[,c(4,8,9,12,13,21,22,23,25,26,27)] <- scale(test_set[,c(4,8,9,12,13,21,22,23,25,26,27)])


###### Applying Linear Regression

# Here we use a process a backward elimination, where we consider all the variables in the training dataset. 
# 1.) Select Model Significance 0.05
# 2.) fit the full model with all possible predictors
# 3.) Consider the predictor with highes p-value. If, P > Significance level,go to step 4, otherwise model is ready
# 4.) Remove the predictor variable with the highest P-value
# 5.) Fit model without this variable. Return back to step 3 if model is not ready
str(results)
#flight.canceled removed due to error in regression model
regressor = lm(formula= Likelihood.to.recommend ~ Airline.Status + Age + Gender + Price.Sensitivity + Year.of.First.Flight +
                 Flights.Per.Year + Loyalty + Type.of.Travel + Total.Freq.Flyer.Accts + Shopping.Amount.at.Airport + Class + 
                 Eating.and.Drinking.at.Airport  + Day.of.Month + Scheduled.Departure.Hour + Departure.Delay.in.Minutes+ 
                 Arrival.Delay.in.Minutes  + Flight.time.in.minutes + Flight.Distance,
               data = training_set)
summary(regressor)
# remove price.sensitivity

regressor = lm(formula= Likelihood.to.recommend ~ Airline.Status + Age + Gender  + Year.of.First.Flight +
                 Flights.Per.Year + Loyalty + Type.of.Travel + Total.Freq.Flyer.Accts + Shopping.Amount.at.Airport + Class + 
                 Eating.and.Drinking.at.Airport  + Day.of.Month + Scheduled.Departure.Hour + Departure.Delay.in.Minutes+ 
                 Arrival.Delay.in.Minutes  + Flight.time.in.minutes + Flight.Distance,
               data = training_set)
summary(regressor)
# remove scheduled departure hour

regressor = lm(formula= Likelihood.to.recommend ~ Airline.Status + Age + Gender  + Year.of.First.Flight +
                 Flights.Per.Year + Loyalty + Type.of.Travel + Total.Freq.Flyer.Accts + Shopping.Amount.at.Airport + Class + 
                 Eating.and.Drinking.at.Airport  + Day.of.Month  + Departure.Delay.in.Minutes+ 
                 Arrival.Delay.in.Minutes  + Flight.time.in.minutes + Flight.Distance,
               data = training_set)
summary(regressor)
# remove shopping amount at airport

regressor = lm(formula= Likelihood.to.recommend ~ Airline.Status + Age + Gender  + Year.of.First.Flight +
                 Flights.Per.Year + Loyalty + Type.of.Travel + Total.Freq.Flyer.Accts  + Class + 
                 Eating.and.Drinking.at.Airport  + Day.of.Month  + Departure.Delay.in.Minutes+ 
                 Arrival.Delay.in.Minutes  + Flight.time.in.minutes + Flight.Distance,
               data = training_set)
summary(regressor)
#remove total freq flyer accounts

regressor = lm(formula= Likelihood.to.recommend ~ Airline.Status + Age + Gender  + Year.of.First.Flight +
                 Flights.Per.Year + Loyalty + Type.of.Travel   + Class + 
                 Eating.and.Drinking.at.Airport  + Day.of.Month  + Departure.Delay.in.Minutes+ 
                 Arrival.Delay.in.Minutes  + Flight.time.in.minutes + Flight.Distance,
               data = training_set)
summary(regressor)
# remove loyalty

regressor = lm(formula= Likelihood.to.recommend ~ Airline.Status + Age + Gender  + Year.of.First.Flight +
                 Flights.Per.Year  + Type.of.Travel   + Class + 
                 Eating.and.Drinking.at.Airport  + Day.of.Month  + Departure.Delay.in.Minutes+ 
                 Arrival.Delay.in.Minutes  + Flight.time.in.minutes + Flight.Distance,
               data = training_set)
summary(regressor)
# remove day of month

regressor = lm(formula= Likelihood.to.recommend ~ Airline.Status + Age + Gender  + Year.of.First.Flight +
                 Flights.Per.Year  + Type.of.Travel   + Class + 
                 Eating.and.Drinking.at.Airport    + Departure.Delay.in.Minutes+ 
                 Arrival.Delay.in.Minutes  + Flight.time.in.minutes + Flight.Distance,
               data = training_set)
summary(regressor)
#remove departure delay in minutes

regressor = lm(formula= Likelihood.to.recommend ~ Airline.Status + Age + Gender  + Year.of.First.Flight +
                 Flights.Per.Year  + Type.of.Travel   + Class + 
                 Eating.and.Drinking.at.Airport    + 
                 Arrival.Delay.in.Minutes  + Flight.time.in.minutes + Flight.Distance,
               data = training_set)
summary(regressor)
# remove year of first flight

regressor = lm(formula= Likelihood.to.recommend ~ Airline.Status + Age + Gender   +
                 Flights.Per.Year  + Type.of.Travel   + Class + 
                 Eating.and.Drinking.at.Airport    + 
                 Arrival.Delay.in.Minutes  + Flight.time.in.minutes + Flight.Distance,
               data = training_set)
summary(regressor)
# The final model








# All predictor variables have achieved a p value of less than the significant value(0.05) with
# an adjusted R- square value of 0.3868 (38.68%)






# predicting NPS
y_pred <- predict(regressor,newdata = test_set)
y_pred <- as.data.frame(y_pred)

actuals_preds <- data.frame(cbind(actuals=test_set$Likelihood.to.recommend, y_pred=y_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 64.03%
correlation_accuracy
## We have built a predictive model to predict the NPS of customer from the test set with 64 % accuracy



##### APRIORI

#create new variable and filter out explanatory variables required for modelling
ap_results <- results
str(ap_results)
apriori_results <- ap_results[,-c(1,2,4,8,9,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33)]
str(apriori_results)


#Convert data type of desired variables to factor for sparse matrix, to calculate aprirori insights
apriori_results <- apriori_results %>%
  mutate(agegroup = as.factor(agegroup),npsgroup = as.factor(npsgroup),Year.of.First.Flight = as.factor(Year.of.First.Flight)
         ,Total.Freq.Flyer.Accts = as.factor(Total.Freq.Flyer.Accts))

str(apriori_results)

# Convert dataframe into transactions
apriori_resultsx <- as(apriori_results,"transactions")
itemFrequencyPlot(apriori_resultsx,topN =10)

# run model for promoters
magic <- apriori(apriori_resultsx, # initiates apriori library on magic
                 parameter = list(support = 0.005,confidence = 0.5), # sets the support and confidence as parameters
                 appearance = list(default='lhs',rhs=("npsgroup=Promoter"))) # # to find connections of passengers who are Promoters

inspectDT(sort(magic,by='lift')[1:10])
sub_magic = magic[quality(magic)$confidence>0.8]
plot(sub_magic,method = "paracoord")

# run model for detractors
magic2 <- apriori(apriori_resultsx, # initiates apriori library on magic
                 parameter = list(support = 0.005,confidence = 0.5), # sets the support and confidence as parameters
                 appearance = list(default='lhs',rhs=("npsgroup=Detractor"))) # # to find what kind of passengers survived

inspectDT(sort(magic2,by='lift')[1:10])
sub_magic2 = magic2[quality(magic2)$confidence>0.83]
plot(sub_magic2,method = "paracoord")


###################### SUPPORT VECTOR

# data preparation
sv_results <- results[,c(3,4,6,7,8,9,11:13,15,21:23,25,26,35)] # categorical variables are excluded
str(sv_results)

# Output variable is converted to factor
sv_results <- sv_results %>%
  mutate(npsgroup = as.factor(npsgroup))

str(sv_results)
## spliting into training and test set

randIndex <- sample(1:dim(sv_results)[1])
summary(randIndex)

length(randIndex)

cutpoint2_3 <- floor(2*dim(sv_results)[1]/3)
cutpoint2_3

str(sv_results)
train_data <- sv_results[randIndex[1:cutpoint2_3],]
test_data <- sv_results[randIndex[(cutpoint2_3+1):dim(sv_results)[1]],]

dim(train_data)
dim(test_data)



# train the model
svmOutput <- ksvm(npsgroup ~.,data=train_data,kernel="rbfdot",kpar="automatic",C=15,cross=3,prob.model=TRUE)
svmOutput

#predict values
svmPred <- predict(svmOutput,test_data)
#view(svmPred)

#confusion matrix
confmatrix <- table(test_data$npsgroup,svmPred)
confmatrix

#accuracy rate
accuracy_rate <- ((confmatrix[1,1] +confmatrix[2,2] + confmatrix[3,3])/(sum(confmatrix[1,]) +
                                        sum(confmatrix[2,])+sum(confmatrix[3,])))*100
accuracy_rate

#histogram to understand complex data
hist(alpha(svmOutput)[[1]])

###          SVM ######
#library(e1071)

#svm1 <- svm(npsgroup~., data=train_data, 
#           method="C-classification", kernal="radial", 
#           gamma=0.1, cost=10)
#summary(svm1)
#svm1$SV
#prediction <- predict(svm1, test_data)
#xtab <- table(test_data$npsgroup,prediction)
#xtab


#(442+362+981)/nrow(test_data)
# 0.53


###########################################PHASE 4 ######################################
# Plot flight routes for all likelihood to recommend scores below 2
flight_results <- results[results$Likelihood.to.recommend <= 2,]


usMap <- borders("state", colour="grey", fill="white")
ggplot() + usMap


allUSA <- ggplot() + usMap +
  geom_curve(data=flight_results,
             aes(x=olong, y=olat, xend=dlong, yend=dlat),
             col="#00008b",
             size=.5,
             curvature=0.2) +
  geom_point(data=flight_results,
             aes(x=olong, y=olat), 
             colour="blue",
             size=1.5) +
  geom_point(data=flight_results,
             aes(x=dlong, y=dlat), 
             colour="blue") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        plot.title=element_text(hjust=0.5, size=12)) +
  ggtitle("Route Map for NPS scores <= 2")

allUSA


############################################################## PHASE 05 ######################
# filter data with passengers having NPS<6(Detractors)
low_score_flight <- filter(results,Likelihood.to.recommend <= 6)
view(low_score_flight)
summary(low_score_flight)

# origin cities having lowest nps scores
flightdata <- low_score_flight %>%
  count(Origin.City) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

flightdata <- flightdata[with(flightdata,order(-pct)),]
flightdata <- flightdata[1:10,]

ggplot(flightdata, 
       aes(x = reorder(Origin.City, -pct), 
           y = pct)) + 
  geom_bar(stat = "identity",
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  labs(x = "Origin City", 
       y = "Frequency", 
       title  = "Customers by Origin City") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

# destination cities with lowest nps scores
flightdata2 <- low_score_flight %>%
  count(Destination.City) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

flightdata2 <- flightdata2[with(flightdata2,order(-pct)),]
flightdata2 <- flightdata2[1:10,]

ggplot(flightdata2, 
       aes(x = reorder(Destination.City, -pct), 
           y = pct)) + 
  geom_bar(stat = "identity",
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  labs(x = "Destination City", 
       y = "Frequency", 
       title  = "Customers by Destination City") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

########## Flights originating or destined to Chicago, Atlanta, Houston have the majority of most unsatisfied customers

#filter data with passengers only from top three flight origin cities where NPS are lowest amongst the majority

tri_city <- filter(low_score_flight,Origin.City == "Chicago" | Origin.City == "Atlanta" | Origin.City == "Houston")

# 1)
ggplot(tri_city, 
       aes(x = Likelihood.to.recommend, 
           y = Shopping.Amount.at.Airport)) +
  geom_bar(stat = "identity") 

ggplot(tri_city, 
       aes(x = Likelihood.to.recommend, 
           y = Eating.and.Drinking.at.Airport)) +
  geom_bar(stat = "identity")

# From the above two distributions customers who who rate higher nps scores (Range 4-6) spend more in airport
# Spending is on shopping or food and beverages.
# Therefore, customers who spend less in airport facilities tend to give low nps scores
# HIgher spending customers are likely to give higher NPS.


#2) Pie chart to show travel purpose for unsatisfied customer
tri_city2 <- tri_city %>%
  count(Type.of.Travel) %>%
  arrange(desc(Type.of.Travel)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

tri_city2$label <- paste0(tri_city2$Type.of.Travel, "\n",
                         round(tri_city2$prop), "%")

ggplot(tri_city2, 
       aes(x = "", 
           y = prop, 
           fill = Type.of.Travel)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Passengers by Travel Purpose")
# the most un satisfied customers travel for personal reasons.

# 3.)
# Analysis of passengers who travel for personal reasons
tri_city_personal <- filter(tri_city, Type.of.Travel == "Personal Travel")
table(tri_city_personal$Airline.Status)
# most of the passengers who travel for personal reasons have blue airline status

table(tri_city_personal$Gender)
# most of the passengers who travel for personal reason are females

table(tri_city_personal$Class)
# passengers travel in economy class mostly while travelling for personal reasons

hist(tri_city_personal$Loyalty)
# high number of passengers who are unsatisfied are disloyal customers are frequently customers to other airlines


#4)
#Analysis of Partner Airlines on tri_city data
tri_city_partner <- tri_city_personal %>%
  count(Partner.Name) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

ggplot(tri_city_partner, 
       aes(x = reorder(Partner.Name, -pct), 
           y = pct)) + 
  geom_bar(stat = "identity",
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  labs(x = "Partner Name", 
       y = "Frequency", 
       title  = "Customers by Partner Airlines") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

# Customers who travel with Flyfast(25% ) and Sigma(20%) for personal reasons are the most unsatisfied.

# Map depicting NPS scores by State
results$region <- tolower(results$Origin.State)
states_map <- map_data("state")
nps_map <- left_join(states_map, results, by = "region")

ggplot(nps_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Likelihood.to.recommend), color = "white")+
  scale_fill_viridis_c(option = "C")

ggplot(nps_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Class), color = "white")+
  scale_fill_viridis_c(option = "C")
