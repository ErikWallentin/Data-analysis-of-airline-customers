#####################################################################################################
# In this project, the goal is to analyse randomly created data from an airline price comparison
# website X. The data is presented in the form of two Excel files where the first file named "Feedback"
# contains e.g. feedback from customers who have travelled with a given airline, what time the feedback
# was received and whether the person who gave the feedback used the price comparison site's own 
# application or not when the feedback was submitted.

# The second Excel file named "CoversionRate" contains information on how many people visited the price 
# comparison website a given day, how many bookings were made that day, and what device type the visitor
# used.

# With help of this information, the goal of this project is to answer the three following questions:

# I. What type of feedback are most common among the customers, and is there any difference in 
# what type of feedback a customer gives regarding whether the customer uses the price comparison site's
# own application or not?

# II. Is it possible to distinguish a relationship between the number of feedback and the conversion 
# rate a given day for the different devices? 
# (conversion rate = number of bookings day x / number of visitors day x).

# III. Is it possible to effectively visualize which airline is associated with a given type of feedback?

# The project is divided into three separated parts where the data material undergoes the necessary 
# transformations needed to answer the given questions.
#####################################################################################################


################################
######### Question I ###########
################################


# Import the Excel file "Feedback" using the "XLConnect" package.
install.packages("XLConnect")
library(XLConnect)

user_feedback <- loadWorkbook(file.choose(), create = T)
user_feedback <- readWorksheet(user_feedback,1, header = T)

# Before the analysis phase begins, it's important to look for potential outliers and/or NULL-values 
# in the data that can adversely affect the analysis. After the dataframe "user_feedback" is searched 
# throug by, among other things, the summary ()- and the plot ()-function, a major outlier is found in
# the column "Final.Price".
# Below, this outlier is plotted and then the row containing this value is removed from the data frame,
# since the price for this round-trip ticket amounts to approximately 105 833 Euro. 
# (374 264 077 Colombian peso)
plot(user_feedback$Final.Price)
user_feedback <- user_feedback[-which(user_feedback$Final.Price==max(user_feedback$Final.Price)),]

# Make sure that there is only one type of feedback per row in the column "Feedback.Type" using the 
# "tidyr" package. If a row e.g. contains two feedbacks, that row is split into to two separate rows,
# containing only one feedback per row. The function knows that it should make a divide if the 
# character "," followed by space appears in the "Feedback.Type" column.
install.packages("tidyr")
library(tidyr)

user_feedback <- separate_rows(user_feedback, Feedback.Type, sep = ",\\s")

# Plot the total number of feedbacks per category for the total 10 111 feedbacks given.
barplot(sort(table(user_feedback$Feedback.Type), decreasing = T), 
        xlab = "Category", ylab = "Frequency", font.lab=2,
        main = paste("Total number of feedback per category.", 
                     "\n","A total of", nrow(user_feedback), "feedbacks were given."),
        ylim = c(0,3500), col = "grey")

# Present the number of feedback per category in the R-console
sort(table(user_feedback$Feedback.Type), decreasing = T) 

# Plot the proportion of feedbacks per category for the total 10 111 feedbacks given.
barplot(round(prop.table(sort(table(user_feedback$Feedback.Type), decreasing = T)), 3),
        xlab = "Category", ylab = "Proportion", font.lab=2, 
        main = paste("Proportion of which feedback the customers gives.", 
                     "\n","A total of", nrow(user_feedback), "feedbacks were given."), 
        ylim = c(0,0.35), col = "grey")

# Present the proportion of feedback per category in the R-console
round(prop.table(sort(table(user_feedback$Feedback.Type), decreasing = T)),3)

#####################################################################################################
# We have now answered the first part of question I, as we in the graph and in the R-console can see
# that people complain the most about that the price didn't match, followed by unexpected extras and 
# that the website was hard to use. The second part of question I is answered below.
#####################################################################################################

# Make sure that the two categories "app_mobile" & "app_tablet" are merged under the name "App user",
# and the three categories "desktop", "mobile" & "tablet" are merged under the name "Non app user".
# Lastly, convert the column containing these two categories to a factor column.
user_feedback$Device.Type <- ifelse(user_feedback$Device.Type %in% c("app_mobile", "app_tablet"),
                                    "App user", "Non app user")
user_feedback$Device.Type <- as.factor(user_feedback$Device.Type)

# Find out which category of feedback is most common among app users.
FeedbackApp <- table(user_feedback[which(user_feedback$Device == "App user"),]$Feedback.Type)

barplot(FeedbackApp, xlab = "Category", ylab = "Frequency", font.lab=2,
        main = paste("Total number of feedback per category from customers using the app.", 
                     "\n","A total of", sum(FeedbackApp), "feedbacks were given."),
        ylim = c(0,2000), col = "grey")

# Find out which category of feedback is most common among non app users.
FeedbackNonApp <- table(user_feedback[which(user_feedback$Device == "Non app user"),]$Feedback.Type)

barplot(FeedbackNonApp, xlab = "Category", ylab = "Frequency", font.lab=2,
        main = paste("Total number of feedback per category from customers not using the app.", 
                     "\n","A total of", sum(FeedbackNonApp), "feedbacks were given."),
        ylim = c(0,2000), col = "blue")

# Present the proportion of which type of feedback both app users & non-app users give the most.
FBboth <- rbind(prop.table(FeedbackApp), prop.table(FeedbackNonApp))
barplot(FBboth, beside = T, xlab = "Category", ylab = "Proportion", font.lab=2, ylim = c(0,0.40),
        main = "Proportion of which feedback the customers gives.", col = c("grey", "blue"))
legend("topright", legend = c("App user", "Non app user"), fill = c("grey", "blue"))

# As seen in the plot that present the proportion of which type of feedback both app users & non-app 
# users give the most, we see that feedback tend to differ depending on whether persons use the price 
# comparison site's application or not. To confirm or deny this thesis, a statistical test in the form 
# of a chi-two test is performed.

# Null hypothesis (Ho): The use of the app or not the app has no connection to what type of feedback
# you give.
# Alternative hypothesis (Ha): The use of the app or not the app has a connection to what type of 
# feedback you give.

FBboth2 <- rbind(FeedbackApp, FeedbackNonApp) 
chisq.test(FBboth2)

# X-squared = 149.28 & p-value < 2.2e-16. Ho is rejected at a very low significance level because the
# X-squared and p-value are very large respectively small.
# (p-value = probability that the test statistics X-squared assumes the reached, or an even more
# extreme, value given that the null hypothesis is really true).

# We have statistical evidence that the feedback differ between app users and non-app users.
# The two groups thus tend to give different types of feedback.


################################
######### Question II ##########
################################


# To answer question number two, we need to calculate the correlation between feedback and 
# conversion rate for the two groups computer users and telephone users. 
# To be able to calculate this correlation, our data must first be prepared.

# Import the Excel file "CoversionRate" using the "XLConnect" package.
coversion_rate <- loadWorkbook(file.choose(), create = T)
coversion_rate <- readWorksheet(coversion_rate,1, header = T)

# (After the data frame "coversion_rate" is searched through , no outliers or NULL-values were found).

# We need access to the original values in the column "Device.Type" in the data frame "user_feedback".
# We therefore import the original version, remove the outlier and then perform the same operation so 
# that there is no more than one feedback per row.
user_feedback2 <- loadWorkbook(file.choose(), create = T)
user_feedback2 <- readWorksheet(user_feedback2, 1, header = T)

user_feedback2 <- user_feedback2[-which(user_feedback2$Final.Price==max(user_feedback2$Final.Price)),]

user_feedback2 <- separate_rows(user_feedback2, Feedback.Type, sep = ",\\s")

# Sort the dates in the same way in the two newly created data frames.
user_feedback2 <- user_feedback2[order(user_feedback2$Feedback.Date),]

# Check if the two data frames contain as many unique dates.
length(unique(user_feedback2$Feedback.Date)) # 92 unique dates.
length(unique(coversion_rate$Order.Date)) # 93 unique dates.
# In "user_feedback2" we have data with start date "2018-09-10" and end date "2018-12-10".
# In "coversion_rate" we have data with start date "2018-09-09" and end date "2018-12-10".

# Remove data from the date "2018-09-09" in the data frame "coversion_rate".
Dates <- unique(user_feedback2$Feedback.Date)
coversion_rate <- coversion_rate[coversion_rate$Order.Date %in% Dates,]

# Calculate the conversion rate (number of bookings / number of page visits) and add the result to a 
# column in the data frame "coversion_rate".
coversion_rate$'Conversion rate' <- round(coversion_rate$Bookings / coversion_rate$Entries, 5)

# When inspecting the result after the conversion rate calculation, we see below that three values for
# the conversion rate exceed the value 1. However, the rows containing these values remain in the data
# frame because a person can book more than one trip per page visit.
sort(coversion_rate$'Conversion rate', decreasing = T)

# Four smaller data frames are created, containing the information needed to calculate the 
# correlation between feedback and conversion rate for the two groups computer users and telephone users.
user_feedbackComputer <- subset(user_feedback2[,c(1,4,14)], user_feedback2$Device.Type == "desktop")
user_feedbackPhone <- subset(user_feedback2[,c(1,4,14)], user_feedback2$Device.Type == "app_mobile" | user_feedback2$Device.Type == "mobile")

coversion_rateComputer <- subset(coversion_rate[,c(1:3,6)], coversion_rate$DeviceType == "Desktop Computer")
coversion_ratePhone <- subset(coversion_rate[,c(1:3,6)], coversion_rate$DeviceType == "Phone")

# Examine how many unique dates are in our data frames. This is important because we need to be able
# to link how many feedbacks and conversion rate a specific day has.
length(unique(user_feedbackComputer$Feedback.Date)) # 39
length(unique(coversion_rateComputer$Order.Date))   # 92

length(unique(user_feedbackPhone$Feedback.Date)) # 92
length(unique(coversion_ratePhone$Order.Date))   # 92

# We only have data on people who submitted feedback using a computer for 39 days. After inspection, 
# it appears that data on feedback from computer users began to be collected on "2018-11-02".
# This explains why we only have 39 unique dates in "user_feedbackComputer".


# Trim the data frame "coversion_rateComputer" so that it only contains the same dates as the data 
# frame "user_feedbackComputer".
Dates2 <- unique(user_feedbackComputer$Feedback.Date)
coversion_rateComputer <- coversion_rateComputer[coversion_rateComputer$Order.Date %in% Dates2,]

# Calculate the average conversion rate per date for the two different categories computer users and telephone users.
meanCoversion_rateComputer <- coversion_rateComputer$'Conversion rate'
meanCoversion_ratePhone <- as.vector(round(tapply(coversion_ratePhone$'Conversion rate', coversion_ratePhone$Order.Date, mean),5))

# Calculate the number of feedback per date for the two different categories computer users and telephone users.
totUser_feedbackComputer <- as.vector(tapply(user_feedbackComputer$Feedback.Type, user_feedbackComputer$Feedback.Date,  function(x) length(x)))
totUser_feedbackPhone <- as.vector(tapply(user_feedbackPhone$Feedback.Type, user_feedbackPhone$Feedback.Date, function(x) length(x)))

# Create a scatter plot and calculate the correlation so we can inspect the relationship between the 
# two variables feedback and conversion rate for computer users.
plot(meanCoversion_rateComputer, totUser_feedbackComputer, 
     xlab = "Mean convention rate for computer-users",
     ylab = "Total feedback from people using computers", font.lab = 2, 
     main = paste("The correlation beetwen the two variables is",round(cor(meanCoversion_rateComputer, totUser_feedbackComputer),5)))

cor(meanCoversion_rateComputer, totUser_feedbackComputer) # -0.2436917

# Create a scatter plot and calculate the correlation so we can inspect the relationship between the 
# two variables feedback and conversion rate for telephone users.
plot(meanCoversion_ratePhone, totUser_feedbackPhone,
     xlab = "Mean convention rate for phone-users",
     ylab = "Total feedback from people using phones", font.lab = 2,
     main = paste("The correlation beetwen the two variables is",round(cor(meanCoversion_ratePhone, totUser_feedbackPhone),5)))

cor(meanCoversion_ratePhone, totUser_feedbackPhone) # 0.0572671

# For computer users, we find a small negative correlation, which means that if the conversion rate 
# goes up, the feedbacks tend to decrease slightly and vice versa. 
# For telephone users, we can hardly find any correlation at all between the two variables, which means
# that the relationship between these two variables is weak. Notice that the three days where 
# conversion rate exceeded 1 clearly can be identified in the plot. Had we removed these three days, 
# the correlation beetwen the two variables had become even smaller.

# Finally, we examine whether the low correlation between feedback and conversion rate for telephone
# users could be explained by the fact that a low proportion of users of e.g. BlackBerry and / or 
# Windows Phone books airline tickets when they visit the price comparison site.
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "BlackBerry"),]$'Conversion rate') # mean = 0
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "Windows Phone"),]$'Conversion rate') # mean = 0.02933
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "iPod"),]$'Conversion rate') # mean = 0.1235
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "iPhone"),]$'Conversion rate') # mean = 0.1708
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "Android Phone"),]$'Conversion rate') # mean = 0.12645

# If the correlation between feedback and convention rate for iPhone and Android users only were 
# calculated, the correlation would probably be greater between the two variables, which is confirmed 
# down below.
coversion_rateIphoneAndroid <- subset(coversion_ratePhone, coversion_ratePhone$Devices == "Android Phone" | coversion_ratePhone$Devices == "iPhone")

length(unique(coversion_rateIphoneAndroid$Order.Date)) # 92

meanCoversion_rateIphoneAndroid <- as.vector(round(tapply(coversion_rateIphoneAndroid$'Conversion rate', coversion_rateIphoneAndroid$Order.Date, mean),5))

plot(meanCoversion_rateIphoneAndroid, totUser_feedbackPhone,
     xlab = "Mean convention rate for iPhone or Android users",
     ylab = "Total feedback from people using iPhone or Android", font.lab = 2,
     main = paste("The correlation beetwen the two variables is",round(cor(meanCoversion_rateIphoneAndroid, totUser_feedbackPhone),5)))

cor(meanCoversion_rateIphoneAndroid, totUser_feedbackPhone) # -0.3464306

# We now obtain the greatest correlation so far between the two variables, which means that if the 
# conversion rate goes up, the feedbacks tend to decrease even more and vice versa.


################################
######## Question III ##########
################################


# In question number I, the type of feedback that was most common among customers was reported. 
# However, what was not stated in the answer is which airlines are associated with what type of 
# feedback and which airline who received the most. That information is important to get a hold on so 
# that, for example, an airline that receives a lot of feedback about that their price didn't match
# can assimilate the criticism and fix this problem to improve the customer experience.

# A function is therefore created that returns a data frame containing information about how much of a
# certain type of feedback a given airline has received. The function is flexible to the extent that
# it can handle new data about feedback coming in. The only thing that then needs to be done is to
# import the data file into the variable "user_feedback3" using the package "XLConnect".

# Import the Excel file "Feedback" using the "XLConnect" package and remove the outlier.
user_feedback3 <- loadWorkbook(file.choose(), create = T)
user_feedback3 <- readWorksheet(user_feedback3,1, header = T)
user_feedback3 <- user_feedback3[-which(user_feedback3$Final.Price==max(user_feedback3$Final.Price)),]

# Show values such as "1e-04" as "0.0001"
options(scipen=999)

# Create the function that returns a data frame containing information about which airlines are 
# associated with what type of feedback and which airline who received the most.
# The function call is found at the bottom of the code.
GetFeedback <- function(){
  
  # Make sure that every row only consists of one airlinename in the column "Carrier.Name" and that 
  # there is only one type of feedback per row in the column "Feedback.Type". If a row e.g. contains two
  # airlinenames, that row is split into to two separate rows, containing only one airlinename per row.
  user_feedback3 <- separate_rows(user_feedback3, Carrier.Name, sep = ",\\s")
  user_feedback3 <- separate_rows(user_feedback3, Feedback.Type, sep = ",\\s")
  
  # Get information on how much feedback the given airline company x has received.
  'Flight Not Available' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Flight Not Available"),]$Carrier.Name))
  'Other issues' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Other issues"),]$Carrier.Name))
  'Prices did not match' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Prices didn't match"),]$Carrier.Name))
  'Site hard to use' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Site hard to use"),]$Carrier.Name))
  'Unexpected extras' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Unexpected extras"),]$Carrier.Name))
  
  # Get the names of the airlines that have received at least one feedback. Save these names in a 
  # column of type character in a data frame named "Feedback".
  Name <- sort(unique(user_feedback3$Carrier.Name))
  Feedback <- as.data.frame(Name)
  Feedback$Name <- as.character(Feedback$Name)
  
  # Create a vector containing as many zeros as the number of rows in the data frame "Feedback".
  zero <- rep(0, length(Feedback))
  
  # Below the data frame "Feedback" is extended by adding the total number of feedback a given airline
  # company received for the five different feedback-categories. The first for-each loop down below uses
  # the data frame "Flight Not Available" to find out the name of the first airline that received at 
  # least one feedback of the type "Flight Not Available". In order to place that value, i.e. the 
  # number of feedbacks of the type "Flight Not Available" for airlines x, on the right place in the
  # data frame "Feedback", the line where column one in "Feedback" equals that airline name is tracked. 
  # This procedure is then repeated for the other four feedback-categories.
  # Finally, two columns containing the total number of feedbacks and the proportion of the total
  # number of feedbacks received by the given airline are created.
  
  # Fix the second column "Flight Not Available".
  Feedback$'Flight Not Available' <- zero
  
  `Flight Not Available`$Var1 <- as.character(`Flight Not Available`$Var1)
  
  for(d in 1:nrow(`Flight Not Available`)){
    
    Feedback[which(Feedback$Name ==`Flight Not Available`[d,1]),2] <- `Flight Not Available`[d,2]
  }
  
  # Fix the third column "Other issues".
  Feedback$'Other issues' <- zero
  
  `Other issues`$Var1 <- as.character(`Other issues`$Var1)
  
  for(d in 1:nrow(`Other issues`)){
    
    Feedback[which(Feedback$Name ==`Other issues`[d,1]),3] <- `Other issues`[d,2]
    
  }
  
  # Fix the fourth column "Prices did not match".
  Feedback$'Prices did not match' <- zero
  
  `Prices did not match`$Var1 <- as.character(`Prices did not match`$Var1)
  
  for(d in 1:nrow(`Prices did not match`)){
    
    Feedback[which(Feedback$Name ==`Prices did not match`[d,1]),4] <- `Prices did not match`[d,2]
    
  }
  
  # Fix the fifth column "Site hard to use".
  Feedback$'Site hard to use' <- zero
  
  `Site hard to use`$Var1 <- as.character(`Site hard to use`$Var1)
  
  for(d in 1:nrow(`Site hard to use`)){
    
    Feedback[which(Feedback$Name ==`Site hard to use`[d,1]),5] <- `Site hard to use`[d,2]
    
  }
  
  # Fix the sixth column "Unexpected extras".
  Feedback$'Unexpected extras' <- zero
  
  `Unexpected extras`$Var1 <- as.character(`Unexpected extras`$Var1)
  
  for(d in 1:nrow(`Unexpected extras`)){
    
    Feedback[which(Feedback$Name ==`Unexpected extras`[d,1]),6] <- `Unexpected extras`[d,2]
    
  }
  
  # Fix the seventh column "Total feedback".
  Feedback$'Total feedback' <- rowSums(Feedback[,2:6])
  
  # Fix the eighth column "Proportion of the total feedback".
  Feedback$'Proportion of the total feedback' <- round(prop.table(Feedback$`Total feedback`),5)
  
  # Return the resulting data frame.
  return(View(Feedback))
}

# The function call.
GetFeedback()

# We see that the airline company that received the most feedback regarding that their price didn't 
# match is "Emirates" followed by "Virgin Atlantic".