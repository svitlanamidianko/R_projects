#### Presented by Svitlana Midianko, September 2019, CS112 MW9AM class ####

#TASK 1.a.
#install.packages('dplyr') #installing needed packages 
#install.packages('date')
library(dplyr, date) #putting it (aka 'attaching') in working memory

foo <- read.csv("https://tinyurl.com/yb4phxx8", na.strings=c("","NA"))
#just examining the data:
head(foo)
summary(foo)
names(foo)
dim(foo)

date.columns <- c(11 , 12 , 14 , 15 , 16 , 17 , 18 , 25)
#note: I have not read the second page of instructions before processing the date objects, thus ended up writing 
#my own code for it 
foo[,date.columns] <- lapply(foo[,date.columns], function(date.columns) as.Date(date.columns, origin = '1970-01-01'))
sapply(foo, class) #to ensure that it is indeed Date 
foo[3,12] - foo[4,12] #just checking if it all works with dates

#subsetting only those that satisfy condition in question 1
length(which(foo$CirculationDate < '2009-01-01'))
rows_to_keep <- which((!is.na(foo$CirculationDate)) & foo$CirculationDate >= '2009-01-01')
new_foo <- foo[rows_to_keep,]
any(is.na(new_foo$CirculationDate)) #double checking

new_foo[,date.columns] #just to examine the data again

approval_duration<- new_foo$OriginalCompletionDate - new_foo$ApprovalDate
#sd(new_foo$approval_duration, na.rm = TRUE)
ave_approval_dur <-  mean(approval_duration, na.rm = TRUE) #in days
sd(new_foo$approval_duration, na.rm = TRUE) #sd is huge 300+, thus we have a very flat distribution
as.numeric(ave_approval_dur)/30.42 #in months
median(approval_duration, na.rm = TRUE)
24* 100/21.4 #finding a percentage of how 'off' the claim is 
new_foo <- cbind(new_foo, approval_duration) #adding a new column to the data in case we need it again

#TASK 1.b.
length(which(is.na(new_foo$RevisedCompletionDate)))
length(which(is.na(new_foo$OriginalCompletionDate))) #16 missing values! needs to be addressed
missing <- c(which(is.na(new_foo$OriginalCompletionDate)))
mean(new_foo[missing,'Rating'], na.rm = TRUE) #looking if they are good 
mean(new_foo[,'Rating'], na.rm = TRUE) #and comparing to general success rate
#seeing what is the project duration of these projects, which is way longer than average du
mean(new_foo[missing, 'RevisedCompletionDate'] - new_foo[missing, 'ApprovalDate'])
head(new_foo)

project_delay <- new_foo$RevisedCompletionDate - new_foo$OriginalCompletionDate
new_foo <- cbind(new_foo, project_delay) 
sorted_foo <- new_foo[order(-project_delay),] 
head(sorted_foo)  
new_foo$project_delay

plot(sorted_foo$CirculationDate, sorted_foo$project_delay, pch=20,col = 'darkblue', main = 'Delays of the Projects Over Time', xlab = 'Circulation Time of the Project (year)', ylab = 'Delay Period (days)')
abline(lm(sorted_foo$project_delay ~ sorted_foo$CirculationDate), col = 'red')
model <- lm(sorted_foo$project_delay ~ sorted_foo$CirculationDate)

#TASK 1.c
mean(sorted_foo$project_delay, na.rm = TRUE)
median(sorted_foo$project_delay, na.rm = TRUE)
quantile(sorted_foo$project_delay, na.rm = TRUE)
IQR(sorted_foo$project_delay, na.rm = TRUE)

actual_duration <- new_foo$RevisedCompletionDate - new_foo$ApprovalDate
mean(actual_duration)
new_foo <- cbind(new_foo, actual_duration)
mean(new_foo$actual_duration, na.rm = TRUE)
median(new_foo$actual_duration, na.rm = TRUE)
median(approval_duration, na.rm = TRUE)

IQR(new_foo$actual_duration, na.rm = TRUE)
IQR(approval_duration, na.rm = TRUE)
quantile(approval_duration,na.rm = TRUE)
quantile(actual_duration,na.rm = TRUE)
head(new_foo)

#TASK 2
after_2010 <- c(which(new_foo$RevisedCompletionDate > '2010-01-01'))
prop.table(table(new_foo[after_2010, 'Rating']))
length(which(is.na(new_foo[after_2010, 'Rating'])))
hist(new_foo[after_2010, 'Rating'], xlab = 'Rating Score', main = 'Percentage Distribution of Ratings Across All the Projects \n (Completed after 2010-01-01)')

prop.table(table(new_foo[new_foo$Type == 'PATA'& new_foo$RevisedCompletionDate > '2010-01-01', 'Rating']))
hist(new_foo[new_foo$Type == 'PATA'&new_foo$RevisedCompletionDate > '2010-01-01', 'Rating'], xlab = 'Rating Score', main = 'Distribution of Success Ratings Across "PATA" Projects \n (Completed after 2010-01-01)')

# TASK 4
length(new_foo)
sorted2_foo <- new_foo[order(-new_foo$RevisedAmount),]
head(sorted2_foo)
dim(sorted2_foo)
top_10 <- sorted2_foo[1:166,] #taking top 10% of the ordered df 
buttom_10 <- sorted2_foo[1501:1666,] #I tried top_n top_fraq but it was giving 10% according to value not items
#thus I ended up doing it very manually.. Curious to learn hot to do it better during OH!. And sorry for
# a spelling mistake, it was too late to change when I noticed it...:(
mean(top_10$RevisedAmount)
median(top_10$RevisedAmount)
mean(buttom_10$RevisedAmount)
median(buttom_10$RevisedAmount)

mean(top_10$Rating, na.rm = TRUE)
median(top_10$Rating, na.rm = TRUE)
mean(buttom_10$Rating, na.rm= TRUE)
median(buttom_10$Rating, na.rm = TRUE)

#lets explroe other variables
top_10$Status
buttom_10$Status
#lets see if there are any dominant types

sort(prop.table(table(top_10[, 'Type'], useNA = "ifany")), descending = TRUE)
sort(prop.table(table(buttom_10[, 'Type'], useNA = "ifany")), descending = TRUE)

#any dominant country?
sort(prop.table(table(top_10[, 'Country'], useNA = "ifany")))
sort(prop.table(table(buttom_10[, 'Country'], useNA = "ifany")))

sort(prop.table(table(top_10[, 'Division'], useNA = "ifany")), decreasing = TRUE)
sort(prop.table(table(buttom_10[, 'Division'], useNA = "ifany")), decreasing = TRUE)

sort(prop.table(table(top_10[, 7], useNA = "ifany")), decreasing = TRUE)
sort(prop.table(table(buttom_10[, 7], useNA = "ifany")), decreasing = TRUE)

sort(prop.table(table(top_10[, 9], useNA = "ifany")), decreasing = TRUE)
sort(prop.table(table(buttom_10[, 9], useNA = "ifany")), decreasing = TRUE)

mean(top_10[, 'Loan' ], na.rm = FALSE)
mean(buttom_10[, 'Loan'], na.rm = FALSE)

#lets see if these 'hugely budgeted' projects are just getting a lot of loan
non_0_loan_top <- c(which(top_10$Loan>0))
dim(top_10[non_0_loan_top,])
only_loan_top <- top_10[non_0_loan_top,]
mean(only_loan_top$Loan)

non_0_loan_buttom <- c(which(buttom_10$Loan>0))
dim(buttom_10[non_0_loan_buttom,])
only_loan_buttom <- top_10[non_0_loan_buttom,]
mean(only_loan_buttom$Loan)

head(top_10)
mean(top_10$approval_duration)
mean(buttom_10$approval_duration)
median(top_10$approval_duration)
median(buttom_10$approval_duration)

mean(top_10$actual_duration)
mean(buttom_10$actual_duration)
median(top_10$actual_duration)
median(buttom_10$actual_duration)

#TASK 5
head(new_foo)
dim(new_foo)

to_remove <- c(which(is.na(new_foo$project_delay)))
PDstudy <- new_foo[-to_remove,]
dim(PDstudy)
head(PDstudy) 
PDstudy1 <- PDstudy[order(PDstudy$project_delay),]
head(PDstudy1)
tail(PDstudy1)
dim(PDstudy)

undelayed <- PDstudy1[1:160,]
delayed <- PDstudy1[1490:1650,]
mean(undelayed$RevisedAmount)
mean(delayed$RevisedAmount)
mean(undelayed$Loan)
mean(delayed$Loan)
head(undelayed)

