#Homework-2 

#naming the columns for simpler viewing and using them
colnames(Typical_Employee_Survey_Data)<- c("Age", "Gender", "JobSatisfaction", "ImportantCharacteristics", "YearsActive", "PromotionChances", "AffectingWorkDec.", "Budg.Decision", "PrideAtWork", "Loyalty", "WorkRelations")
#Typical_Employee_Survey_Data data used


mydf <- data.frame(Typical_Employee_Survey_Data)  #converting the data set into a data frame
mydf

# Explore Data Frame

names(mydf) #displaying the column names
ncol(mydf)  #number of columns
nrow(mydf)  #number of rows
dim(mydf)   #dimensions of the data frame
head(mydf)  #top 6 values of the df
tail(mydf)  #bottom 6 values of the df
str(mydf)   #displaying the data type of each column in the df
class(mydf$Gender) #displaying the data type of a specific column 'gender' in the df
class(mydf$Age) #displaying the data type of a specific column 'Age' in the df
mydf$Gender <- as.factor(mydf$Gender) #changing the data type of 'gender' column in the df
class(mydf$Gender)  #confirming the change
str(mydf)

#columnn

mydf$Age #displaying the values of a specific column 'age'
mydf[1]  #another way to display the values of the 'age' column
mydf[c("Age")] #another way to display the values of the 'age' column
mydf[2:3] #displaying the values of the 'gender' and 'satisfactionwithjob' column which are the 2nd and 3rd column
mydf[c("Age","Gender")] #another way to display the values of the 'gender' and 'JobSatisfaction' column which are the 2nd and 3rd column

# Rows

mydf[4,] #displaying a specific row ,'4th', from the data frame for viewing

# Combined

mydf[2:3,c("JobSatisfaction")] #displaying values from specific column and specified rows (rows 2nd and 3rd, column 'JobSatisfaction') 
mydf[2:3,2:3] #displaying values from specific columns and specified rows (rows 2nd and 3rd, column 'gender' and 'JobSatisfaction')

#Levels

mydf$Gender
table(mydf$Gender)  #converting the column values from 'Gender' into a table
table(mydf$Age)     #converting the column values from 'Age' into a table
table(mydf$Age, mydf$Gender) #Combining both tables to see the age distribution among males and females

# Filtering

mydf[mydf$Gender == "1",] #filtering out all the values for males
mydf[mydf$Gender == "2",] #filtering out all the values for females
mydf[mydf$Age == "39",] #filtering out all the values for employees aged 39

# Ordering

mydf[order(mydf$Age),]  #sorting/ordering all the column values by age, in ascending order
mydf[order(-mydf$Age),] #sorting/ordering all the column values by age, in descending order
mydf[order(-mydf$JobSatisfaction),] #sorting/ordering all the column values by Job Satisfcation, in descending order (Very Dissatisfied to Very Satisfied)
mydf[order(mydf$PromotionChances),] #sorting/ordering all the column values by Promotion Chances, in ascending order (Very Likely to Very Unlikely)


# Change Column Name

names(mydf)[4] <- "IMP.Characteristics" #changing column name from ImportantCharacrteristics to IMP.Characteristics
head(mydf)
names(mydf)[5] <- "Yrs.Employed"  #changing column name from YearsActive to Yrs.Employed
head(mydf)

# Changing Row Values

mydf [1,1] <- 36 #changing 'Age' value for the 1st employee to '36' from '35'
head(mydf)
mydf$Age[2] <- 32 #changing 'Age' value for the 2nd employee to '32' from '33'
head(mydf)


# Graphing
barplot(mydf$Age) #minimalist bar chart of ages in the survey data

barplot(mydf$Age, main = "Age of People", xlab = "Employees", ylab = "Age", col = "Red") #proper graph of ages with name, y-axis name, x-axis name and colored plots 

pie(mydf$Age) #minimalist pie chart of employee ages in the survey data

pie(mydf$Age, main = "Ages of Employees") #proper pie chart of employee ages in the survey data with name of the pie chart

stem(mydf$Yrs.Employed)  #creating a stem and leaf plot w.r.t to the Years employed of employees of the company

stem(mydf$Age)  #creating a stem and leaf plot w.r.t to the age of employees of the company

hist(mydf$Age)  #creating a histogram by the age of employees of the company

hist(mydf$JobSatisfaction) #creating a histogram by the age of employees of the company

hist(mydf$Loyalty) #creating a histogram by the loyalty of employees of the company

hist(mydf$PromotionChances) #creating a histogram by the chances of promotion of employees of the company

boxplot(mydf$Age) #creating a boxplot by the age of employees of the company

boxplot(mydf$PromotionChances) #creating a boxplot by the chances of promotion of employees of the company

boxplot(mydf$Age,mydf$Yrs.Employed) #creating 2 boxplots by the age and years employed of employees of the company to view them together


# Summary Stats

min(mydf$Age) #finding out the minimum age out of all the employees
max(mydf$Age) #finding out the maximum age out of all the employees
range(mydf$Age) #finding out the range of ages in all the employees
min(mydf$Yrs.Employed) #finding out the minimum age out of all the employees
max(mydf$Yrs.Employed) #finding out the maximum age out of all the employees
range(mydf$Yrs.Employed) #finding out the range of ages in all the employees

AgeRange = max(mydf$Age) - min(mydf$Age)
AgeRange
#rm(StatRange)
Yrs.EmployesRange = max(mydf$Yrs.Employed) - min(mydf$Yrs.Employed)
Yrs.EmployesRange

mean(mydf$Age)  #calculating mean of age
mean(mydf$Yrs.Employed) #calculating mean of years emoloyed
sd(mydf$Age)  #standard dev of age
var(mydf$Age) #variance of age
sqrt(var(mydf$Age)) #another way to calculate the standard dev
fivenum(mydf$Age) #calculating Turkey's five number summary of age from the survey data
IQR(mydf$Age)     #interquartile range of age
quantile(mydf$Age)  #quartiles of age w.r.t to the survey data
summary(mydf$Age)
boxplot.stats(mydf$Age)
boxplot.stats(mydf$Age)$out


mean(mydf$PrideAtWork)  #calculating mean of employees being proud at the organization they work at
mean(mydf$PrideAtWork) #calculating mean of yemployees being proud at the organization they work at
sd(mydf$PrideAtWork)  #standard dev of employees being proud at the organization they work at
var(mydf$PrideAtWork) #variance of employees being proud at the organization they work at
sqrt(var(mydf$PrideAtWork)) #another way to calculate the standard dev

fivenum(mydf$Yrs.Employed) #calculating Turkey's five number summary of years employed from the survey data
IQR(mydf$Yrs.Employed)     #interquartile range of years employed
quantile(mydf$Yrs.Employed)  #quartiles of years employed w.r.t to the survey data
summary(mydf$Yrs.Employed)
boxplot.stats(mydf$Yrs.Employed)
boxplot.stats(mydf$Yrs.Employed)$out

# Data Frame Summary

summary(mydf) #summary of the entire survey data

by(mydf$Age,mydf$Gender,mean) #calculating the mean ages separately for male and female employees 
by(mydf$Age,mydf$Gender,sd)   #calculating the sd of ages separately for male and female employees 
by(mydf$Age,mydf$Gender,summary)

by(mydf$Yrs.Employed,mydf$Gender,mean) #calculating the mean of being employed at the company separately for male and female employees

by(mydf$Yrs.Employed,mydf$JobSatisfaction,mean) #calculating mean of employment years based their satisfaction with the job

aggregate(mydf$Age,list("Type" = mydf$Gender),median) #finding out the median for different gender by ages

aggregate(mydf$Age,list("Type" = mydf$Gender),summary) #finding out the summary from the survey data for different genders by their ages


