#This code gives description of 20 most widely used data manipulations in R
#It uses the iris data from R which is internally stored
#All the steps are given numbers
# Run the install package step first so that you have all the required package
# downloaded and installed before running the code.


#INCLUDING ALL THE REQUIRED LIBRARIES HERE
require(gtools) || install.packages("gtools")
library(gtools)
require(xlsx) || install.packages("xlsx")
library(xlsx)
require(plyr) || install.packages("plyr")
library(plyr)
require(reshape) || install.packages("reshape")
library(reshape)
require(reshape2) || install.packages("reshape2")
library(reshape2)
require(stringr) || install.packages("stringr")
library(stringr)
require(lubridate) || install.packages("lubridate")
library(lubridate)
require(ggplot2) || install.packages("ggplot2")
library(ggplot2)
require(dplyr) || install.packages("dplyr")
library(dplyr)
require(sqldf) || install.packages("sqldf")
library(sqldf)
require(digest) || install.packages("digest")
library(digest)
require(stringr) || install.packages("stringr")
library(stringr)


#Important R codes
IRIS <- iris

#1) To get names in a data frame
names(IRIS)

#2) To see how data frame looks like
str(IRIS)

#3) To filter the nycflights dataframe
IRIS_filter <- IRIS %>% filter(Petal.Length > 3)
IRIS_filter <- filter(IRIS, Petal.Length > 3) #Filter the msleep dataframe with sleep_total >= 16

#4) Selecting set of rows according to conditions acts like a Filter without a dplyr package
IRIS_Conditions <- IRIS[which(IRIS$Petal.Length > 3),]


#5) Subsetting and the selecting some columns from the dataframe.
#Selects Sepal.Length and Petal.Length.

IRIS_subset <- subset(IRIS, Petal.Length >= 3,
                  select=c(Sepal.Length, Petal.Length))

#Selects Sepal.Length and Petal.Length and all the columns between them.
IRIS_subset <- subset(IRIS, Petal.Length>= 3,
                  select=Sepal.Length:Petal.Length)




#6) Selecting a data from a dataframe based on names of columns present in tha dataframe.
IRIS_select <- select(IRIS, - Sepal.Width ) ## Select everything other than Sepal.Width
IRIS_select <- select(IRIS, Sepal.Width:Petal.Width) ## Select everything between Sepal.Width and Petal.Width including Number and State
IRIS_select <- select(IRIS, starts_with("S")) ## Select all columns whose names start with S


#7) To summarise
IRIS %>% summarise(mean_dd = mean(Sepal.Width), sd_dd = sd(Sepal.Width), n = n()) #Piping Operator
IRIS_summarise = summarise(IRIS, mean_dd = mean(Sepal.Width), sd_dd = sd(Sepal.Width), n = n()) 


#8) Creating a new variable is IRIS Dataframe
IRIS$New_Variable <- ifelse(IRIS$Sepal.Length > 5 , "Greater than 5", "Less than 5")


#9) Removing the Newly Created Dataframe from IRIS.
IRIS$New_Variable <- NULL


#10) Sorting a Dataframe
IRIS_Sorted <- IRIS[order(-IRIS$Sepal.Length) , ] # Sorts in descending order of Sepal Width
IRIS_Sorted <- IRIS[order(IRIS$Sepal.Length) , ] # Sorts in ascending order of Sepal Width
# Sorts in ascending order of Sepal Length and if they are same then it sorts in descending
#order of Sepal Width
IRIS_Sorted <- IRIS[order(IRIS$Sepal.Length, -IRIS$Sepal.Width) , ] 
# A different function to 
IRIS_Sorted <- arrange(IRIS, desc(Sepal.Width))
#Sorting using Pipe Operator
IRIS_Sorted %>% arrange(Sepal.Width)




#11) Compress the string 
# Compresses the string in Metric column and removes all the space '-' and '?' from it.
metric_new <- str_replace_all(string=Surveys$Metric, pattern="[ ?-]", repl="")


#12) Renaming Variable
IRIS_Rename <- IRIS
  

#13)Renames the second Column name in the dataframe IRIS_Rename
names(IRIS_Rename)[names(IRIS_Rename)     =="Sepal.Length"]     <- "changed_SepalLength"
names(IRIS_Rename)[2] <- "SEPALWIDTH" # Renaming the variable in the second column
names(IRIS_Rename)[2] <- "Sepal.Length" # Renaming the variable in the second column
names(IRIS_Rename)[2:4] <- c("2", "3", "4") # Rename the column names in 2nd , 3rd and 4th column all together.

IRIS_Rename %>% rename(Species = SPECIES)


#14)Making a new column based on conditions
IRIS$grade[Sepal.Width > 2.8 & Sepal.Width < 3.2] <- "B"             
IRIS$grade[Sepal.Width >= 3.2] <- "A"             
IRIS$grade[Sepal.Width <= 2.8] <- "C"             


#15)Removing the made column
IRIS$grade <- NULL

#16)Remove all the leading and trailing spaces in a particular column and change the text in it to uppercase.
IRIS$Species <- toupper(str_trim(IRIS$Species, side = c("both")))
IRIS <- iris

#17)na.rm Function in R
x <- c(1, 2, NA, 3)
y <- sum(x) # This gives NA since NA is available in the original dataser.
y <- sum(x, na.rm = T) # Does not take NA into account while calculating the sum.



#18)Working with dates.
#Represented as the number of days since
#January 1, 1970, with negative values for earlier dates

strDates <- c("01/05/1965", "08/16/1975") # Date is stored as a character variable
dates <- as.Date(strDates, "%m/%d/%Y")    # Date is stored as a Date format

month(dates)      #To get month
year(dates)       #To get year
day(dates)        #To get day

Sys.Date()  #To get todays system date
date()      #To get todays system date as well as time






#

# IF there is some different format of Dates than the usual format then use this.
RawData <- read.csv("C:/Users/r.pannalal.mundra/Downloads/Simian.csv")
RawData$Boarding_Time <- strptime(RawData$Boarding_Time ,"%m/%d/%Y %H:%M")




#BLOCK OF CODE to get the difference between todays date and 1st January in weeks.
today <- Sys.Date()
dob <- as.Date("1993-01-01")
difftime(today, dob, units="weeks")


#19)Merging two data frames.

Merged_Dataframe <- merge(dataframeA, dataframeB, by=c("ID"), all.x = T) #Left Join
Merged_Dataframe <- merge(dataframeA, dataframeB, by=c("ID"), all.y = T) #Right Join
Merged_Dataframe <- merge(dataframeA, dataframeB, by=c("ID"), all.x = T, all.y = T) #Inner  Join
Merged_Dataframe <- merge(dataframeA, dataframeB, by=c("ID")) #Outer Join



#20)Summarise the data using piping operator
iris%>%
  group_by(Species)%>%
  summarise(Average = mean(Sepal.Length, na.rm = TRUE))

#Summarise using normal code
summarise(group_by(iris, Species), sum = sum(Sepal.Length, na.rm = T))

#Taking a Random Sample of 5 rows without replacing is done using the following code.
IRIS_sample <- IRIS[sample(1:nrow(IRIS), 5, replace=FALSE),]

#21) Using sqldf (To write all the sql queries)

#Important point to note. We use "" on a variable if the variable name has a . in it else 
# There is not need of double quotes while writing a sql query inside sqldf.
IRIS_sqldf <- sqldf('Select distinct Species, sum("Sepal.Length") from IRIS group by Species')


#22) Bind the dataframes when the number of columns in both the dataframe is different#
IRIS2 <- IRIS
IRIS2$Species <- NULL
smartbind(IRIS2, IRIS) #WAY1 Package = gtools
Bind_Data <- bind_rows(IRIS, IRIS2) #WAY2 Package = dplyr (More efficient)

# You can use rbind directly if the number of columns in both the tables is same.

#23) Melting the data in R
IRIS_MELT <- melt(IRIS)



