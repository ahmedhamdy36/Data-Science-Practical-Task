
                               ###############################################
                                    # 1- Review of Big Data Analytic Methods
                               ###############################################
setwd("D:\\final_year_(fourth_year)\\nd_term\\data_science\\Data_Science_project_2022")

#step1
###############################################
# Retrieve and Clean Up Data
###############################################
f=file.path("zeta.csv")
zeta_table <- read.csv (f)

# summary of zeta data

summary(zeta_table)

#number of rows in data

nrow(zeta_table)

# check any duplicate rows of data in the zeta table

duplicated(zeta_table)
print(paste("number of duplicated row is : ",sum(duplicated(zeta_table))))
sum(duplicated(zeta_table))
unique(zeta_table)

# create anew csv file for zeta data

write.csv(zeta_table,"D:\\final_year_(fourth_year)\\nd_term\\data_science\\Data_Science_project_2022\\zeta_nodupes.csv",row.names= FALSE)



#step2
############################################
# Data Analysis
############################################

# Load the text file of income data

zic <- read.table("D:\\final_year_(fourth_year)\\nd_term\\data_science\\Data_Science_project_2022\\zipIncome.txt", sep=",", header=TRUE)
z <- zic

# Change the column names of your data frame so that zcta becomeszipCode and meanhouseholdincome becomes income.
colnames(z) <- c("zipCode", "income")

# summary of income data

summary(z$income)
mean(z$income)

# plotting data

x <- z$zipCode
y <- z$income
# Plot with main and axis titles
# Change point shape (pch = 20) and remove frame.
plot(x, y, main = "plot",
     xlab = "zipCode", ylab = "income",
     pch = 20, frame = FALSE)

#  Removing Outliers

z <- subset(z, z$income  >= 7000 & z$income < 200000)

# mean and summary after Removing Outliers

mean(z$income)
summary(z)


#step3
######################################
# Visualize  data
######################################

# simple box plot of data 
# Plot the chart.
boxplot(z$income ~ z$zipCode, data = z, 
        xlab = "zipCode",
        ylab = "income", 
        main = "zipincome")

# box plot where the y-axis uses a log scale

boxplot(z$income ~ z$zipCode, data=z, range=0, outline=F, log="y",
        xlab="#zipCode", ylab="Income" ,main = "zipincome")


                        ##########################################
                                       # 2-(K-means)
                        ##########################################






setwd("D:\\final_year_(fourth_year)\\nd_term\\data_science\\Data_Science_project_2022")

#  Read in the Data for Modeling

f1=file.path("income_elec_state.csv")

IES <- read.csv (f1)
plot<-read.csv (f1)



#colnames(income_elect_state) <- c("state", "mean household 
#income","mean electricity usage")
colnames(IES)[1] <- "state"
colnames(IES)[2] <- "mean household income"
colnames(IES)[3] <- "mean electricity usage"



# Execute the Model



drop <- c("state")
IES = IES[,!(names(IES) %in% drop)]
#IES <- sort(IES)

#Fit the k-means cluster with 10 initial cluster centers
km <- kmeans (IES,10,10)

# Review the Output

km 


#  Plot the Results

plot(IES, col = km$cluster)


#Plot centers
points(km$centers, col = 1:10, pch = 8)

# Find the Appropriate Number of Clusters

wss <- numeric(15) 
for (i in 1:15) wss[i] <- sum(kmeans(IES, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



#Fit the k-means cluster with 2 initial cluster centers after choosing new k
km <- kmeans (IES,2,10)

km 


#  Plot the Results

plot(IES, col = km$cluster)


#Plot centers
points(km$centers, col = 1:10, pch = 8)



#boxplot(IES$`mean electricity usage` ~ IES$`mean household income`, data = IES, 
 #       xlab = "mean electricity usage",
  #      ylab = "mean electricity usage", 
   #     main = "electricity usage")





#Apply log10:

IES_log <- log10(IES)

# Find the Appropriate Number of Clusters

arr <- numeric(15)
for (i in 1:15)
{
        km <- kmeans(IES_log,centers = i)
        arr[i] <- sum(km$withinss)
}
plot(1:15, arr, type = "b", xlab = "Number of Clusters ",
     ylab = "within groups sum of squares")

#from the 'elbow' plot the best k is 5
#because there is large change around this value

km <- kmeans(IES_log ,5,10)
plot(IES_log , col = km$cluster)



# simple box plot of data 
# Plot the chart.

boxplot(r$income  ~ r$X, data = r, 
        xlab = "state",
        ylab = "mean household income", 
        main = "household income")


#  Removing Outliers
IES <- subset(IES,IES$`mean household income` > 40000 &IES$`mean household income` <70000)                                                                      

# Find the Appropriate Number of Clusters
wss <- numeric(15) 
for (i in 1:15) wss[i] <- sum(kmeans(IES, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


