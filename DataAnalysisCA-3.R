

#Firstly we are going load the crime dataset 

Crime_Ireland <- read.csv("IRELAND_CRIME_GARDA_DIVISION_wise_2003-2019.csv", header = TRUE)

# To check the structure of the Dataset

str(Crime_Ireland)


# To find the missing the value


library(mice)
library(VIM)
md.pattern(Crime_Ireland)
missing_values <- aggr(Crime_Ireland, prop = FALSE, numbers = TRUE)


# Converting the structure of the variable from factor to chararcter

Crime_Ireland$TYPE.OF.OFFENCE <- as.character(Crime_Ireland$TYPE.OF.OFFENCE)


# Putting the values of a column into another

Crime_Ireland$`Abbr. of Crime` <- Crime_Ireland$TYPE.OF.OFFENCE

str(Crime_Ireland)

# Renaming the type of offence in the Abb. of Crime with their Abbreviations

Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "ATTEMPTS/THREATS TO MURDER/ASSAULTS/ HARASSMENTS AND RELATED OFFENCES"] <- "ATTEMPTS/Thrtmdr/A/HARO"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "BURGLARY AND RELATED OFFENCES"] <- "BARO"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "CONTROLLED DRUG OFFENCES"] <- "CRO" 
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "DAMAGE TO PROPERTY AND ENVIRONMENT"] <- "DPE"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "DANGEROUS OR NEGLIGENT ACTS"] <- "DONA"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "FRAUD/DECEPTION AND RELATED OFFENCES"] <- "Fraud/DRO"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "HOMICIDE OFFENCES"] <- "HO"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "KIDNAPPING AND RELATED OFFENCES"] <- "KRO"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "OFFENCES AGAINST GOVERNMENT/ JUSTICE PROCEDURES AND ORGANISATION OF CRIME"] <- "OffencesAG/JPOC"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "PUBLIC ORDER AND OTHER SOCIAL CODE OFFENCES"] <- "POASCO"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "ROBBERY/EXTORTION AND HIJACKING OFFENCES"] <- "Robbery/EHO"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "SEXUAL OFFENCES"] <- "SEXO"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "THEFT AND RELATED OFFENCES"] <- "THRO"
Crime_Ireland$`Abbr. of Crime`[Crime_Ireland$`Abbr. of Crime` == "WEAPONS AND EXPLOSIVES OFFENCES"] <- "WEO"


# Converting the struture back to the factor from the character

Crime_Ireland$`Abbr. of Crime` <- as.factor(Crime_Ireland$`Abbr. of Crime`)

Crime_Ireland$TYPE.OF.OFFENCE <- as.factor(Crime_Ireland$TYPE.OF.OFFENCE)


# To confirm the changes are made successfully

str(Crime_Ireland)


# To check the frequency of the Type of offence using plot

attach(Crime_Ireland)
plot(Crime_Ireland$`Abbr. of Crime`, 
     ylim= c(0,1624), 
     col = rainbow(14), 
     main = "Type of Offences", 
     xlab = "Crime", 
     ylab = "Counts of Crime")




# Changing the quarterly data into yearly by adding the quarterly columns 

Crime_Ireland$Year2003 <- Crime_Ireland$X2003Q1 + Crime_Ireland$X2003Q2 + Crime_Ireland$X2003Q3 + Crime_Ireland$X2003Q4

Crime_Ireland$Year2004 <- Crime_Ireland$X2004Q1 + Crime_Ireland$X2004Q2 + Crime_Ireland$X2004Q3 + Crime_Ireland$X2004Q4

Crime_Ireland$Year2005 <- Crime_Ireland$X2005Q1 + Crime_Ireland$X2005Q2 + Crime_Ireland$X2005Q3 + Crime_Ireland$X2005Q4

Crime_Ireland$Year2006 <- Crime_Ireland$X2006Q1 + Crime_Ireland$X2006Q2 + Crime_Ireland$X2006Q3 + Crime_Ireland$X2006Q4

Crime_Ireland$Year2007 <- Crime_Ireland$X2007Q1 + Crime_Ireland$X2007Q2 + Crime_Ireland$X2007Q3 + Crime_Ireland$X2007Q4

Crime_Ireland$Year2008 <- Crime_Ireland$X2008Q1 + Crime_Ireland$X2008Q2 + Crime_Ireland$X2008Q3 + Crime_Ireland$X2008Q4

Crime_Ireland$Year2009 <- Crime_Ireland$X2009Q1 + Crime_Ireland$X2009Q2 + Crime_Ireland$X2009Q3 + Crime_Ireland$X2009Q4

Crime_Ireland$Year2010 <- Crime_Ireland$X2010Q1 + Crime_Ireland$X2010Q2 + Crime_Ireland$X2010Q3 + Crime_Ireland$X2010Q4

Crime_Ireland$Year2011 <- Crime_Ireland$X2011Q1 + Crime_Ireland$X2011Q2 + Crime_Ireland$X2011Q3 + Crime_Ireland$X2011Q4

Crime_Ireland$Year2012 <- Crime_Ireland$X2012Q1 + Crime_Ireland$X2012Q2 + Crime_Ireland$X2012Q3 + Crime_Ireland$X2012Q4

Crime_Ireland$Year2013 <- Crime_Ireland$X2013Q1 + Crime_Ireland$X2013Q2 + Crime_Ireland$X2013Q3 + Crime_Ireland$X2013Q4

Crime_Ireland$Year2014 <- Crime_Ireland$X2014Q1 + Crime_Ireland$X2014Q2 + Crime_Ireland$X2014Q3 + Crime_Ireland$X2014Q4

Crime_Ireland$Year2015 <- Crime_Ireland$X2015Q1 + Crime_Ireland$X2015Q2 + Crime_Ireland$X2015Q3 + Crime_Ireland$X2015Q4

Crime_Ireland$Year2016 <- Crime_Ireland$X2016Q1 + Crime_Ireland$X2016Q2 + Crime_Ireland$X2016Q3 + Crime_Ireland$X2016Q4

Crime_Ireland$Year2017 <- Crime_Ireland$X2017Q1 + Crime_Ireland$X2017Q2 + Crime_Ireland$X2017Q3 + Crime_Ireland$X2017Q4

Crime_Ireland$Year2018 <- Crime_Ireland$X2018Q1 + Crime_Ireland$X2018Q2 + Crime_Ireland$X2018Q3 + Crime_Ireland$X2018Q4

Crime_Ireland$Year2019 <- Crime_Ireland$X2019Q1 + Crime_Ireland$X2019Q2 + Crime_Ireland$X2019Q3


# Selecting only the relevant columns and saving it into the other dataframe.

Crime_Ireland_adjusted <- data.frame(Crime_Ireland$REGION, Crime_Ireland$GARDA.DIVISION, Crime_Ireland$OFFENCE.CODE,
                                     Crime_Ireland$OFFENCE, Crime_Ireland$TYPE.OF.OFFENCE, Crime_Ireland$`Abbr. of Crime`,
                                     Crime_Ireland$Year2003, Crime_Ireland$Year2004,
                                     Crime_Ireland$Year2005, Crime_Ireland$Year2006, Crime_Ireland$Year2007,
                                     Crime_Ireland$Year2008, Crime_Ireland$Year2009, Crime_Ireland$Year2010, Crime_Ireland$Year2011,
                                     Crime_Ireland$Year2012, Crime_Ireland$Year2013, Crime_Ireland$Year2014, Crime_Ireland$Year2015,
                                     Crime_Ireland$Year2016, Crime_Ireland$Year2017, Crime_Ireland$Year2018, Crime_Ireland$Year2019)




# Droping the irrelevant columns from the new dataframe

CrimefinalIreland <- subset(Crime_Ireland_adjusted, select = -c(1,2,3,4,5) )


# Changing the orientation of the CrimefinalIreland using the reshape2 package


install.packages("reshape2")
library(reshape2)

CrimefinalIreland <- melt(CrimefinalIreland, id=c("Crime_Ireland..Abbr..of.Crime."), variable.name="year", value.name="count")


CrimefinalIreland <- melt(CrimefinalIreland , id.vars=c("year", "count"), variable.name="type",value.name ="offence")


CrimefinalIreland<- subset(CrimefinalIreland, select = -c(3))


library(dplyr)


FinalCrimeDataset <- CrimefinalIreland %>% group_by(year, offence) %>%  # group the columns we want to "leave alone"
summarize(count=sum(count))  
  
FinalCrimeDataset <- dcast(FinalCrimeDataset, year ~ offence)



# Calculating the sum of the rows column wise using rowsum 

FinalCrimeDataset$TotalCrime <- rowSums(FinalCrimeDataset[2:15])



# Remove the particular text from column using the gsub

FinalCrimeDataset$year <- gsub("Crime_Ireland.Year", "",FinalCrimeDataset$year)

# Renaming the cOlumn name using the plyr library

library(plyr)
FinalCrimeDataset <- rename(FinalCrimeDataset, c("year"="Year"))

str(FinalCrimeDataset)


#Here we are converting the structure of column year to Numeric

FinalCrimeDataset$Year <- as.numeric(FinalCrimeDataset$Year)

str(FinalCrimeDataset)


#Here we are loading another dataset which is unemployment rate of Ireland

Unemploymentrate <- read.csv("ireland-unemployment-rate.csv", header = TRUE)


#Removing the rows which are not necessary

Unemploymentrate <- Unemploymentrate[-c(1:12),]


#Removing the date part from the column to show only year using gsub

Unemploymentrate$date <- gsub("\\d\\d/\\d\\d/", "",Unemploymentrate$date)


#Renaming the column name from date to year

Unemploymentrate <- rename(Unemploymentrate, c("date"="Year"))

str(Unemploymentrate)

#Changing the structure of Year to Numeric

Unemploymentrate$Year <- as.numeric(Unemploymentrate$Year)


# Loading one more dataset which contains the details of Alcohol consumption 

AlcoholConsumption <- read.csv("Alcohol and cigarette consumption per annum.csv", header = TRUE)


#Dropping the rows which are not necessary

AlcoholConsumption <-  AlcoholConsumption[-c(1:8),]


#Changing the structure of Year to Numeric

AlcoholConsumption$Year <- as.numeric(AlcoholConsumption$Year)

str(AlcoholConsumption)


# Merging the Unemployment dataset to the main FinalCrimeDataset based on the year column 

FinalCrimeDataset <- merge(FinalCrimeDataset, Unemploymentrate, by.x = "Year", by.y  = "Year",
                           
                           all.x = TRUE, all.y = FALSE)

# Merging the Alcohol Consumption dataset to the main FinalCrimeDataset based on the year column  

FinalCrimeDataset <- merge(FinalCrimeDataset, AlcoholConsumption, by.x = "Year", by.y  = "Year",
                           
                           all.x = TRUE, all.y = FALSE)


# Because we have merge the another dataset so we need to look at the missing value again


library(mice)
library(VIM)
md.pattern(FinalCrimeDataset)
missing_values <- aggr(FinalCrimeDataset, prop = FALSE, numbers = TRUE)

# Dealing with the missing values by filling them according to the mean of the columns  

for(i in 1:ncol(FinalCrimeDataset)){
  FinalCrimeDataset[is.na(FinalCrimeDataset[,i]), i] <- mean(FinalCrimeDataset[,i], na.rm = TRUE)
}

# To confirm that the values has been added or not we need to check the missing the value again 

library(mice)
library(VIM)
md.pattern(FinalCrimeDataset)
missing_values <- aggr(FinalCrimeDataset, prop = FALSE, numbers = TRUE)  

str(FinalCrimeDataset)


# using the lapply we are changing structure of the columns which are integer to numeric

FinalCrimeDataset[] <- lapply(FinalCrimeDataset, function(x) as.numeric(as.integer(x)))

str(FinalCrimeDataset)


# Checking the correlation between the variables using corrplot


opar <- (no.readonly=TRUE)
library(corrplot)
corrplot(corr = cor(FinalCrimeDataset),
         tl.col = "Black", tl.cex = 0.4)




##-----Principal Component Analysis -----------------


# Passing the dataset into the prcomp() function
# and setting two arguments, center and scale, to be TRUE. 
# Then we can have a peek at the PCA object with summary().

pca <- prcomp(FinalCrimeDataset, center = TRUE, scale. = TRUE)
pca

summary(pca)

# eigenvalues measure the amount of variation retained by each principal component. 
# Eigenvalues are large for the first PCs and small for the subsequent PCs. 
# That is, the first PCs corresponds to the directions with the maximum amount of
# variation in the data set.
# We examine the eigenvalues to determine the number of principal components to be considered. 
# The eigenvalues and the proportion of variances (i.e., information) retained by the 
# principal components (PCs) can be extracted using the function get_eigenvalue() 
# from the factoextra package


library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values


fviz_eig(pca, addlabels = TRUE, ylim = c(0,50))



# The components of the get_pca_var() can be used in the plot of variables as follow:
# var$coord: coordinates of variables to create a scatter plot
# var$cos2: represents the quality of representation for variables on the factor map. 
# It’s calculated as the squared coordinates: var$cos2 = var$coord * var$coord.
# var$contrib: contains the contributions (in percentage) of the variables to 
# the principal components. The contribution of a variable (var) to a given principal 
# component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component).

pca_for_variables <- get_pca_var(pca)
pca_for_variables




# Using Correlation plot
#It is possible to use the function corrplot() function  to highlight 
# the most contributing variables for each dimension


library("corrplot")
corrplot(pca_for_variables$cos2, is.corr = FALSE)


# Positively correlated variables are grouped together.
# Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
# The distance between variables and the origin measures the quality of the variables on 
# the factor map. Variables that are away from the origin are well represented on the factor map.


fviz_pca_var(pca,col.var = "black")



# Cos2 - quality of representation
# The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates). 
# We can access to the cos2 as follows


head(pca_for_variables$cos2, 10)


# We can show a bar plot of variables cos2 using the function fviz_cos2()
# from the in factoextra library.

# Total cos2 of variables on Dim.1 and Dim.2


fviz_cos2(pca, choice = "var", axes = 1:2)




fviz_pca_var(pca, col.var = "cos2", 
             gradient.cols = c("red", "Blue", "Green"),
             repel = TRUE)



# Contribution of variables to each PC
# The larger the value of the contribution, the more the variable contributes to the component. 

head(pca_for_variables$contrib, 20)



# The most important (or, contributing) variables can be highlighted on the correlation plot as follows

fviz_pca_var(pca,
             axes = c(1,2),
             col.var = "contrib",
             gradient.cols = c("red","Blue","Green"))



# We can use the function fviz_contrib() from the factoextra package
# to draw a bar plot of variable contributions. If your data 
# contains many variables, you can decide to show only the top 
# contributing variables. This code shows the top 20 variables 
# contributing to the principal components:
# Contributions of variables to PC1


library(factoextra)

fviz_contrib(pca, choice = "var", axes = 1, top = 20)


# Contributions of variables to PC2

fviz_contrib(pca, choice = "var", axes = 2, top = 20)


# Contribution to PC1 - PC5

fviz_contrib(pca, choice = "var", axes = 1:5, top = 20)



# Using statistical methods to examine
# the relationships between variables of interest
# As the variables are continous so we need to perform Pearson’s Correlation Coefficient test
# to examine the relationship between the variables 

test <- cor.test(FinalCrimeDataset$Unemployment.Rate...., FinalCrimeDataset$KRO,
                 method = 'pearson', exact = FALSE)             
test             

test <- cor.test(FinalCrimeDataset$Alcohol.Litres..per.capita.over.15, FinalCrimeDataset$HO,
                 method = 'pearson', exact = FALSE)             
test


write.csv(FinalCrimeDataset, file = "FinalCrimeDataset.csv")


#================End========================================================================================


