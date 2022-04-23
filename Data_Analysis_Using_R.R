---
Title: "R Notebook"
Author: Gabriel Egbenya
Topic: Data Manipulation Using R
---

```{r}
install.packages("tidyverse")
install.packages("dplyr")
```

```

# Load Libraries


```{r}
suppressPackageStartupMessages(library(lm.beta))
suppressPackageStartupMessages(library(tidyverse))
library("dplyr")
```
#To answer these questions you will need to use the codebook on Brightspace, called codebook15_llcp.pdf.
#The answer to each question should be assigned to the value before the colon.  




#Set Working Directory to point to Desktop
```{r}
setwd("/Users/gbegb/Desktop")

```



#Load the dataset
```{r}
brfss2015 <- read.csv("BRFSS2015.csv")
brfss2015
```


#Q1: How many people have any kind of health care coverage?

```{r}
Q1 <- brfss2015 %>%                                            #loading the data      
    group_by(HLTHPLN1)%>%                                      #grouping the data by the relevant column      
    select (HLTHPLN1)%>%                                       #selecting the relevant column
    summarise(has_healthPlanCovrge=n(), .groups = 'drop')%>%   #summarizing the result set to get the number of people
    arrange(desc(has_healthPlanCovrge))%>%                     #For this data set column HLTHPLN!: 1 means Yes, 2 means No, 7 means Don't know/Not Sure, 9 means Refused. 
    select(has_healthPlanCovrge)%>%
    head(1)
Q1
```



#Q2: What is the average "Number of Days Mental Health Not Good" for those in Pennsylvania who have numeric data? Make sure to change the response corresponding to none to 0. 

```{r}

Q2 <- brfss2015[c("MENTHLTH")] %>%                                                #loading the data set and selecting only the MENTHLTH column
  filter(MENTHLTH != 77 & MENTHLTH != 99 ) %>%                                      #cleaning the data not to include people that Don't Know/Not Sure (77) & Refused to answer (99)
  mutate(MENTAL_HEALTH = ifelse(MENTHLTH == 88, 0, MENTHLTH)) %>%                   #replacing None (88) with 0
  summarise(Avg_MentHlth = mean(MENTAL_HEALTH, na.rm = TRUE), .groups = 'drop')     #calculating the Average of the total population  
Q2 = round((Q2 * 0.013 ),2)                                                          # calculating the percentage average of people in Pennsylvania only (1.3 % of total population)                                                                                         #and rounding the answer to 2 decimal place
Q2
```



```{r}
Q3 <- brfss2015[, c( "HAVARTH3", "WEIGHT2","WTKG3")] %>%   
  filter(HAVARTH3 == 1 | HAVARTH3 == 2)%>%                                          #filtering the data to only include 1 or 2  which means Yes or No respectively
  group_by(HAVARTH3) %>%                                                            #grouping the data
  na.omit() %>%                                                                             #removing NA's
  mutate( wt_in_pnds = ifelse(WEIGHT2 > 0999, (WEIGHT2-9000) * 2.20462, WEIGHT2)) %>%       #converting the data from kilogram to pounds, and leaving others as is.
  summarise(mean_weight = round(mean(wt_in_pnds, na.rm = TRUE ),2),                         #calculating the mean and sd of the weight in pounds.
            sd_weight = round(sd(wt_in_pnds, na.rm = TRUE),2), .groups = 'drop')%>%
  select(mean_weight, sd_weight)

Q3
```

#For the next questions you'll be exploring the relationship between marital status and minutes of total physical activity per week. 
#Ignore those who refused to answer, weren't asked, or had missing data. You'll need to convert the marital status variable to a factor. Then:

#Q4: Remove outliers from minutes of total physical activity per week using 0.997 and 0.003 as criteria.  
#What percentage of observations remain?  Assign that value to Q4.

```{r}
 
Q4 <- brfss2015[, c('MARITAL', 'PA1MIN_')] %>%                      #creating a new dataset and selecting the required columns
  filter(MARITAL != 9) %>%                                          # removing the refused to answer people from Marital column
  na.omit() %>%                                                     # removing the NA's from Pa1MIN column
  mutate(MARITAL_F = factor(MARITAL), PA1MIN = PA1MIN_ )%>%         #converting the marital col from int to factor & renaming the PA1MIN for convenience
  select(MARITAL_F, PA1MIN)

  Q4_upper <- quantile(Q4$PA1MIN, 0.997, na.rm = TRUE)               #building the code for identifying outliers
  Q4_lower <- quantile(Q4$PA1MIN, 0.003, na.rm = TRUE)
  Q4_out <- which(Q4$PA1MIN > Q4_upper | Q4$PA1MIN < Q4_lower)
  Q4 <- (nrow(Q4) - length(Q4_out))/nrow(Q4)*100
  Q4 = round(Q4,2)                                                    #rounding the result set to 2 decimal place.

Q4
```



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#data without outliers: See code below :
```{r}  
brfss2015_noout <- brfss2015[, c('MARITAL', 'PA1MIN_')] %>%
    filter(MARITAL != 9) %>%
    na.omit() %>%
    mutate(MARITAL_F = factor(MARITAL), PA1MIN = PA1MIN_ )%>%
    select(MARITAL_F, PA1MIN)
  
  Q4_upper <- quantile(brfss2015_noout$PA1MIN, 0.997, na.rm = TRUE)
  Q4_lower <- quantile(brfss2015_noout$PA1MIN, 0.003, na.rm = TRUE)
  Q4_out <- which(brfss2015_noout$PA1MIN > Q4_upper | brfss2015_noout$PA1MIN < Q4_lower)
  brfss2015_noout <- brfss2015_noout[-Q4_out,]                                             #new dataset without outliers ( 286,935 rows), 
brfss2015_noout
``` 
  #PLS Note: moving forward, I will be making reference from this dataset. (brfss2015_noout)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Answer the following questions using the dataset without outliers.  



#Q5:  Group by marital status and calculate the mean, standard deviation, minimum, and maximum of total exercise, to two decimals.
 
```{r}
Q5 <- brfss2015_noout %>%
    group_by(MARITAL_F) %>%
    summarise(mean_data = round(mean(PA1MIN, na.rm = TRUE),2),
              sd_data = round(sd(PA1MIN,na.rm = TRUE),2),
              min_data = min(PA1MIN, na.rm = TRUE),
              max_data = max(PA1MIN, na.rm = TRUE), .groups = 'drop')
Q5
```


#Q6: Create a boxplot for total exercise by marital status.

```{r}
Q6 <- brfss2015[, c('MARITAL', 'PA1MIN_')] %>%                                     #creating a new dataset and selecting the required columns
  filter(MARITAL != 9) %>%                                                         # removing the refused to answer people from Marital column
  na.omit() %>%                                                                    # removing the NA's from Pa1MIN column
  mutate(MARITAL_F = factor(MARITAL), PA1MIN = PA1MIN_ )%>%                        #converting the marital col from int to factor & renaming the PA1MIN for convenience
  select(MARITAL_F, PA1MIN)

level_upper <- quantile(Q6$PA1MIN, 0.997, na.rm = TRUE)
level_lower <- quantile(Q6$PA1MIN, 0.003, na.rm = TRUE)
level_out <- which(Q6$PA1MIN > level_upper | Q6$PA1MIN < level_lower)
data_wit_noout <- Q6[-level_out,]                                                                                #extracting data without outliers

data_wit_noout$PA1MIN = ifelse(data_wit_noout$PA1MIN > 5.460000e+02, 5.460000e+02, data_wit_noout$PA1MIN)        # after extracting the data without outliers at 0.997 & 0.003 quantile, 
                                                                                                                  # the boxplot was still not properly represented, 
                                                                                                                  # so i replaced the maximum value outliers by 4th quantile

ggplot(data = data_wit_noout, mapping = aes(x = MARITAL_F  , y = PA1MIN)) + 
  geom_boxplot()                                                                      #Interpretation of the boxplot result set 1:  Married, 2:  Divorced, 3: Widowed
                                                                                      #4: Seperated, 5: Never married, 6: A member of an unmarried couple
```


#Q7: Run a regression predicting exercise by marital status.  Assign the model summary to Q7.
```{r}
Q7 <- brfss2015[, c('MARITAL', 'PA1MIN_')] %>%                                     
  filter(MARITAL != 9) %>%                                                        
  na.omit() %>%                                                                    
  mutate(MARITAL_F = factor(MARITAL), PA1MIN = PA1MIN_ )%>%                        
  select(MARITAL_F, PA1MIN)

level_upper <- quantile(Q7$PA1MIN, 0.997, na.rm = TRUE)
level_lower <- quantile(Q7$PA1MIN, 0.003, na.rm = TRUE)
level_out <- which(Q7$PA1MIN > level_upper | Q7$PA1MIN < level_lower)
Q7_wit_noout <- Q7[-level_out,]  

Q7 <- lm(PA1MIN ~ MARITAL_F, data = Q7_wit_noout)                                    #assigning the regression predictor to Q7
summary(Q7)
```


#Q8: Run an ANOVA comparing exercise across marital status, and assign the TukeyHSD post-hoc test to Q8.

```{r}
Q8 <- brfss2015[, c('MARITAL', 'PA1MIN_')] %>%                                     
  filter(MARITAL != 9) %>%                                                        
  na.omit() %>%                                                                    
  mutate(MARITAL_F = factor(MARITAL), PA1MIN = PA1MIN_ )%>%                        
  select(MARITAL_F, PA1MIN)

level_upper <- quantile(Q8$PA1MIN, 0.997, na.rm = TRUE)
level_lower <- quantile(Q8$PA1MIN, 0.003, na.rm = TRUE)
level_out <- which(Q8$PA1MIN > level_upper | Q8$PA1MIN < level_lower)
Q8_wit_noout <- Q8[-level_out,] 

Q8_anova <- aov(PA1MIN ~ MARITAL_F, data = Q8_wit_noout)                            
Q8 = TukeyHSD(Q8_anova)                                                             #executing the TukeyHSD post-hoc test
  
Q8
```


#Q9: Run a regression as in Q7, but add total fruits consumed per day.  Based on the R-squared and AIC, what is the better model?  Assign the better AIC value to Q9.

```{r}
Q9 <- brfss2015[, c("MARITAL", "PA1MIN_", "X_FRUTSUM")] %>%                         
  filter(MARITAL != 9) %>%                                                         
  na.omit() %>%                                                                    
  mutate(MARITAL_F = factor(MARITAL), PA1MIN = PA1MIN_)%>%  
  select(MARITAL_F, PA1MIN, X_FRUTSUM)
  colnames(Q9)[colnames(Q9) == "X_FRUTSUM"] <- "FRUTSUM"                                 #renaming the "_FRUTSUM" column to FRUTSUM

  level_upper <- quantile(Q9$PA1MIN, 0.997, na.rm = TRUE)
  level_lower <- quantile(Q9$PA1MIN, 0.003, na.rm = TRUE)
  level_out <- which(Q9$PA1MIN > level_upper | Q9$PA1MIN < level_lower)
  Q7_wit_noout <- Q9[-level_out,]                                                        #removing outliers from the PA1MIN column


  fruits_upper <- quantile(Q7_wit_noout$FRUTSUM, 0.997, na.rm = TRUE)
  fruits_lower <- quantile(Q7_wit_noout$FRUTSUM, 0.003, na.rm = TRUE)
  fruits_out <- which(Q7_wit_noout$FRUTSUM > fruits_upper | Q7_wit_noout$FRUTSUM < fruits_lower)
  fruits_wit_noout <- Q7_wit_noout[-fruits_out,]                                          #removing outliers from the FRUTSUM column

Q9 <- lm(PA1MIN ~ MARITAL_F + FRUTSUM, data = fruits_wit_noout)
Q9 = AIC(Q9)     
Q9

````

#For the final section, you will choose four variables to explore we previously have not.  Complete the following;
#Q10: Remove any outliers.  Briefly explain why you chose the method you used.  Make sure to comment it out.


```{r}

Q10 <- brfss2015[, c('X_FRUTSUM', 'X_AGE65YR' , 'HEIGHT3', 'HTM4')] %>%
  na.omit()
Q1 <- quantile(Q10$X_FRUTSUM, 0.25, na.rm = TRUE)
Q3 <- quantile(Q10$X_FRUTSUM, 0.75, na.rm = TRUE)
IQR <- IQR(Q10$X_FRUTSUM, na.rm = TRUE)

no_outliers <- subset(Q10, Q10$'X_FRUTSUM' > (Q1 - 1.5*IQR) & Q10$'X_FRUTSUM' < (Q3 + 1.5*IQR)) #removing the outliers

boxplot(no_outliers$X_FRUTSUM ~ no_outliers$X_AGE65YR, 
        main = "FRUITSUM boxplot by Age",                                       #renaming the title of the boxplot
        ylab = "FruitConsuption",                                               #renaming the y axis
        xlab = "AgeBracket",                                                    #renaming the x axis
        names = c("Age=18-64", "Age=>64", "Age=Unknown"))                        #renaming the variable properties

#Interquantile range method was used to remove the outliers from '_FRUTSUM' column in the data set.
#I used this method because it is a very popular method in the data science field. quantile(data$column).

```


#Q11: Address the values of any variables.  For instance, is "none" equal to a value other than 0? Are there extra decimals implied? 

```{r}

Q11 <- brfss2015[, c('X_FRUTSUM', 'X_AGE65YR' , 'HEIGHT3', 'HTM4')]               #selected only four columns
  colnames(Q11)[colnames(Q11) == "X_FRUTSUM"] <- "FRUTSUM"                       #renamed the FRUTSUM column
  Q11$FRUTSUM[is.na(Q11$FRUTSUM)] <- 9.999999   #replaced NA with 9.999999      #replaced NA with 9.999999 in FRUTSUM column only
                                                                                #2 decimal implied for HEIGHT3 & HTM4, but not for FRUTSUM column
Q11  
```
Q12: Complete exploratory analyses doing appropriate visualizations with ggplot2.
```{r}
Q12 <- brfss2015[, c('X_FRUTSUM', 'X_AGE65YR' , 'HEIGHT3', 'HTM4', 'SEX')] %>%
    filter(HEIGHT3 != 7777 & HEIGHT3 != 9999) %>%
    group_by(SEX) %>%
    na.omit() %>%
    mutate(Hgt_in_ft.In = round(ifelse(HEIGHT3 >= 9000 & HEIGHT3 <= 9998, (HTM4/100)*3.28, HEIGHT3/100 ), 2))    #convert meter to feet/inches
  
  Q1 <- quantile(Q12$'X_FRUTSUM', 0.25, na.rm = TRUE)
  Q3 <- quantile(Q12$'X_FRUTSUM', 0.75, na.rm = TRUE)
  IQR <- IQR(Q12$'X_FRUTSUM', na.rm = TRUE)
  
  no_outliers <- subset(Q12, Q12$'X_FRUTSUM' > (Q1 - 1.5*IQR) & Q12$'X_FRUTSUM' < (Q3 + 1.5*IQR))             #removing the outliers
  
  ggplot(data = no_outliers) +
    geom_bar(mapping = aes(x = SEX))      
```


#Q14: Finally, run an appropriate regression predicting one of those variables.  Identify the best model.  
  
```{r}
# Created a regression predicting Hgt_in_ft.in by AGEBRACKET and SEX sleep.  After comparison, model A is better best because it has the lowest AIC?


Q14 <- brfss2015[, c('X_FRUTSUM', 'X_AGE65YR' , 'HEIGHT3', 'HTM4', 'SEX')] %>%
  filter(HEIGHT3 != 7777 & HEIGHT3 != 9999) %>%
  group_by(SEX) %>%
  na.omit() %>%
  mutate(Hgt_in_ft.In = round(ifelse(HEIGHT3 >= 9000 & HEIGHT3 <= 9998, (HTM4/100)*3.28, HEIGHT3/100 ), 2))    #convert meter to feet/inches

  Q1 <- quantile(Q14$'X_FRUTSUM', 0.25, na.rm = TRUE)
  Q3 <- quantile(Q14$'X_FRUTSUM', 0.75, na.rm = TRUE)
  IQR <- IQR(Q14$'X_FRUTSUM', na.rm = TRUE)
  
  no_outliers <- subset(Q14, Q14$'X_FRUTSUM' > (Q1 - 1.5*IQR) & Q14$'X_FRUTSUM' < (Q3 + 1.5*IQR))             #removing the outliers


# Model A. 
  Q14 <- no_outliers#%>%
  #names(Q14)[names(Q14) == "X_AGE65YR"] <- "AGE65YR"
  Q14 <- lm(Hgt_in_ft.In ~ SEX + X_AGE65YR, data = Q14)                 #Model A is the best model
  AIC(Q14)
  
  
#Model B  
  Q14 <- lm(Hgt_in_ft.In ~ SEX, data = no_outliers)
  AIC(Q14)
  
  
#Model C
  Q14 <- no_outliers#%>%
  #names(Q14)[names(Q14) == "_AGE65YR"] <- "AGE65YR"
  Q14 <- lm(Hgt_in_ft.In ~ X_AGE65YR, data = Q14) 
  AIC(Q14)
```
The best model is A because it has the lowest AIC
