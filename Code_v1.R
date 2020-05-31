# Data Incubator Challenge 4th November 2019

###### Section 1

#options(digits = 10)

# Loading Data

mydata <- data.table::fread(file = "C:\\Users\\Dell\\Desktop\\Data incubator challenge\\November 2019\\Arrest_Data_from_2010_to_Present (1).csv", header = T)
head(mydata)
tail(mydata)
nrow(mydata)
str(mydata)
View(mydata)
class(mydata)
# lubridate::year(mydata$`Arrest Date`)
# zoo::as.Date(mydata$`Arrest Date`)
# as.numeric(mydata$`Arrest Date`)




### How many bookings of arrestees were made in 2018?

ArrestDate.Object <- lubridate::mdy(mydata$`Arrest Date`)
sum(is.na(mydata$`Arrest Date`)) # Checking for blanks in Arrest Date Column.
str(mydata)
class(ArrestDate.Object)
sprintf("%0.10f",sum(lubridate::year(ArrestDate.Object)==2018))
class(lubridate::year(ArrestDate.Object))
signif(nrow(mydata),digits = 10)

signif(sum(lubridate::year(ArrestDate.Object)==2018),digits = 10)



### How many bookings of arrestees were made in 2018?

length(mydata$`Area ID`)
str(mydata)
sum(is.na(mydata$`Area ID`))
sum(is.na(mydata$`Area Name`))
unique(mydata$`Area Name`)
sum(mydata$`Area Name` %in% unique(mydata$`Area Name`) & lubridate::year(ArrestDate.Object)==2018)
levels(as.factor(mydata$`Area Name`))
summary(as.factor(mydata$`Area Name`[lubridate::year(ArrestDate.Object)==2018]))
sum(mydata$`Area Name`=="Foothill" & lubridate::year(ArrestDate.Object)==2018)
sum(mydata$`Area Name`=="77th Street" & lubridate::year(ArrestDate.Object)==2018)
# ?signif

signif(sort(summary(as.factor(mydata$`Area Name`[lubridate::year(ArrestDate.Object)==2018])),decreasing = T),digits = 10)



### What is the 95% quantile of the age of the arrestee in 2018?

str(mydata)

unique(mydata$`Charge Group Description`)
unique(mydata$`Charge Description`)

length(mydata$Age[lubridate::year(ArrestDate.Object)==2018 & 
      mydata$`Charge Group Description` %in% c("Vehicle Theft","Robbery","Burglary","Receive Stolen Property")])

length(mydata$Age[lubridate::year(ArrestDate.Object)==2018 & 
    (mydata$`Charge Group Description`== "Vehicle Theft" | 
       mydata$`Charge Group Description`== "Robbery" | 
       mydata$`Charge Group Description` == "Burglary" | 
       mydata$`Charge Group Description` == "Receive Stolen Property")])

sum((mydata$Age[lubridate::year(ArrestDate.Object)==2018 & 
                        mydata$`Charge Group Description` %in% c("Vehicle Theft","Robbery","Burglary","Receive Stolen Property")])
)

signif(quantile(x = (mydata$Age[lubridate::year(ArrestDate.Object)==2018 & 
                                 mydata$`Charge Group Description` %in% c("Vehicle Theft","Robbery","Burglary","Receive Stolen Property")])
         ,probs = 0.95),digits = 10)





### There are differences between the average age of an arrestee for the various charge groups.
### Are these differences statistically significant?

apply(mydata$Age[lubridate::year(ArrestDate.Object)==2018 & 
  !(mydata$`Charge Group Description` %in% c("Pre-Delinquency","","Non-Criminal Detention"))],MARGIN = 2,FUN = mean)

length(mydata$Age[lubridate::year(ArrestDate.Object)==2018 & 
                    (mydata$`Charge Group Description` %in% c("Pre-Delinquency","","Non-Criminal Detention"))])

summary(mean(mydata$Age[
  as.factor(mydata$`Charge Group Description`[!(mydata$`Charge Group Description` %in% c("Pre-Delinquency","","Non-Criminal Detention"))]
            )]))

scale(x = mydata$Age[lubridate::year(ArrestDate.Object)==2018 & 
                       !(mydata$`Charge Group Description` %in% c("Pre-Delinquency","","Non-Criminal Detention"))],center = T,scale = T)


avg.age <- aggregate(x = mydata$Age[lubridate::year(ArrestDate.Object)==2018 & 
 !(mydata$`Charge Group Description` %in% c("Pre-Delinquency","","Non-Criminal Detention"))], 
 list(mydata$`Charge Group Description`[lubridate::year(ArrestDate.Object)==2018 & 
!(mydata$`Charge Group Description` %in% c("Pre-Delinquency","","Non-Criminal Detention"))]),
 mean)
avg.age
str(avg.age)

cbind(avg.age$Group.1,abs(scale(avg.age$x,center = T,scale = T)))

signif(sort(abs(scale(avg.age$x,center = T,scale = T)),decreasing = T),digits = 10)




# Felony arrest incidents have been dropping over the years. 
# Using a trend line (linear estimation) for the data from 2010 and 2018 (inclusive), 
# what is the projected number of felony arrests in 2019?
  

modeldata <- aggregate(x = (mydata$`Arrest Type Code`=="F" & lubridate::year(ArrestDate.Object) %in% seq(2010,2018)),
  list(lubridate::year(ArrestDate.Object)[(mydata$`Arrest Type Code`=="F" & lubridate::year(ArrestDate.Object) %in% seq(2010,2018))]),sum)


#Felony <- sum(mydata$`Arrest Type Code`=="F" & lubridate::year(ArrestDate.Object) %in% seq(2010,2018))
#aggregate(x = )
#Felonydata.frame(data = c(X = , 
#  Y = sum(mydata$`Arrest Type Code`=="F" & lubridate::year(ArrestDate.Object) %in% seq(2010,2018))))

Year <- sort(unique(lubridate::year(ArrestDate.Object))[-1])
length(Year)
numFelony <- c()
c <- 0
for (year in Year){
  c <- c+1
  cat(c,year,"\n")
  numFelony[c] <- sum(mydata$`Arrest Type Code`=="F" & lubridate::year(ArrestDate.Object)==year) 
}

numFelony
cbind(Year,numFelony)
modeldata <- data.frame(Year, numFelony)
modeldata

model <- lm( numFelony ~ Year, data = modeldata)
summary(model)
modelpred <- predict(object = model,newdata = data.frame(Year=2019))
signif(modelpred,digits = 10)
sprintf("%.10f",modelpred)
signif(round(modelpred,digits = 0),digits = 10)

### How many arrest incidents occurred within 2 km from the Bradbury Building in 2018?  

# as.numeric(gsub("[^\\d]+", "", unlist(strsplit(x = mydata$Location[1],split = ","))[2], perl=TRUE))
# strsplit(x = mydata$Location[1],split = ",")
# unlist(strsplit(x = mydata$Location[1],split = ","))

location <- (stringr::str_split(string = gsub("[()]","",mydata$Location),pattern = ",",n = 2,simplify = T))
#head(location[,1])

lat <- as.numeric(location[,1])
head(lat)
long <- as.numeric(location[,2])
head(long)
mydata$Latitude <- with(mydata, Latitude <- lat)
mydata$Longitude <- with(mydata, Longitude <- long)
str(mydata)
View(mydata)

### How many arrest incidents were made per kilometer on Pico Boulevard during 2018?




### Calculate this ratio for all charge group code and area ID pairs.




  