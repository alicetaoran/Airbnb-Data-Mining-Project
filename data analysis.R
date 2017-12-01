
############################
#### Airbnb Data Mining ####
############################


## Project Objective
### Study the distribution of ratings of properties
### between and within the districts in Paris as of July 2017 
### and the factors that affect the ratings, 
### using the analysis result to make better 
### property recommendation to people who are traveling to Paris



library(arm)
library(dplyr)
library(sqldf)
library(tidyr)
library(ggplot2)
library(ggmap)
library(nnetpredint)
library(lme4)
library(rstan)
library(rstanarm)


######################
## Data pre-process ##
######################


### Import data

ParisAirbnb<-read.csv("Paris Airbnb.csv")

### Remove unrelevant columns

Parisdat<-ParisAirbnb[, c(-2,-5,-6,-7,-13,-15,-16,-18,-21)]


### Correct French character into English character
### as french character cant be stored properly in csv file

Parisdat$neighborhood<-as.character(Parisdat$neighborhood)

Parisdat$neighborhood[Parisdat$neighborhood=="Amérique"]="Amerique"
Parisdat$neighborhood[Parisdat$neighborhood=="Champs-Elysées"]="Champs-Elysees"
Parisdat$neighborhood[Parisdat$neighborhood=="Chaussée-dAntin"]="Chaussee-dAntin"
Parisdat$neighborhood[Parisdat$neighborhood=="Folie-Méricourt"]="Folie-Mericourt"
Parisdat$neighborhood[Parisdat$neighborhood=="Grandes-Carrières"]="Grandes-Carrieres"
Parisdat$neighborhood[Parisdat$neighborhood=="Hôpital-Saint-Louis"]="Hopital-Saint-Louis"
Parisdat$neighborhood[Parisdat$neighborhood=="Père-Lachaise"]="Pere-Lachaise"
Parisdat$neighborhood[Parisdat$neighborhood=="Place-Vendôme"]="Place-Vendome"
Parisdat$neighborhood[Parisdat$neighborhood=="Saint-Germain-des-Prés"]="Saint-Germain-des-Pres"
Parisdat$neighborhood[Parisdat$neighborhood=="Salpêtrière"]="Salpetriere"


### Check Missing data

summary(Parisdat)

### there is no NA data
### while ratings are our dependent variable
### it doesn't make sence to include properties with zero reviews

Parisdat<-filter(Parisdat, reviews>0)
  
### Check category data
### All the category data has been factored

table(Parisdat$room_type)
table(Parisdat$neighborhood)
table(Parisdat$property_type)


### Check Numberic Data

### we can characterlized id of room and host 

Parisdat$room_id<-as.character(Parisdat$room_id)
Parisdat$host_id<-as.character(Parisdat$host_id)

### check outliers of numeric variables: 
### number of accommodate, bedroom and price

mahal<-mahalanobis(Parisdat[, c(7:9)],
                    colMeans(Parisdat[, c(7:9)]),
                    cov(Parisdat[, c(7:9)], use = "pairwise.complete.obs"))

cutoff<-qchisq(1-0.001, ncol(Parisdat[, c(7:9)]))

paris<-Parisdat[mahal<cutoff, ]

length(unique(paris$room_id))

### Check ratings (overal satisfication) and number of reviews

hist(paris$reviews, main = "Distribution of Reviews", xlab = "Number of reviews")

ecdf.rev<-ecdf(paris$reviews)
ecdf.rev(50)
max(paris$reviews)

### there are 50406 unique rooms in our data after cleaning
### as we can see 89.7% of total Airbnb rooms, that are 45104 rooms have less than 50 reviews
### while there is 1 room which has 529 reviews
### 11 rooms have over 400 reviews, 46 rooms have over 300 reviews
### 332 rooms have over 200 reviews, 1800 rooms have over 100 reviews

### we can create a table of reviews with number of rooms

review<-sqldf("SELECT reviews, COUNT (room_id) FROM paris Group by 1")
colnames(review)<-c("reviews", "Num_room")

### now let's explore the ratings

hist(paris$overall_satisfaction, main = "Distribution of Ratings", xlab = "overall satisfaction")

summary(paris$overall_satisfaction)

Rating<-sqldf("SELECT overall_satisfaction, COUNT (room_id) FROM paris Group by 1")
colnames(Rating)<-c("rating","Num_room")

### we can see only 22.7% of rooms with ratings less than 1
### while 75% of room with ratings higher than 4 
### and the average rating is 3.6
### 25% of rooms have ratings of 5
### we can see generally the airbnb properties in paris have good ratings


### now let's have a look of relationship between ratings and number of reviews

ggplot(data=paris, aes(x=overall_satisfaction, y=reviews))+geom_jitter()+xlab("Ratings")

ggplot(data=paris, aes(x=overall_satisfaction, y=reviews))+geom_bin2d()+xlab("Ratings")+
  ggtitle("Ratings & Reviews")

### as we can see, generally higher ratings tend to have more reviews



#######################
## Data manipulation ##
#######################

### 1) we are going to add a district indicator variable
### In total, there are 20 districts in paris
### Through searching data online, 
### we found the Violence rate per 1000 inhabitants of each district
### from http://www.lefigaro.fr/actualite-france/2017/01/02/01016-20170102ARTFIG00290-decouvrez-la-carte-des-crimes-et-delits-en-france-et-dans-le-grand-paris.php
### we will import this district info and add to our data

index<-read.csv("Index.csv")
colnames(index)<-c("neighborhood","district","violenceRate")

### factor district

index$district<-as.factor(index$district)

### characterized neighborhood to join two tables

index$neighborhood<-as.character(index$neighborhood)
paris$neighborhood<-as.character(paris$neighborhood)

parisdata<-left_join(paris, index, by = "neighborhood")

### create a district table with number of rooms per district

district<-sqldf("SELECT district, COUNT (room_id), Avg(overall_satisfaction),
                Avg(longitude), Avg(latitude) FROM parisdata Group by 1")
colnames(district)<-c("district","Num_room", "Avg_rating", "lon", "lat")

ggplot(data=district, aes(x=district, y=Num_room))+geom_bar(stat = "identity")+
  ggtitle("Number of Airbnb Rooms per District")+ylab("number of room")


### we also create the map with the circle size showin gthe number of rooms in each district
parismap<-qmap("paris", zoom=12, "osm")

parismap+
  geom_point(aes(lon, lat), data=district, col="blue", alpha=0.4, size = (district$Num_room/500))+
  scale_size_discrete(range=range(district$Num_room))

### as we can see 2nd district has smallest number of rooms: 817
### while 18th district has the highest number of rooms: 6334
### 10th and 11th have over 4000 room respectively

### In conclution, the sample sizes in all districts are relative large


### 2) Dependent variable
### as we know a rating of 5 with 100 reviews is different from
### a rating of 5 with 1 reviews
### therefore, we are going to creat the weighted rating as our response
### the weight will be determined by the number of reviews
### we use sigmoid function to determine the weight
### as the increase of review can't indicate the linear increase of the weighted rating
### we can't say a rating of 5 with 1000 reviews is 1000 times bettter than
### the rating of 5 with 1 reviews
### we also found prevouis study of yelp rating using sigmoid function
### http://www.developintelligence.com/blog/2017/06/practical-neural-networks-keras-classifying-yelp-reviews/

sig.function<-function(data){
  y=1/(1+6.16*exp(-0.18*data))
  return(y)
}


weight<-sig.function(parisdata$reviews)
weight<-as.data.frame(weight)
colnames(weight)<-"weight"

### add to our dataset

parisdata$weight<-weight$weight

### our independent variable

parisdata$weightedRating<-parisdata$overall_satisfaction*parisdata$weight


### 3) Predictors

### our group level indicator: violence rate per district
### individual indicator:
### catergory: room type, property type
### numeric: price, accommodates, bedrooms


#######################
######### EDA #########
#######################

### before we fit the multilevel model
### we need initial EDA to help us have a
### direct insight of the distribution of weighted ratings 
### among and between districts
### and also the relationship of independent variables and dependents variables

### 1 Independent and Dependent

### 1) Distribution of ratings in different districts

ggplot(data=parisdata, aes(x=weightedRating))+
  geom_histogram()+facet_wrap(~district) + ggtitle("Weighted Rating distribution per district")

ggplot(data=parisdata, aes(x=weightedRating, fill=district))+geom_density()+
  ggtitle("Weighted Rating distribution")

### as we can see the distribution of weighted ratings is different between districts


### 2) Average weighted rating per district

districtdat<-sqldf("SELECT district, Avg(longitude), Avg(latitude), 
                Avg(weightedRating) FROM parisdata Group by 1")

colnames(districtdat)<-c("district", "lon", "lat", "AvgWeightedRating")

ggplot(data=districtdat, aes(x=district, y=AvgWeightedRating, col="red"))+geom_point()+
  ggtitle("Average Weighted Rating of Each District")


### we also plot the average rating into map

parismap+
  geom_point(aes(lon, lat), data=districtdat, col="red", 
             alpha=0.4, size = ((districtdat$AvgWeightedRating-1)*5))

### as we can see, the average weighted rating is different between districts


### 3) weighted rating and price
### obvious variablity of slope between district
ggplot(data=parisdata, aes(x=price, y=weightedRating, group=district))+
  geom_smooth(method = "lm")+ggtitle("Weighted Rating and Price")


### 4) weighted rating and accommodates
### varing slope betweeen district
### but less variability compared with that of weighted rating and price
ggplot(data=parisdata, aes(x=accommodates, y=weightedRating, group=district, col="red"))+
  geom_smooth(method = "lm")+ggtitle("Weighted Rating and Accommodates")


### 5) weighted rating and bedrooms
### varing slope betweeen district
ggplot(data=parisdata, aes(x=bedrooms, y=weightedRating, group=district))+
  geom_smooth(method = "lm")+ggtitle("Weighted Rating and Bedrooms")


### 5) weighted rating and room type
### varing slope betweeen district
ggplot(data=parisdata, aes(x=weightedRating, fill=room_type))+geom_density()+
  ggtitle("Weighted Rating and Room Type")



### 2. Between predictors ==> check Collinearity

### 1) correlation calculation

correlation<-cor(parisdata[,c(7,8,9,14)])
symnum(correlation)

### we can see there is a moderate correlation between accommodates and bedrooms

### 2) room type and accommodates
### entire home tends to allow more accommodates
ggplot(data=parisdata, aes(x=accommodates, fill=room_type))+geom_bar(position = "fill")+
  ggtitle("Accommodates and Room Type")

### 3) room type and bedrooms
### most of airbnb listes are entire home/apt or private room
ggplot(data=parisdata, aes(x=bedrooms, fill=room_type))+geom_bar(position = "fill")+
  ggtitle("Bedrooms and Room Type")

### 4) price and property type
### most of airbnb listed are apartment
ggplot(data=parisdata, aes(x=price, fill=property_type))+geom_histogram()+
  ggtitle("Price and Property Type")

### 5) price and district
### variability between districts
ggplot(data=parisdata, aes(x=price, fill=district))+geom_histogram()+
  ggtitle("Price and District")

### 6) violence rate and district
### variability between districts
ggplot(data=parisdata, aes(x=district, y=violenceRate))+geom_point()+
  ggtitle("Violence rate and District")




#######################
###### Model Fit ######
#######################

parisdata$cprice<-scale(parisdata$price, center = TRUE, scale = TRUE)

### 0) Model0: no random effect

model0<-lm(weightedRating~room_type+accommodates+
               bedrooms+cprice+violenceRate, data=parisdata)

display(model0)

### as we can see from the output of the model, 
### all the coefficients are statistically significantly
### with each unit increase of accommodates, weighted rating increases by 0.06;
### with every unit increase of bedrooms, weighted rating decreases by 0.02;
### with every unit increase of violence rate, weighted rating increases by 0.02,
### as the districts in the downtown have higher rate of violence
### which indicate airbnb users prefer properties near downtown
### with each unit increase of price deviating from mean price over standard deviation
### weighted rating increases by 0.13
### with other variable constant, shared room has 0.6 higher weighted rating
### than that of entired room,
### while pricate room has 0.14 higher weighted rating than that of entired room




### 1) Model1: random intercept
### group predictor of violence rate
### and individual predictor of room type, accommodate, bedrooms, price


model1<-lmer(weightedRating~room_type+accommodates+
               bedrooms+cprice+violenceRate+(1|district), data=parisdata)

display(model1)

### as we can see from the output of the model, 
### all the coefficients are statistically significantly except the 
### coeficient of violence rate
### with each unit increase of accommodates, weighted rating increases by 0.08 on average;
### with every unit increase of bedrooms, weighted rating decreases by 0.15 on average;
### with each unit increase of price deviating from mean price over standard deviation
### weighted rating increases by 0.03 on average
### with other variable constant, shared room has 0.59 higher weighted rating
### than that of entired room on average,
### while private room has 0.16 higher weighted rating than that of entired room


### the the unexplained within-county variation has an estimated standard
### deviation of 1.71
### the estimated standard deviation of the district intercept is 0.28

### as we can see from the correlation table:
### there is high correlation between bedrooms and accommodates,
### violence rate and intercept


### 2) Model2: random slope

model2<-lmer(weightedRating~accommodates+room_type+
               bedrooms+violenceRate+cprice+(0+cprice|district), data=parisdata)

display(model2)


### as we can see from the output of the model, 
### all the coefficients are statistically significantly
### with each unit increase of accommodates, weighted rating increases by 0.06 on average;
### with every unit increase of bedrooms, weighted rating decreases by 0.21 on average;
### with every unit increase of violence rate, weighted rating increases by 0.02 on average,
### as the districts in the downtown have higher rate of violence
### which indicate airbnb users prefer properties near downtown
### with each unit increase of price deviating from mean price over standard deviation
### weighted rating increases by 0.16 on average
### with other variable constant, shared room has 0.62 higher weighted rating
### than that of entired room on average,
### while pricate room has 0.17 higher weighted rating than that of entired room


### the the unexplained within-county variation has an estimated standard
### deviation of 1.71
### the estimated standard deviation of the district price slope is 0.12

### as we can see from the correlation table:
### there is high correlation between bedrooms and accommodates,
### accommodate and intercept



### 3) Model3: random slope and intercept

model3<-lmer(weightedRating~accommodates+room_type+
               bedrooms+violenceRate+cprice+(1+cprice|district), data=parisdata)

display(model3)

### as we can see, the coefficients of violence rate is not statistically significant
### the intercept does't change much compared with the previous models
### the coeficent of accomodates is between that of model2 and model1
### the bedroom coeficient is equal to that of model1, indicating each unit increase
### of bedroom will decrease the weighted rating by 0.15
### with each unit increase of price deviating from mean price over standard deviation
### weighted rating increases by 0.04 on average

### the residual within district is 1.71, which is smaller than those of model1 and model2
### indicating model3 is a better fit
### the sd of district price slope is 0.06, and its correlation with intercept is -0.41
### while the sd of district intercept is 0.28


###
model3.1<-lmer(weightedRating~cprice+room_type+
               bedrooms+violenceRate+accommodates+(1+accommodates|district), data=parisdata)

display(model3.1)

### as we can see, the coefficients of violence rate is not statistically significant
### the coeficient of bedrooms, accommodates, and violence rate in model3.1 are same 
### as those in model3
### while the coeficeint of private room, shareroom and price decrease slightly compared
### with model3

### the residual is same as that of model3
### while the standard error of district intercept and accomodates slope decreased
### the correlation between accommodates and intercept is 0.09


###
model3.2<-lmer(weightedRating~cprice+room_type+
                 accommodates+violenceRate+bedrooms+(1+bedrooms|district), data=parisdata)

display(model3.2)

### the coeficients of bedroom, voilence and price ramain unchange compared with model3
### while the intercept decreases by 0.05 to 2.02
### the coeficients of private and shared room decrease slightlt
### and the accommodate coifficient increases by 0.01

### the residual is 1.71, same as that of model3 and model3.1
### the sd of district intercept is same as that of model3
### while the district bedroom slope is between that of model3 and model3.1
### correlation between bedroom slope and district intercept is -0.25


### IN CONCLUSION: random slope and random intercept model is a better fit
### compared with only random slope or only random intercept



### 4) Model4: random slope and intercept with interaction

### as we noticed in the previous model and also shown in our correlation table
### there is a moderate colliearity between accommodate and bedrooms
### we can add interaction of those two into our model

model4<-lmer(weightedRating~accommodates*bedrooms+
                 room_type+violenceRate+cprice+(1+cprice|district), data=parisdata)

display(model4)

### the coeficients of price and share room incease slightly compared with model3
### the coeficients of private room and voilence remain same
### while the coeficients of accommodate and bedroom increase, which is 
### offset by the coeficient of those two interaction

### the sd of distirct intercept, slope and their correlatio are same as model3
### the residual is same as model3, while the deviance decreases


### 
model4.1<-lmer(weightedRating~accommodates*room_type+bedrooms+
               violenceRate+cprice+(1+cprice|district), data=parisdata)

display(model4.1)


### the coeficients of price and voilence remain same
### the coiffienct of accommodate, private and share increase obviously, 
### which are offset by the interaction
### the bedroom coefficient decreases

### the sd of distirct intercept, slope and their correlatio are same as model3 and model4
### the residual is same as model4, while the deviance decreases


#######################
### Model Checking ####
#######################


### 1) Anova Analysis

anova(model1, model2, model3)

### we can see model3 is a better fit compared with model1 and model2


anova(model3, model3.1, model3.2)

### we can see pvalue is 1 bigger than 0.05,
### indicating no difference of model3, model3.1, model3.2

anova(model3, model4, model4.1)
### we can see pvalue of model 4.1 is smaller than 0.05,
### indicating it is a better fit compared with model4 and model3

### In Conclusion, model4.1 is best fit among our models



### 2) Residual

### check the constant standard deviation of model4.1

plot(fitted(model4.1),resid(model4.1,type="pearson"),col="blue")

qqnorm(resid(model4.1))

### as we can see from the Q-Q plot,
### it doesn't show a normal distribution pattern
### but a bimodal pattern

### Therefore linear multilevel model may not be approporiate
### we can differenciate the airbnb weighted rating into two category
### above 2: then satisfied
### below 2: then unsatisfied
### Then we can fit a logistic model


#######################
##### Model Refit #####
#######################

### 1) we will start to add one binary variable into our data as our new response

AddSatisfaction<-function(data){
  result=data
  for (row in 1:nrow(result)){
    for (n in names(result)[16]){
      if (result[row,n]>=2){
        result$Satisfaction[row] = 1
      }else{
        result$Satisfaction[row] = 0
      }
    }
  }
  return(result)
}

pardata<-AddSatisfaction(parisdata)



### 2) we can start from fitting simple logistic model

### data visualization

ggplot(pardata)+aes(x=room_type,y=accommodates)+
  geom_jitter()+facet_grid(.~Satisfaction)+
  scale_fill_manual(values=c("blue","red"))+ylab("")+xlab("")

ggplot(pardata)+aes(x=bedrooms,y=accommodates)+
  geom_jitter()+facet_grid(.~Satisfaction)+
  scale_fill_manual(values=c("blue","red"))+ylab("")+xlab("")

ggplot(pardata)+aes(x=room_type,y=price)+
  geom_jitter()+facet_grid(.~Satisfaction)+
  scale_fill_manual(values=c("blue","red"))+ylab("")+xlab("")

### model fit1
fit1<-glm(Satisfaction~room_type+accommodates+
            bedrooms+cprice+violenceRate, family = binomial, data=pardata)
display(fit1)


### model fit1.1
### as we has seen in the EDA, there is correlation between room type and accommodate
### we can add interaction to our model

fit1.1<-glm(Satisfaction~room_type*accommodates+
            bedrooms+cprice+violenceRate, family = binomial, data=pardata)
display(fit1.1)


### model check

car::marginalModelPlot(fit1)
car::marginalModelPlot(fit1.1)

### as we can see from the marginal model plot
### model fit1.1 is a better fit compared with model fit1



### 3) so we can fit multilevel logistic model

ggplot(data=pardata, aes(x=Satisfaction))+geom_histogram()+
  ggtitle("Satisfaction")+facet_wrap(~district)

### first we can visualize the relationship between predictors and single variable
### group by district


ggplot(data=pardata, aes(x=price, y=Satisfaction, fill=district))+
  geom_smooth(method = "glm")+ggtitle("Satisfaction and Price")

ggplot(data=pardata, aes(x=accommodates, y=Satisfaction, fill=district))+
  geom_smooth(method = "glm")+ggtitle("Satisfaction and Accommodates")

ggplot(data=pardata, aes(x=bedrooms, y=Satisfaction, fill=district))+
  geom_smooth(method = "glm")+ggtitle("Satisfaction and Bedrooms")


### as we can see from the graphics
### the slope varies between district
### so we will start from varing slope models

fit2<-glmer(Satisfaction~accommodates*room_type+bedrooms+
              violenceRate+cprice+(1+cprice|district), family = binomial, data=pardata)

fit2.1<-glmer(Satisfaction~accommodates*room_type+bedrooms+
              violenceRate+cprice+(1+bedrooms|district), family = binomial, data=pardata)

### we can check this model by looking at residual plot

binnedplot(fitted(fit2),residuals(fit2, type="response"))
binnedplot(fitted(fit2.1),residuals(fit2.1, type="response"))

### we can see model fit2 is a better fit compared with model fit2.1



### 4) model diagnosis
### 4.1) Deviance and residual analysis
### now we are going to compare model fit1.1 and model fit2

deviance(fit1.1)
deviance(fit2)

binnedplot(fitted(fit1.1),residuals(fit1.1, type="response"))
binnedplot(fitted(fit2),residuals(fit2, type="response"))

### as we can see model fit2 is the best fit so far

### 4.2) we are going to do predictive checking
display(fit2)

newdata<-model.matrix(~accommodates*room_type+bedrooms+
                         violenceRate+cprice,data=pardata)

cf<-fixef(fit2)
coefhat<-as.matrix(coef(fit2)$district)
sigma.p.hat<-sigma.hat(fit2)$sigma$data
sigma.p.hat<-sigma.hat(fit2)$sigma$district

n<-nrow(newdata)
simpoints<-matrix(NA,n,1000)

for (i in 1:1000){
  ptide<-invlogit(newdata%*%t(coefhat))
  ytide<-rbinom(n,1,ptide)
  simpoints[,i]<-ytide
}

hist(simpoints)

### 
propsim<-sum(simpoints)/50406000
propdat<-sum(pardata$Satisfaction)/50406

print(propsim)
print(propdat)

### we can see model fit2 is pretty a good fit for our data




#######################
##### Impplication ####
#######################

display(fit2)

### Implication of fixed effect

cf<-fixef(fit2)
cf<-as.data.frame(cf)
colnames(cf)<-"coefficient"

### On average,
### the number of accommodates, room type, voilence rate and price
### have positive impact on the weighted ratings,
### among them, accommodates and room type are statistically significant
### (1) specifically, shared room tend to have higher ratings than private room
### and entire room
### (2) increase of accommodates could increase the weighted ratings of entire room;
### while decrease the ratings of shared room and private room
### (3) each unit increase of price deviate from average price over standard deviation
### will increase 0.03 ratings on average; while price is not statistically significant
### (4) the increase of violence rate also could increase ratings. As the most popular 
### districts generally have higher violence rate. While this predictor is not statistically
### significant
### (5) the number of bedrooms has negative impact on the weighted ratings, each unit increase
### of bedrooms will decrease 0.15 weighted ratings on average


### Implication of random effect

rand<-coef(fit2)$district
rand<-as.data.frame(rand)
randef<-rand[,c(1,7)]
colnames(randef)<-c("Intercept","Cprice")
randef$district<-c(1:20)
randef$district<-as.factor(randef$district)


### random intercept and random slope

ggplot(data = randef, aes(x=Intercept, y=Cprice, col=district))+
  geom_point(size=1.5)+
  scale_color_discrete(randef$district)+ggtitle("Random Effect Per District")+
  geom_vline(xintercept = 0)+geom_hline(yintercept = 0)


### (1) random intercept: we can see 2,3,4,5,6 districts have postive intercept, 
### which means they have higher weighted ratings with average price than 
### other districts when other variables are same

### (2) random slope: we can see 2,3,4,6,7,8,9 districts have negative slope,
### which means each unit increase of price deviating from average price over
### standard error will decrease their ratings, while other district have positive 
### slope


### clustering
### we can cluster 20 districts into four categories

### (1) Star District (positive random intercept and random slope): 
### district 5th
### Airbnbs in this district have higher weighted ratings generally,
### and there is still potential for airbnb hosts to increase the price 

### (2) Golden District(positive random intercept but negative random slope): 
### distict  2nd, 3rd, 4th, 6th, 7th
### Airbnbs in this district have higher weighted ratings generally,
### but they are already highly priced, further increase of price will decrease the ratings

### (3) Problem District (negative random intercept and random slope)
### district 8th, 9th
### Airbnbs in this district have lower weighted ratings generally,
### but they are already highly priced, further increase of price will decrease the ratings

### (4) Potential District (negative random intercept and random slope)
### district 1st, 10th, 11th, 12th, 13th, 14th, 15th, 16th, 17th, 18th, 19th, 20th
### Airbnbs in this district have lower weighted ratings generally,
### but there is still potential for airbnb hosts to increase the price



ggplot(data=pardata, aes(x=price, fill=district))+geom_density()
Price<-sqldf("SELECT district, Avg (price) as AvgPrice FROM pardata Group by 1 Order by 2")

ggplot(data = Price, aes(x=district, y=AvgPrice))+geom_col()

