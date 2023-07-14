library(readr)
yelp_data_tip_weather <- read_csv("~/UA/Erasmus/SoSe/Data Science and Marketing Analytics/Proyecto/Missing Values/yelp_data_tip_weather.csv")
View(yelp_data_tip_weather)

setwd("C://Users/Lucas/Documents/UA/Erasmus/SoSe/Data Science and Marketing Analytics/Proyecto/susa")
DailyLvl_data <- read.csv("tips_2018-01-01-2018-11-14-Akron_data (3).csv",header=TRUE, na.strings="NA") #read csv file. Be aware of the parameters, such as the string for NA's. Usually either "" or "\\NA".
cum_u_names=DailyLvl_data$cum_u_names
Nobs=length(cum_u_names)


## now dealing with the missings.
yelp_data_tip_weather$n_photo[is.na(yelp_data_tip_weather$n_photo)]=0

yelp_data_sub <- subset(yelp_data_tip_weather,select = -c(business_id)) # removed this because MICE does not like imputing factors with more than 50 levels

library(mice)

#inspect pattern of missings
md.pattern(yelp_data_sub)

#Below, the predictormatrix is specified.
#It is a square matrix of size ncol(data) containing 0/1 data specifying the set of predictors to be used for each target column.
#Rows correspond to target variables (i.e. variables to be imputed), in the sequence as they appear in data.
#A value of '1' means that the column variable is used as a predictor for the target variable (in the rows).
#The diagonal of predictorMatrix must be zero.
predictorMatrix <- matrix(0,nrow = ncol(yelp_data_sub), ncol = ncol(yelp_data_sub)) # Make a matrix of zeros
colnames(predictorMatrix)=colnames(yelp_data_sub)
row.names(predictorMatrix)=colnames(yelp_data_sub)
predictorMatrix[c("business_price"),] <- 1 #variables "business_price" can be explained by all other variables
diag(predictorMatrix) <- 0 #diagonal must be zero

#impute data
yelp_data_imputed <- mice(yelp_data_sub, predictorMatrix = predictorMatrix, m=5, maxit = 50, seed = 500)

summary(yelp_data_imputed)

#get one of the complete data sets ( 2nd out of 5)
yelp_data_complete <- complete(yelp_data_imputed,2)

# bring back the business_id
yelp_data_complete=cbind(business_id=yelp_data_tip_weather$business_id,yelp_data_complete)



DailyLvl_data=DailyLvl_data%>%
  inner_join(yelp_data_complete,by="business_id")

#####HASTA AQUI TOODO OK######################################################################################


# make factors out of chr variables
for(j in 1:ncol(DailyLvl_data)){
  if(typeof(DailyLvl_data[,j])=="character")
    DailyLvl_data[,j]=as.factor(DailyLvl_data[,j])
}

# limit the number of categories to Asian, American, Mexican and Others

categories=as.character(DailyLvl_data$business_cat)
new_categories=c("Mexican","Others","American","Asian")

changed=0
for(k in new_categories[-1]){
  categories[grepl(k,categories)]=k
  changed=changed+grepl(k,categories)
}
categories[changed==0]="Others"
DailyLvl_data$business_cat=as.factor(categories)

# n_photos==NA and cum_max_u_elite==NA are actually zeros, let's replace them with 0 before imputing.

DailyLvl_data$cum_max_u_elite.x[is.na(DailyLvl_data$cum_max_u_elite.x)]=0

# some descriptives of the data
describe(DailyLvl_data)
