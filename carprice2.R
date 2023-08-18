## 1. 사용할 패키지 불러오기
library(ggplot2)
library(dplyr)
library(patchwork)
library(corrplot)

## 2. 데이터 불러오기
### Step 1: Reading and Understanding the Data
car = read.csv("CarPrice_Assignment.csv", stringsAsFactors = T)
dim(car)
summary(car)

### Step 2 : Data Cleaning and Preparation
CarName = strsplit(as.character(car$CarName), split = ' ')
CompanyName = c()
for(i in 1:length(CarName)){
  CompanyName = c(CompanyName, CarName[[i]][1])
}
car$CarName = CompanyName
colnames(car)[3] = "CompanyName"
unique(car$CompanyName)

# There seems to be some spelling error in the CompanyName column.
car$CompanyName = tolower(car$CompanyName)
car$CompanyName[car$CompanyName == 'maxda'] = 'mazda'
car$CompanyName[car$CompanyName == 'porcshce'] = 'porsche'
car$CompanyName[car$CompanyName == 'toyouta'] = 'toyota'
car$CompanyName[car$CompanyName == 'vokswagen'] = 'volkswagen'
car$CompanyName[car$CompanyName == 'vw'] = 'volkswagen'
car$CompanyName = factor(car$CompanyName)
unique(car$CompanyName)


### Step 3: Visualizing the data
# Price
ggplot(car, aes(x=price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


ggplot(car, aes(y=price)) + 
  geom_boxplot()

# Step 3.1 : Visualising Categorical Data
ggplot(car, aes(CompanyName)) +
  geom_bar(aes(fill = CompanyName))

ggplot(car, aes(fueltype)) +
  geom_bar(aes(fill = fueltype))

ggplot(car, aes(carbody)) +
  geom_bar(aes(fill = carbody))

# Toyota seemed to be favored car company.
# Number of gas fueled cars are more than diesel.
# sedan is the top car type prefered.

###############
car$symboling = factor(car$symboling)
ggplot(car, aes(symboling)) +
  geom_bar(aes(fill = symboling))

ggplot(car, aes(x = symboling, y = price)) +
  geom_boxplot(aes(fill = symboling))

# It seems that the symboling with 0 and 1 values have high number of rows (i.e. They are most sold.)
# The cars with -1 symboling seems to be high priced (as it makes sense too, insurance risk rating -1 is quite good). But it seems that symboling with 3 value has the price range similar to -2 value. There is a dip in price at symboling 1.


###############
car$enginetype = factor(car$enginetype)
ggplot(car, aes(enginetype)) +
  geom_bar(aes(fill = enginetype))

ggplot(car, aes(x = enginetype, y = price)) +
  geom_boxplot(aes(fill = enginetype))

group_car = car %>% group_by(enginetype) %>% summarise(price = mean(price))

ggplot(group_car, aes(x = enginetype, y = price)) +
  geom_bar(stat = 'identity')

# ohc Engine type seems to be most favored type.
# ohcv has the highest price range (While dohcv has only one row), ohc and ohcf have the low price range."

###############
group_car = car %>% group_by(CompanyName) %>% summarise(price = mean(price))
ggplot(group_car, aes(x = CompanyName, y = price)) +
  geom_bar(stat = 'identity')

group_car = car %>% group_by(fueltype) %>% summarise(price = mean(price))
ggplot(group_car, aes(x = fueltype, y = price)) +
  geom_bar(stat = 'identity')

group_car = car %>% group_by(carbody) %>% summarise(price = mean(price))
ggplot(group_car, aes(x = carbody, y = price)) +
  geom_bar(stat = 'identity')

# Jaguar and Buick seem to have highest average price.
# diesel has higher average price than gas.
# hardtop and convertible have higher average price.

###############
ggplot(car, aes(doornumber)) +
  geom_bar(aes(fill = doornumber))

ggplot(car, aes(x = doornumber, y = price)) +
  geom_boxplot(aes(fill = doornumber))

ggplot(car, aes(aspiration)) +
  geom_bar(aes(fill = aspiration))

ggplot(car, aes(x = aspiration, y = price)) +
  geom_boxplot(aes(fill = aspiration))

# doornumber variable is not affacting the price much. There is no sugnificant difference between the categories in it.
# It seems aspiration with turbo have higher price range than the std(though it has some high values outside the whiskers.)

###############
ggplot(car, aes(enginelocation)) +
  geom_bar(aes(fill = enginelocation))
ggplot(car, aes(x = enginelocation, y = price)) +
  geom_boxplot(aes(fill = enginelocation))

ggplot(car, aes(cylindernumber)) +
  geom_bar(aes(fill = cylindernumber))
ggplot(car, aes(x = cylindernumber, y = price)) +
  geom_boxplot(aes(fill = cylindernumber))

ggplot(car, aes(fuelsystem)) +
  geom_bar(aes(fill = fuelsystem))
ggplot(car, aes(x = fuelsystem, y = price)) +
  geom_boxplot(aes(fill = fuelsystem))

ggplot(car, aes(drivewheel)) +
  geom_bar(aes(fill = drivewheel))
ggplot(car, aes(x = drivewheel, y = price)) +
  geom_boxplot(aes(fill = drivewheel))

# Very few datapoints for enginelocation categories to make an inference.
# Most common number of cylinders are four, six and five. Though eight cylinders have the highest price range.
# mpfi and 2bbl are most common type of fuel systems. mpfi and idi having the highest price range. But there are few data for other categories to derive any meaningful inference
# A very significant difference in drivewheel category. Most high ranged cars seeme to prefer rwd drivewheel.

### Step 3.2 : Visualising numerical data
p1 = ggplot(car, aes(carlength, price)) +
  geom_point()
p2 = ggplot(car, aes(carlength, price)) +
  geom_point()
p3 = ggplot(car, aes(carlength, price)) +
  geom_point()
p4 = ggplot(car, aes(carlength, price)) +
  geom_point()
p1 + p2 + p3 + p4

# carwidth, carlength and curbweight seems to have a poitive correlation with price.
# carheight doesn't show any significant trend with price.


###############
p1 = ggplot(car, aes(enginesize, price)) +
  geom_point()
p2 = ggplot(car, aes(boreratio, price)) +
  geom_point() + 
  theme(axis.title.y=element_blank(), axis.text.y = element_blank())
p3 = ggplot(car, aes(stroke, price)) +
  geom_point() + 
  theme(axis.title.y=element_blank(), axis.text.y = element_blank())
p1 + p2 + p3

p1 = ggplot(car, aes(compressionratio, price)) +
  geom_point()
p2 = ggplot(car, aes(horsepower, price)) +
  geom_point() + 
  theme(axis.title.y=element_blank(), axis.text.y = element_blank())
p3 = ggplot(car, aes(peakrpm, price)) +
  geom_point() + 
  theme(axis.title.y=element_blank(), axis.text.y = element_blank())
p1 + p2 + p3

p1 = ggplot(car, aes(wheelbase, price)) +
  geom_point()
p2 = ggplot(car, aes(citympg, price)) +
  geom_point() + 
  theme(axis.title.y=element_blank(), axis.text.y = element_blank())
p3 = ggplot(car, aes(highwaympg, price)) +
  geom_point() + 
  theme(axis.title.y=element_blank(), axis.text.y = element_blank())
p1 + p2 + p3

# enginesize, boreratio, horsepower, wheelbase - seem to have a significant positive correlation with price.
# citympg, highwaympg - seem to have a significant negative correlation with price.

cor(car['carlength'], car['carwidth'])

### Step 4 : Deriving new features
car['fueleconomy'] = (0.55 * car['citympg']) + (0.45 * car['highwaympg'])

#Binning the Car Companies based on avg prices of each Company.
group_car = car %>% group_by(CompanyName) %>% summarise(price = mean(price))
group_car$carsrange = ifelse(group_car$price < 10000, 'Budget', ifelse(group_car$price < 20000, 'Medium', 'Highend'))
car = merge(car, group_car[, c(1,3)], by = 'CompanyName')
car$carsrange = factor(car$carsrange)

### Step 5 : Bivariate Analysis
ggplot(car, aes(fueleconomy, price, col = drivewheel)) +
  geom_point()

# fueleconomy has an obvios negative correlation with price and is significant.


### List of significant variables after Visual analysis
cars_lr = car[, c('price', 'fueltype', 'aspiration','carbody', 'drivewheel','wheelbase',
                  'curbweight', 'enginetype', 'enginesize', 'boreratio','horsepower', 
                  'fueleconomy', 'carlength','carwidth', 'carsrange')]
head(cars_lr)

pairs(cars_lr[,c('price', 'wheelbase', 'curbweight', 'enginesize', 'boreratio', 'horsepower', 'fueleconomy', 'carlength', 'carwidth')])

### Step 6 : Min max 
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
cars_lr$price = normalize(cars_lr$price)
cars_lr$wheelbase = normalize(cars_lr$wheelbase)
cars_lr$curbweight = normalize(cars_lr$curbweight)
cars_lr$enginesize = normalize(cars_lr$enginesize)
cars_lr$boreratio = normalize(cars_lr$boreratio)
cars_lr$horsepower = normalize(cars_lr$horsepower)
cars_lr$fueleconomy = normalize(cars_lr$fueleconomy)
cars_lr$carlength = normalize(cars_lr$carlength)
cars_lr$carwidth = normalize(cars_lr$carwidth)


### Step 7 : Train-Test Split and feature scaling
set.seed(10)
n_test = as.integer(length(cars_lr$price) * 0.3 )

test_sample = sample(1:length(cars_lr$price), n_test, replace = F)
train_sample = setdiff(1:length(cars_lr$price), test_sample)

df_train = cars_lr[train_sample, ]
df_test = cars_lr[test_sample, ]

cor_train = cor(df_train[, c('price', 'wheelbase', 'curbweight', 'enginesize', 'boreratio', 'horsepower', 'fueleconomy', 'carlength', 'carwidth')])
corrplot(cor_train, method = 'num')

### Step 8 : Model Building
lr_model = lm(price ~ . , df_train)
summary(lr_model)
vif(lr_model)

# Remove variables with high vif ( > 10)
df_train = df_train[, c('price', 'fueltype', 'aspiration','carbody', 'drivewheel','wheelbase',
                        'boreratio','fueleconomy', 'carwidth', 'carsrange')]

df_test = df_test[, c('price', 'fueltype', 'aspiration','carbody', 'drivewheel','wheelbase',
                      'boreratio','fueleconomy', 'carwidth', 'carsrange')]

lr_model = lm(price ~ . , df_train)
summary(lr_model)
vif(lr_model)

y_pred = predict(lr_model, df_test)
result = data.frame(y_pred, df_test$price)
colnames(result) = c('y_pred', 'y_test')

ggplot(result, aes(y_test, y_pred)) +
  geom_point() +
  ggtitle('y_test vs y_red')
