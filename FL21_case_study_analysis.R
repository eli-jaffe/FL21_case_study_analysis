library(dplyr)
library(readxl)
library(e1071)
library(Hmisc)
library(MASS)
library(ggplot2)
library(formattable)

# set wd
setwd("YOURDIRECTORY")

# read in the data
data = read_xlsx("MCD_data.xlsx")

# the plan ####

# take a look to see if there are regional differences
# then, make a model to predict brokeness of machine
# eeturn is significant
# while machines are being redesigned, 
# if we take steps to reduce turnover or otherwise help employees, it will positiviely impact 
# machine uptime and thereby custsat


# work below ####

# let's try to get a handle on what the data look like. are there any obvious trends?
str(data)
data$eeturn_to_stafften = data$eeturn/data$stafften

# let's see what variables are significantly different by Region ####
# Region is the only factor variable we have
table.one = data %>%
       group_by(Region) %>%
       summarise("Franchise Age" = mean(franyr),
                 "Average Staff Size" = mean(stafften),
                 "Drive-thru" = mean(dthru),
                 "Employee Turnover" = mean(eeturn),
                 "Pct time broken" = mean(broke),
                 "Customer Satisfaction" = mean(custsat),
                 "Price" = mean(price)) %>%
  mutate_if(is.numeric, round, digits = 2)
  

ggplot(data, aes(x=eeturn, y=broke)) +
  geom_point() +
  geom_smooth(method = lm) +

  
ggplot(data, aes(x=eeturn, y=broke)) +
  geom_point() +
  geom_smooth(method = lm)

gplotEturnBroke = ggplot(data, aes(x=eeturn, y=broke)) +
  geom_point(col="lightblue") +
  geom_smooth(method=lm) +
  labs(title="Employee Turnover Vs Broke",
       y="Percent of time broken",
       x="Employee Turnover",
       caption = "Scatterplot of Broke by Employee Turnover with best fit line")

gplotEturnBrokeNoScatter = ggplot(data, aes(x=eeturn, y=broke)) +
  geom_smooth(col="#77B0DB",method=lm, se=FALSE) +
  coord_cartesian(ylim = c(0,30)) +
  labs(title="Relationship between Employee Turnover and Percentage of Broken Machines",
       y="Percent of Time Broken",
       x="Employee Turnover",
       caption = "Figure 1. Best fit line of the relationship between Employee Turnover and % of broken machines")

gplotEturnBroke
gplotEturnBroke + theme_classic()
gplotEturnBrokeNoScatter + theme_classic()

gplotCustSatBrokeNoScatter = ggplot(data, aes(x=custsat, y=broke)) +
  geom_smooth(col="#77B0DB",method=lm, se=FALSE) +
  coord_cartesian(ylim = c(0,30)) +
  labs(title="Relationship between Customer Satisfaction and Percentage of Broken Machines",
       y="Percent of Time Broken",
       x="Customer Satisfaction",
       caption = "Figure 2. Best fit line of the relationship between Customer Satisfaction and % of broken machines")

gplotCustSatBrokeNoScatter + theme_classic()

gplotCustsatBroke = ggplot(data, aes(x=custsat, y=broke)) +
  geom_point(col="salmon") +
  geom_smooth(method = lm) +
  labs(title="Customer Satisfaction Vs Broke",
       y="Percent of time broken",
       x="Customer Satisfaction",
       caption = "Scatterplot of Broke by Customer Satisfaction with best fit line"
       )
gplotCustsatBroke + theme_classic()

gplotPriceBroke = ggplot(data, aes(x=price, y=broke)) +
  geom_point(col="darkseagreen") +
  geom_smooth(method = lm) +
  labs(title="Order Price Vs Broke",
       y="Percent of time broken",
       x="Average Order Price",
       caption = "Scatterplot of Broke by Price with best fit line"
       )
gplotPriceBroke + theme_classic()

data %>%
  group_by(Region) %>%
  summarise(mean(franyr))

# not significant difference
anova(lm(franyr ~ Region, data))

# let's look at mean turnover by region
data %>%
  group_by(Region) %>%
  summarise(mean(eeturn))

# significant difference
eeturn_lm = lm(eeturn ~ Region, data)
anova(eeturn_lm)

# eeturn_to_stafften by region
data %>%
  group_by(Region) %>%
  summarise(mean(eeturn_to_stafften))

# significant
ee_to_stafften_lm = lm(eeturn_to_stafften ~ Region, data)
anova(ee_to_stafften_lm)

# now let's look at mean broke by region
View(data %>%
  group_by(Region) %>%
  summarise(mean(broke), median(broke), sd(broke), min(broke), max(broke), max(broke)- min(broke)))
# significant
anova(lm(broke ~ Region, data))

# this comes to same number as just stafften
data %>%
  group_by(Region) %>%
  summarise(n = n(), staff.per.store = sum(stafften)/n())

# we can see that North America seems to be an outlier on both turnover and pct time
# that ice cream machine is broken. what accounts for this? can we learn from other regions?

# staff size is pretty consistent across regions
data %>%
  group_by(Region) %>%
  summarise(mean(stafften))

# not significant
anova(lm(stafften ~ Region, data))

# drivethru is slightly higher in North America
data %>%
  group_by(Region) %>%
  summarise(mean(dthru))

# not significant
anova(lm(dthru ~ Region, data))

# customer satisfaction appears noticeably lower in NA
data %>%
  group_by(Region) %>%
  summarise(mean(custsat))

# significant
anova(lm(custsat ~ Region, data))

# price is higher in NA than other regions
data %>%
  group_by(Region) %>%
  summarise(mean(price))

# significant
anova(lm(price ~ Region, data))

# CONCLUSION: eeturn, broke, custsat, and price are significantly affected by Region
# North America has higher eeturn, broke, and price
# North America has lower custsat

# Now let's dive into correlations between variables ####
plot(data$franyr, data$broke)
cor(data$franyr, data$broke)

# eeturn is correlated positively with pct broke (cor = 0.347)
plot(data$eeturn, data$broke)
cor(data$eeturn, data$broke)

# pct broke is correlated negatively with custsat (cor = -0.437)
plot(data$broke, data$custsat)
cor(data$broke, data$custsat)

# eeturn is correlated negatively with custsat (cor = -0.141)
plot(data$eeturn, data$custsat)
cor(data$eeturn, data$custsat)

# eeturn is correlated strongly with broke. broke is correlated strongly with lower custsat
# eeturn is somewhat correlated with lower custsat



# THIS IS INTERESTING: globally, eeturn is correlated with broke pretty positively (r= 0.347)
# whereas in North America, it is actually slightly negatively correlated (r= -0.072)
# this will be reflected in the models below where globally, eeturn is significant predictor of broke
# whereas it is not significant in North American model 

# cor broke and custsat = -0.286
plot(north_america$broke, north_america$custsat)
cor(north_america$broke, north_america$custsat)

# eeturn and custsat = 0.098
plot(north_america$eeturn, north_america$custsat)
cor(north_america$eeturn, north_america$custsat)

# cor eeturn and price = -0.050
plot(north_america$eeturn, north_america$price)
cor(north_america$eeturn, north_america$price)

# cor eeturn and dthru = -0.084
plot(north_america$eeturn, north_america$dthru)
cor(north_america$eeturn, north_america$dthru)

# let's see if employee turnover and/or time broke differs by region
africa_data = subset(data, data$Region=='Africa')
africa_mean_eeturn = mean(africa_data$eeturn)

asia_data = data %>% subset(Region=='Asia')
asia_mean_eeturn = mean(asia_data$eeturn)


# Original work
plot(data$broke, data$custsat)
plot(data$eeturn, data$custsat)
hist(data$custsat)

# getting into modeling ####
# let's start with all the data. then we will repeat with just NA
boxplot(data$broke)
outliers = subset(data, data$broke %in% boxplot.stats(data$broke)$out)
hist(data$broke)
plot(outliers$eeturn, outliers$broke)
plot(data$eeturn, data$broke)
cor.test(data$eeturn, data$broke)

# after removing outliers, we still see a significant correlation between eeturn and broke
# r = 0.315, p < 2.2e-16
data_no_outliers = subset(data, !data$broke %in% boxplot.stats(data$broke)$out)
cor.test(data_no_outliers$eeturn, data_no_outliers$broke)
plot(data_no_outliers$eeturn, data_no_outliers$broke)

# pull in beautifying script
source(".../cor_matrix_stars.R")
cordata = data[c("RestID", "franyr", "stafften", "dthru", "eeturn", "broke", "custsat", "price")]
cor(cordata)
cmat_global_formatted <- cor_matrix_stars(df = cordata)
View(cmat_global_formatted)

# try linear regression
full_model = lm(broke ~ franyr + stafften + dthru + eeturn + custsat + price, data)
summary(full_model)
par(mfrow = c(2,2))
plot(full_model)   

backward_fit = stepAIC(full_model, direction = "backward")
summary(backward_fit)
plot(backward_fit)
