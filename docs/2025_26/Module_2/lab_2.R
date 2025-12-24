## -----------------------------------------------------------------------------
#| message: false
#| warning: false

# Load required R packages:

library(ggplot2)    # For visualizing our data
library(broom)      # For model predictions


# Read in data (making sure to set the working directory to the 
# appropriate location):
SO2 <- read.csv("datasets/SO2.csv", header = TRUE)

# Examine structure of the dataset:
str(SO2)

# Add year as a decimal (continuous variable):
SO2$year_num <- SO2$Years + SO2$Weeks / 53


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| fig-align: center
#| fig-width: 4.5
#| fig-height: 4.5

ggplot(data = SO2,
       aes(y = ln.SO2., x = Weeks ))+
  geom_point()+
  geom_smooth(method = "loess",span=0.75,aes(color="0.75"))+
  geom_smooth(method = "loess",span=0.5,aes(color="0.5"))+
    geom_smooth(method = "loess",span=0.3,aes(color="0.3"))+
  labs(y=expression(log~SO^2))





## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| fig-align: center
#| fig-width: 4.5
#| fig-height: 4.5
#| 
ggplot(data = SO2,
       aes(y = ln.SO2., x = year_num ))+
  geom_point()+
  geom_smooth(method = "loess",aes(color="0.75"))



## -----------------------------------------------------------------------------
# Create a model with seasonal pattern (harmonic model) plus trend:
harmonic_model <- lm(ln.SO2. ~ sin(2 * pi * (Weeks - 53) / 53) + 
              cos(2 * pi * (Weeks - 53) / 53) + year_num,
            data = SO2)



## -----------------------------------------------------------------------------
summary(harmonic_model)


## -----------------------------------------------------------------------------
# Create a prediction grid over the weeks of the years 1989 to 2000:
pred_data <- expand.grid(Weeks = 1:53, year = 1989:2000)
pred_data$year_num <- pred_data$year + pred_data$Weeks / 53


## -----------------------------------------------------------------------------

# Model predictions with confidence interval for the mean

pred_mean = broom::augment(x=harmonic_model,
                     newdata = pred_data,
                     interval = "confidence",
                     type.predict = "response")

# Model predictions with prediction interval for the new observations

pred_obs = broom::augment(x=harmonic_model,
                     newdata = pred_data,
                     interval = "prediction",
                     type.predict = "response")

  


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 4.5
#| fig-height: 4.5
#| 
ggplot() +
  geom_line(data=SO2,aes(y=ln.SO2.,x=year_num),alpha=0.25)+
  geom_ribbon(data= pred_obs,
                aes(x=year_num,
                    ymin = .lower,
                    ymax = .upper,
                    fill="Prediction Interval"),
                alpha = 0.15)+
    geom_ribbon(data= pred_mean,
                aes(x=year_num,
                    ymin = .lower,
                    ymax = .upper,
                    fill="Confidence Interval"),
                alpha = 0.5)+
  geom_line(data=pred_mean,
            aes(y=.fitted,x=year_num),
            color="tomato")+
  scale_fill_discrete(name="")+
  labs(y=expression(log~SO^2),x="Time")


## -----------------------------------------------------------------------------
layout(matrix(1:4, nrow = 2, byrow = TRUE))
plot(harmonic_model)


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 4.5
#| fig-height: 4.5
acf(resid(harmonic_model))


## -----------------------------------------------------------------------------
# Read in data:
haddock.all <- read.table("datasets/haddock.dat",header=TRUE)
# Remove the data from year 2001 onwards:
haddock.data <- haddock.all[haddock.all$Year <= 2000,]







## -----------------------------------------------------------------------------
# Linear trend model:

## Set up data by creating a Time index 1:38 years
haddock.data$Time <- haddock.data$Year-1962
## Fit model:
trend.model0 <- lm(log10(Biomass) ~ Time, data = haddock.data)
summary(trend.model0)



## -----------------------------------------------------------------------------
## Predict for years 2001 to 2010, i.e., time index 39:48
pred.t <- data.frame(Time = 39:48)	


## -----------------------------------------------------------------------------
trend_preds = broom::augment(x=trend.model0,
                     newdata = pred.t,
                     interval = "prediction",
                     type.predict = "response")

# This will allow a plot labelled with "Year"
trend_preds$Year <- 1962 + trend_preds$Time 


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 4.5
#| fig-height: 4.5
ggplot()+
  geom_line(data=haddock.data,
            aes(x=Year,y=log10(Biomass)),
            alpha=0.25)+
   geom_ribbon(data= trend_preds,
                aes(x=Year,
                    ymin = .lower,
                    ymax = .upper),alpha=0.25)+
  geom_line(data=trend_preds,aes(x=Year,y=.fitted),color="tomato")


## -----------------------------------------------------------------------------
trend_preds[c(6,10),]


## -----------------------------------------------------------------------------
exp(trend_preds[c(6,10),2:4])


## -----------------------------------------------------------------------------
layout(matrix(1:4, nrow = 2, byrow = TRUE))
# Check model diagnostic plots:
plot(trend.model0, which = 1:4)


## -----------------------------------------------------------------------------
layout(matrix(1:3, nrow = 1, byrow = TRUE))
e0 <- resid(trend.model0)

plot(haddock.data$Year, e0, type = "l", xlab = "Year", 
     ylab = "Residual", main = "Residual time series")
abline(h = 0, lty=2)
acf(e0,main="Residual ACF")
pacf(e0,main="Residual PACF")



## -----------------------------------------------------------------------------
#| message: false
#| warning: false

library(forecast)

ar1_model <- Arima(log10(haddock.data$Biomass), order=c(1,0,0),
                   xreg = haddock.data$Time,
                   include.constant = TRUE) # Include intercept

summary(ar1_model)



## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 4.5
#| fig-height: 4.5
forecast_result <- forecast::forecast(
  ar1_model,
  xreg = trend_preds$Time,
  h = 10  # forecast horizon
)
autoplot(forecast_result)


## -----------------------------------------------------------------------------
layout(matrix(1:2, nrow = 1, byrow = TRUE))

# Check model diagnostic plots:
e1 <- resid(ar1_model)
acf(e1, main = "Residual ACF for AR(1) model")
pacf(e1, main = "Residual PACF for AR(1) model")


## -----------------------------------------------------------------------------
layout(matrix(1:2, nrow = 2, byrow = TRUE))

plot(resid(ar1_model), type = "o")
qqnorm(resid(ar1_model))
qqline(resid(ar1_model))


## -----------------------------------------------------------------------------
#| message: false
#| warning: false

library(extRemes)  # library for GEV models
library(lubridate) # library to work with dates and time
library(tidyr)     # library for data tidying
library(dplyr)     # library for data manipulation 

#Read the data
ewe <- read.table("datasets/ewe_dailyflow_noleap.txt",header=T)



## -----------------------------------------------------------------------------
ewe <- ewe %>% mutate(
   log_flow = log(flow),  # we'll work with logged data
   date = make_date(year, month, day))


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 4.5
#| fig-height: 4.5
ggplot(data=ewe,aes(y=log_flow,x=date))+
  geom_line(alpha=0.35)+
labs(y="log flow")


## -----------------------------------------------------------------------------
ewe_max <- ewe %>% summarise(flow_max= max(log_flow),.by=year)




## -----------------------------------------------------------------------------
# GEV model fitting
fit_gev <- fevd(ewe_max$flow_max,method="MLE")
results <- summary(fit_gev)


## -----------------------------------------------------------------------------
ci(fit_gev, alpha = 0.05, type = c("parameter"))


## -----------------------------------------------------------------------------
return.level(fit_gev,return.period = c(10,50,100))


## -----------------------------------------------------------------------------
ci(fit_gev, alpha = 0.05, type = c("return.level"),return.period = c(10,50,100))

