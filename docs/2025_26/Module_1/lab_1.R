## -----------------------------------------------------------------------------
#| message: false
#| warning: false

# Load required R packages:

library(NADA)       # for analyzing censored observations
library(ggplot2)    # For visualizing our data
library(patchwork)  # For plotting multiple ggplot objects together

# Read in data (making sure to set the working directory to the 
# appropriate location):
Soar <- read.csv("datasets/SiteSoar.csv", header = TRUE)

# Examine structure of the dataset:
dim(Soar)
str(Soar)



## -----------------------------------------------------------------------------
ggplot(data = Soar,aes(y = Ammonia, x = year))+geom_point()+
  ggplot(data = Soar,aes(y = Ammonia, x = month))+geom_point()+
  ggplot(data = Soar,aes(y = Ammonia, x = day))+geom_point()




## -----------------------------------------------------------------------------
# Further summary statistics:
pctCen(Soar$Ammonia, Soar$censored)     ## percent of censored data
censummary(Soar$Ammonia, Soar$censored) ## like summary cmd but for 
                                        ## censored data


## -----------------------------------------------------------------------------
## 1. ROS:
ROS <- cenros(Soar$Ammonia, Soar$censored) ## constructs an object of 
                                           ## class c("ros", "lm")
plot(ROS)    ## probability plot
plot(ROS, plot.censored = TRUE) ## plots the modelled censored 
    


## -----------------------------------------------------------------------------
summary(ROS) ## more info about the ROS regression


## -----------------------------------------------------------------------------
print(ROS)   ## prints a simple summary of the ROS model.


## -----------------------------------------------------------------------------
## 2. Kaplan-Meier:
KM <- cenfit(Soar$Ammonia, Soar$censored)  ## constructs a Kaplan-Meier model
plot(KM)   ## survival function plot


## -----------------------------------------------------------------------------
print(KM)


## -----------------------------------------------------------------------------
## 3. MLE
MLE <- cenmle(Soar$Ammonia, Soar$censored) ## constructs a Maximum Likelihood model
plot(MLE)


## -----------------------------------------------------------------------------
summary(MLE)
print(MLE)


## -----------------------------------------------------------------------------
censtats(Soar$Ammonia, Soar$censored)



## -----------------------------------------------------------------------------
fx_lod = function(lod,mean,sd) {
    repeat {
      x <- rnorm(1, mean, sd) # generate a value from N(mu,sigma)
      if (x >= 0 && x <= lod) # repeat unless the generated value is >=0 and <LoD
        return(x)   
    }
}


## -----------------------------------------------------------------------------
fx_lod(0.3,mean(ROS),sd(ROS))


## -----------------------------------------------------------------------------

Soar$imputed.ROS <- ifelse(
  Soar$censored == F,
  Soar$Ammonia,  # Keep original if not censored 
  # otherwise apply the fx_lod function for each censored observation
  sapply(Soar$Ammonia[Soar$censored], fx_lod, mean = mean(ROS), sd = sd(ROS))
)



## -----------------------------------------------------------------------------
#| fig-align: center
ggplot(data=Soar,aes(y=Ammonia,x=doy,color=censored))+
  geom_point() +
  scale_color_discrete(name="Censored")+
ggplot(data=Soar,aes(y=imputed.ROS,x=doy,color=censored))+
  geom_point() + scale_color_discrete(name="Imputed")


## -----------------------------------------------------------------------------
#| fig-align: center

Soar$year.day <- Soar$year + Soar$doy / 366

ggplot(data=Soar,aes(y=Ammonia,x=year.day,color=censored))+
  geom_point() +
  scale_color_discrete(name="Censored")+
ggplot(data=Soar,aes(y=imputed.ROS,x=year.day,color=censored))+
  geom_point() + scale_color_discrete(name="Imputed")




## -----------------------------------------------------------------------------
library(spsurvey)
library(mapview)
data("NE_Lakes")


## -----------------------------------------------------------------------------
mapview(NE_Lakes,zcol="AREA_CAT")




## -----------------------------------------------------------------------------
eqprob_irs <- irs(NE_Lakes, n_base = 50)


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 4.5
#| fig-height: 4.5
ggplot() +
  geom_sf(data=NE_Lakes,aes(color="Not Selected")) +
  geom_sf(data=eqprob_irs$sites_base,aes(color="Selected"))


## -----------------------------------------------------------------------------
eqprob <- grts(NE_Lakes, n_base = 50)


## -----------------------------------------------------------------------------
plot(eqprob)


## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data=eqprob$sites_base)


## -----------------------------------------------------------------------------
mapview(eqprob$sites_base)


## -----------------------------------------------------------------------------
n_strata <- c(low = 35, high = 15)

eqprob_strat <- grts(NE_Lakes, n_base = n_strata,
                     stratum_var = "ELEV_CAT")


## -----------------------------------------------------------------------------
mapview(eqprob$sites_base,
        map.types = c("Esri.WorldShadedRelief"),
        col.regions = "tomato",
        layer.name="GTRS sampling")+
mapview(eqprob_strat$sites_base,
        map.types = c("Esri.WorldShadedRelief"),
        col.regions = "purple",
        layer.name="Stratified GTRS sampling")


## -----------------------------------------------------------------------------
caty_n <- c(small = 10, large = 40)
uneqprob <- grts(NE_Lakes, n_base = 50, caty_n = caty_n, caty_var = "AREA_CAT")


## -----------------------------------------------------------------------------
mapview(uneqprob$sites_base,
        zcol="AREA_CAT",
        map.types = c("Esri.WorldShadedRelief"),
        layer.name="GTRS sampling with unequal inclusion probabilities")


## -----------------------------------------------------------------------------
propprob <- grts(NE_Lakes, n_base = 50, aux_var = "AREA")



## -----------------------------------------------------------------------------
mapview(propprob$sites_base,
                zcol="AREA",
        map.types = c("Esri.WorldShadedRelief"),
        layer.name="GTRS sampling with PPS sampling")


## -----------------------------------------------------------------------------
sp_balance(eqprob$sites_base, NE_Lakes)
sp_balance(eqprob_irs$sites_base, NE_Lakes)

