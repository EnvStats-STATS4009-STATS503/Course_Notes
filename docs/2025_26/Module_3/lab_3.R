## -----------------------------------------------------------------------------
#| eval: false
# load("lab_3_DataFiles.RData")


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
# Libraries for Data manipulation
library(tidyr)
library(dplyr)
library(sf)
library(terra)

# Libraries for producing maps
library(ggplot2)
library(scico)
library(tidyterra)
library(mapview)
library(patchwork)

# Libraries for the Analysis
library(spdep) 
library(gstat)
library(variosig)
library(INLA)
library(inlabru)


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
library(SpatialEpi)
data(scotland_sf)


## -----------------------------------------------------------------------------
#| echo: true

# Compute the SMR and add a region index (for later modelling)
scotland_sf <- scotland_sf %>% mutate(
  SMR = cases/expected,
  region_id = 1:nrow(scotland_sf))



## -----------------------------------------------------------------------------
# Visualize the regions colored by the SMR
ggplot()+geom_sf(data=scotland_sf,aes(fill=SMR))+scale_fill_scico(direction = -1)




## -----------------------------------------------------------------------------
#| message: false
#| warning: true

W.nb <- poly2nb(scotland_sf,queen = TRUE)
W.nb




## -----------------------------------------------------------------------------
# neighbors list 
nbw <- nb2listw(W.nb, style = "W",zero.policy = TRUE)


## -----------------------------------------------------------------------------

# Global Moran's I
gmoran <- moran.test(scotland_sf$SMR, nbw,
                     alternative = "greater")
gmoran



## -----------------------------------------------------------------------------
#| code-fold: show
R <- nb2mat(W.nb, style = "B", zero.policy = TRUE)
diag = apply(R,1,sum)
Q = -R
diag(Q) = diag


## -----------------------------------------------------------------------------
#| echo: true
#| code-fold: show

cmp = ~ Intercept(1) + beta_1(AFF, model = "linear") + 
   z_i(region_id , model = "iid") +
  u_i(region_id, model = "besag", graph = Q,scale.model = TRUE) 
 



## -----------------------------------------------------------------------------
#| echo: true
#| code-fold: show

formula = cases ~ Intercept + beta_1 + u_i + z_i


## -----------------------------------------------------------------------------
#| echo: true
#| code-fold: show
lik = bru_obs(formula = formula,

              family = "poisson",

              E = expected,

              data = scotland_sf)


## -----------------------------------------------------------------------------
#| echo: true
#| code-fold: show

fit = bru(cmp, lik)
summary(fit)



## -----------------------------------------------------------------------------
#| echo: false
#| fig-align: center
#| fig-width: 10
#| fig-height: 8

predict_result <- predict(fit, scotland_sf,formula ~ exp(Intercept+beta_1+u_i+z_i))

ggplot() +  geom_sf(data=predict_result,aes(fill=mean))+
  scale_fill_scico(name="Relative Risks",direction = -1)




## -----------------------------------------------------------------------------
#| message: false
#| warning: false
library(sdmTMB)
pcod = sdmTMB::pcod %>% filter(year==2003)
pcod_sf =   st_as_sf(pcod, coords = c("lon","lat"), crs = 4326)


## -----------------------------------------------------------------------------
pcod_sf = st_transform(pcod_sf,
                       crs = "+proj=utm +zone=9 +datum=WGS84 +no_defs +type=crs +units=km" )


## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
#| message: false
#| warning: false
#| 
pcod_sf %>% 
  mutate(present = as.factor(present)) %>%
mapview(zcol = "present",
        layer.name = "Occupancy status of Pacific Cod in 2003")




## -----------------------------------------------------------------------------
#| message: false
#| warning: false

qcs_grid = sdmTMB::qcs_grid
depth_r <- rast(qcs_grid, type = "xyz")



## -----------------------------------------------------------------------------
crs(depth_r) <- crs(pcod_sf)


## -----------------------------------------------------------------------------
#| fig-width: 8 
#| fig-height: 8
#| fig-align: center  

ggplot()+ 
  geom_spatraster(data=depth_r$depth)+
  geom_sf(data=pcod_sf,aes(color=factor(present))) +
    scale_color_manual(name="Occupancy status for the Pacific Cod",
                     values = c("black","orange"),
                     labels= c("Absence","Presence"))+
  scale_fill_scico(name = "Depth",
                   palette = "nuuk",
                   na.value = "transparent" )



## -----------------------------------------------------------------------------
ext(depth_r)[1:2] %>% diff() # difference in x coord
ext(depth_r)[3:4] %>% diff()  # difference in y coord


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| fig-align: center
#| fig-width: 4
#| fig-height: 4

vario_binned <- gstat::variogram(log(density) ~ depth_scaled  + depth_scaled2,
                                 data = pcod_sf %>% filter(density>0),
                                 cloud = FALSE,
                                 cutoff = 300)
plot(vario_binned)



## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| fig-align: center
#| fig-width: 4
#| fig-height: 4


varioEnv <- envelope(vario_binned,
                     data = pcod_sf %>% filter(density>0),
                     locations = st_coordinates(pcod_sf %>% filter(density>0)),
                     formula = log(density) ~ depth_scaled  + depth_scaled2,
                     nsim = 499)
envplot(varioEnv)



## -----------------------------------------------------------------------------

mesh = fm_mesh_2d(loc = pcod_sf,           # Build the mesh
                  max.edge = c(10,20),     # The largest allowed triangle edge length.
                  offset = c(5,50))       # The automatic extension distance
ggplot() + gg(mesh) + geom_sf(data= pcod_sf, aes(color = factor(present)), size = 0.1) + xlab("") + ylab("")




## -----------------------------------------------------------------------------

mesh = fm_mesh_2d(loc = pcod_sf,           # Build the mesh
                  cutoff = 2,
                  max.edge = c(10,20),     # The largest allowed triangle edge length.
                  offset = c(5,50))       # The automatic extension distance
ggplot() + gg(mesh) + geom_sf(data= pcod_sf, aes(color = factor(present)), size = 0.1) + xlab("") + ylab("")



## -----------------------------------------------------------------------------
#| eval: false

# ?fm_mesh_2d


## -----------------------------------------------------------------------------
spde_model1 =  inla.spde2.pcmatern(mesh,
                                  prior.sigma = c(.1, 0.5),
                                  prior.range = c(30, 0.5))
spde_model2 =  inla.spde2.pcmatern(mesh,
                                  prior.sigma = c(10, 0.5),
                                  prior.range = c(1000, 0.5))
spde_model3 =  inla.spde2.pcmatern(mesh,
                                  prior.sigma = c(1, 0.5),
                                  prior.range = c(100, 0.5))


## -----------------------------------------------------------------------------

cmp <- ~
   Intercept_biomass(1) +
    depth_biomass(depth_scaled, model = "linear") +
    depth2_biomass(depth_scaled2, model = "linear") +
    space_biomass(geometry, model = spde_model3) +
    Intercept_caught(1) +
    depth_caught(depth_scaled, model = "linear") +
    depth2_caught(depth_scaled2, model = "linear") +
    space_caught(geometry, model = spde_model3)



## -----------------------------------------------------------------------------

fml_1 = density ~  Intercept_biomass + depth_biomass + depth2_biomass + space_biomass
fml_2 = present ~ Intercept_caught + depth_caught + depth2_caught + space_caught
  
biomass_obs <- bru_obs(formula = fml_1,
      family = "lognormal",
      data = pcod_sf  %>% filter(density>0)) # restrict to those locations where fish were caught

presence_obs <- bru_obs(formula = fml_2 ,
  family = "binomial",
  data = pcod_sf,
)



## -----------------------------------------------------------------------------

fit_hurdle <- bru(
  cmp,
  biomass_obs,
  presence_obs
)

summary(fit_hurdle)



## -----------------------------------------------------------------------------
pxl1 = data.frame(crds(depth_r), 
                  as.data.frame(depth_r)) %>% 
       filter(!is.na(depth)) %>%
st_as_sf(coords = c("x","y"),crs=st_crs(pcod_sf)) 



## -----------------------------------------------------------------------------
pred <- predict( fit_hurdle , pxl1,
  ~ {
    pi <- plogis(Intercept_caught + depth_caught + depth2_caught + space_caught)  # catching probability
    mu_log  <-  Intercept_biomass + depth_biomass + depth2_biomass + space_biomass  
    sd <- sqrt(1/Precision_for_the_lognormal_observations)
    conditional_mean <- exp(mu_log + 0.5 * sd^2)  
    dens <- pi * conditional_mean # biomass density
    list(
      pi = pi,
      conditional_mean = conditional_mean,
      dens = dens)
  },n.samples = 2500)



## -----------------------------------------------------------------------------
ggplot() + gg(pred$pi, geom = "tile",aes(fill = mean)) + scale_fill_scico(palette = "roma") + ggtitle("Posterior mean for catch probability")

ggplot() + gg(pred$dens, geom = "tile",aes(fill = mean))+ scale_fill_scico(palette="lapaz") + ggtitle("Posterior mean of biomass density ")

ggplot() + gg(pred$dens, geom = "tile",aes(fill = sd)) + scale_fill_scico(palette = "tokyo") + ggtitle("Posterior sd biomass density")


## -----------------------------------------------------------------------------
data(gorillas_sf, package = "inlabru")


## -----------------------------------------------------------------------------
nests <- gorillas_sf$nests
mesh <- gorillas_sf$mesh
boundary <- gorillas_sf$boundary
gcov <- gorillas_sf_gcov()
elev <- gcov$elevation
elev <- elev - mean(terra::values(elev), na.rm = TRUE) #scale the  covariate


## -----------------------------------------------------------------------------
ggplot() +
  gg(elev) +
  gg(boundary, alpha = 0.2) +
  gg(nests, color = "white", cex = 0.5)+
  scale_fill_scico(palette = "lisbon")



## -----------------------------------------------------------------------------
mesh <- gorillas_sf$mesh

ggplot() + gg(mesh) + geom_sf(data = nests)

spde_model =  inla.spde2.pcmatern(mesh,
  prior.sigma = c(0.1, 0.01),
  prior.range = c(0.1, 0.01)
)



## -----------------------------------------------------------------------------
ips = fm_int(mesh, samplers = boundary)

ggplot() + geom_sf(data = ips, aes(color = weight)) +
  gg(mesh) +
   scale_color_scico(palette = "roma")



## -----------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 4
#| fig-align: center
#| code-fold: show


cmp = ~ Intercept(1) + space(geometry, model = spde_model) + elev(elev, model = "linear")

formula = geometry ~ Intercept + space + elev


lik = bru_obs("cp",
              formula = formula,
              data = nests,
              ips = ips)

fit_gorillas = bru(cmp, lik)

summary(fit_gorillas)



## -----------------------------------------------------------------------------
pred.df <- fm_pixels(mesh, mask = boundary)


## -----------------------------------------------------------------------------
#| message: false
#| warning: false

e.pred <- predict(
  fit_gorillas,
  pred.df,
  ~ list(
    int = exp(space + elev + Intercept),
    int.log = space + elev + Intercept
  )
)

p1 <- ggplot() +
  gg(e.pred$int, aes(fill = log(sd)), geom = "tile") +
  gg(boundary, alpha = 0) +
  gg(nests, shape = "+") 
p2 <- ggplot() +
  gg(e.pred$int.log, aes(fill = exp(mean + sd^2 / 2)), geom = "tile") +
  gg(boundary, alpha = 0) +
  gg(nests, shape = "+")
(p1 | p2)


## -----------------------------------------------------------------------------

Nest.e <- predict(
  fit_gorillas,
  fm_int(mesh, boundary),
  ~ data.frame(
    N = 400:900,
    density = dpois(400:900,
      lambda = sum(weight * exp(space + elev + Intercept))
    )
  ),
  n.samples = 2000
)





## -----------------------------------------------------------------------------

ggplot(data = Nest.e) +
  geom_line(aes(x = N, y = mean, colour = "Posterior")) +
    geom_vline(xintercept = nrow(nests),
             colour = "red") +
  xlab(expression(Lambda))

