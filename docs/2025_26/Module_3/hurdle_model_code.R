

# Preamble ----------------------------------------------------------------

library(sf)
library(INLA)
library(inlabru)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scico)
library(viridis)
library(patchwork)

pcod_df = sdmTMB::pcod %>% filter(year==2003)
qcs_grid = sdmTMB::qcs_grid
pcod_sf =   st_as_sf(pcod_df, coords = c("lon","lat"), crs = 4326)
pcod_sf = st_transform(pcod_sf,
                       crs = "+proj=utm +zone=9 +datum=WGS84 +no_defs +type=crs +units=km" )
library(terra)
depth_r <- rast(qcs_grid, type = "xyz")
crs(depth_r) <- crs(pcod_sf)
library(tidyterra)
ggplot()+ 
  geom_spatraster(data=depth_r$depth)+
  geom_sf(data=pcod_sf,aes(color=factor(present))) +
  scale_color_manual(name="Locations where Pacific Cod \nwere caught",
                     values = c("black","orange"),
                     labels= c("Absence","Presence"))+
  scale_fill_scico(name = "Depth",
                   palette = "nuuk",
                   na.value = "transparent" ) + xlab("") + ylab("")


# Tweedie Model (no spatial) ----------------------------------------------


# There are some convergence iossues when adding a spde model...

bru_options_set(control.compute = list(dic = TRUE,
                                       waic = TRUE,
                                       mlik = TRUE,
                                       cpo = TRUE))

# define model component
cmp = ~ -1 + Intercept(1) +   depth(depth_scaled , model = "linear") + 
  depth2(depth_scaled2 , model = "linear") 

# define model predictor
eta = density ~ Intercept + depth + depth2 

# build the observation model
lik = bru_obs(formula = eta,
              data = pcod_sf  ,
              family = "tweedie")

# fit the model
fit_Tweedie = bru(cmp, lik)

summary(fit_Tweedie)

# Hurdle model 1 ----------------------------------------------------------


##  Mesh and SPDE



mesh = fm_mesh_2d(loc = pcod_sf,           # Build the mesh
                  cutoff = 2,
                  max.edge = c(10,20),     # The largest allowed triangle edge length.
                  offset = c(5,50))       # The automatic extension distance
ggplot() + gg(mesh) + geom_sf(data= pcod_sf, aes(color = factor(present)), size = 0.1) + xlab("") + ylab("")


spde_model =  inla.spde2.pcmatern(mesh,
                                   prior.sigma = c(1, 0.5),
                                   prior.range = c(100, 0.5))


cmp_2 <- ~
  Intercept_biomass(1) +
  depth_biomass(depth_scaled, model = "linear") +
  depth2_biomass(depth_scaled2, model = "linear") +
  space_biomass(geometry, model = spde_model) +
  Intercept_caught(1) +
  depth_caught(depth_scaled, model = "linear") +
  depth2_caught(depth_scaled2, model = "linear") +
  space_caught(geometry, model = spde_model) 


biomass_obs <- bru_obs(formula = density ~  Intercept_biomass + depth_biomass + depth2_biomass + space_biomass,
                       family = "lognormal",
                       data = pcod_sf  %>% filter(density>0))

presence_obs <- bru_obs(formula = present ~ Intercept_caught + depth_caught + depth2_caught +
                          space_caught,
                        family = "binomial",
                        data = pcod_sf,
)

fit_hurdle <- bru(
  cmp_2,
  biomass_obs,
  presence_obs
)

summary(fit_hurdle)


# Hurdle Model 2 ----------------------------------------------------------

cmp3 <- ~
  Intercept_biomass(1) +
  depth_biomass(depth_scaled, model = "linear") +
  depth2_biomass(depth_scaled2, model = "linear") +
  Intercept_caught(1) +
  depth_caught(depth_scaled, model = "linear") +
  depth2_caught(depth_scaled2, model = "linear") +
  space(geometry, model = spde_model) +
  space_copy(geometry, copy = "space", fixed = FALSE)

biomass_obs <- bru_obs(formula = density ~  Intercept_biomass + depth_biomass + depth2_biomass + space,
                       family = "lognormal",
                       data = pcod_sf  %>% filter(density>0))

presence_obs <- bru_obs(formula = present ~ Intercept_caught + depth_caught + depth2_caught +space_copy,
                        family = "binomial",
                        data = pcod_sf,
)

fit_hurdle_shared <- bru(
  cmp3,
  biomass_obs,
  presence_obs
)

summary(fit_hurdle_shared)

data.frame( Model = c("Tweedie", "Hurdle", "Hurdle 2" ),
            DIC = c(fit_Tweedie$dic$dic, fit_hurdle$dic$dic,  fit_hurdle_shared$dic$dic),
            WAIC = c(fit_Tweedie$waic$waic, fit_hurdle$waic$waic, fit_hurdle_shared$waic$waic),
            MLIK = c(fit_Tweedie$mlik[1], fit_hurdle$mlik[1], fit_hurdle_shared$mlik[1]))



# Predictions -------------------------------------------------------------

# depth quadratic effect
pred_depth = predict(fit_hurdle, pcod_sf %>% select(c(density,depth,depth_scaled,depth_scaled2)), ~ (Intercept_biomass + depth_biomass + depth2_biomass),
                     n.samples = 1000)

p1 <- pred_depth %>% ggplot() + 
  geom_point(aes(depth ,log(density)), alpha = 0.3) +
  geom_line(aes(depth ,mean)) +
  geom_line(aes(depth, q0.025), linetype = "dashed")+
  geom_line(aes(depth, q0.975), linetype = "dashed")+
  xlab("Depth") + ylab("log-Biomass Density")

#  inv logit for cathcing probabilities
inv_logit = function(x) (1+exp(-x))^(-1)

# prediciton data set
pxl1 = data.frame(crds(depth_r), 
                  as.data.frame(depth_r)) %>% 
  filter(!is.na(depth)) %>%
  st_as_sf(coords = c("x","y"),crs=st_crs(pcod_sf)) 

pred <- predict( fit_hurdle , pxl1,
                 ~ {
                   pi <- inv_logit(Intercept_caught + depth_caught + depth2_caught + space_caught)
                   mu_log  <-  Intercept_biomass + depth_biomass + depth2_biomass + space_biomass
                   sd <- sqrt(1/Precision_for_the_lognormal_observations)
                   conditional_mean <- exp(mu_log + 0.5 * sd^2)
                   dens <- pi * conditional_mean
                   list(
                     pi = pi,
                     conditional_mean = conditional_mean,
                     dens = dens)
                 },n.samples = 2500)



p2 =  ggplot() +
  gg(pred$pi, geom = "tile",aes(fill = mean)) +
  scale_fill_scico(direction = -1,palette = "roma",name="Catch probability")+
  guides(fill = guide_colorbar(
    direction = "horizontal",               # horizontal bar
    title.position = "top",                 # title on top of bar
    barwidth = unit(8, "cm"),               # adjust width
    barheight = unit(0.5, "cm")             # adjust height
  )) +
  theme(
    legend.position = "bottom",             # move legend to bottom
    legend.title = element_text(0.5)
  )



p3= ggplot() +
  gg(pred$dens, geom = "tile",aes(fill = mean)) +
  scale_fill_viridis(name="Biomass Density Kg/Km^2")+
  guides(fill = guide_colorbar(
    direction = "horizontal",               # horizontal bar
    title.position = "top",                 # title on top of bar
    barwidth = unit(8, "cm"),               # adjust width
    barheight = unit(0.5, "cm")             # adjust height
  )) +
  theme(
    legend.position = "bottom",             # move legend to bottom
    legend.title = element_text(0.5)
  )



p1 / (p2 + p3)
