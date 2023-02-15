# load libraries
library(lubridate)
library(raster)
library(rgdal)
library(sp)
library(sf)
library(tidyverse)
#library(ctmm)
library(amt)
library(ggplot2)
library(mapview)
library(viridis)
library(ggspatial)
library(cowplot)
#library(rgeos)

 # Load in the relevant data and spatial files 
load("etib_hr.Rdata")
trug <- raster("terrug.tif")
elv <- raster("PA_ned.tif")
sgl <- readOGR("/Users/benjaminpadilla/Documents/R/Elk/GIS/PGC_StateGameland2020/PGC_StateGameland2020.shp")
e_area <- readOGR("/Users/benjaminpadilla/Documents/R/Elk/GIS/PGC_ElkManagementAreas2020/PGC_ElkManagementAreas2020.shp")
veg <- readOGR("/Users/benjaminpadilla/Documents/R/Elk/GIS/20201117_VegTyping/20201117_VegTyping.shp")
 timb <- readOGR("/Users/benjaminpadilla/Documents/R/Elk/GIS/20201117_Other_GIS_Data/20201117_Timbersales.shp")
 
# wildlife <- readOGR("/Users/benjaminpadilla/Documents/R/Elk/GIS/20201117_Other_GIS_Data/20201117_WildlifePlantHabitatProjects.shp")
nlcd <- raster("PA_nlcd2.tif")
forest <- (nlcd %in% c(41,42,43))
names(forest) <- "forest"
open <- (nlcd %in% c(52,71,81,82))
names(open) <- "open"
dev <- (nlcd %in% c(21,22,23,24))
names(dev) <- "dev"
# make everything in the same projection ### 
sgl <- spTransform(sgl, crs(trug))
# veg <- spTransform(veg, crs(trug))
# wildlife <- spTransform(wildlife, crs(trug))
# timb <- spTransform(timb, crs(trug))

# Select and Manipulate VEG data classes into individual rasters ####
## Dry Oak Forests (Clcasses 'AH' and 'AD')
oak <- veg[ veg$Type %in% c("AD","AH"),]
## wildlife treatment projects that are "FOOD PLOTS"
foodplot <- subset(wildlife, ProjectTyp == "Food Plot Management")
## Overstory TImbersales 
overstry <- subset(timb, Treatment == "F - Overstory Removal")
## Shelterwood Timbersales 
shelterW <- subset(timb, Treatment == "C - Shelterwood (1st Entry)")

# rasterize the dcnr and pgc lands ####
pgc <- trug
values(pgc) <- NA
names(pgc) <- "pgc"
pgc <- rasterize(sgl, pgc, field = sgl$SGL)
dcnr <- trug
values(dcnr) <- NA
names(dcnr) <- "dcnr"
dcnr <- rasterize(veg, dcnr, field = factor(veg$Type))
writeRaster(dcnr, file = "dcnr_rast.tif")
writeRaster(pgc, file = "pgc_rast.tif")
# foodplot 
food <- trug
values(food) <- NA
names(food) <- "foodplot"
food <- rasterize(foodplot, food, field = factor(foodplot$ProjectTyp))
plot(crop(food, trug))
over <- trug
values(over) <- NA
names(over)  <- "overstory"
over <- rasterize(overstry, over, field = factor(overstry$Treatment))
shelter <- trug
values(shelter) <- NA
names(shelter)  <- "shelterwood"
shelter <- rasterize(shelterW, shelter, field = factor(shelterW$Treatment))
writeRaster(over, file = "overstory_rast.tif")
writeRaster(shelter, file = "shelterwood_rast.tif")

#########
# make them 0 / 1 ####
### read in rasters for PGC and DCNR
pgc <- raster("pgc_rast.tif")
pgc[is.na(values(pgc))] <- 0
pgc[values(pgc)>0] <- 1
dcnr <- raster("dcnr_rast.tif")
dcnr[is.na(values(dcnr))] <- 0
dcnr[values(dcnr)>0] <- 1
over <- raster("overstory_rast.tif")
over[is.na(values(over))] <- 0
over[values(over)>0] <- 1
shelter <- raster("shelterwood_rast.tif")
shelter[is.na(values(shelter))] <- 0
shelter[values(shelter)>0] <- 1
timber <- over + shelter
timber[values(timber)>0] <- 1
raster::plot(timber)

# data prep and organization ####
etib <- tlm_dat
etib$year <- year(etib$date_time)
etib$month <- month(etib$date_time)
etib$seas <- "fall"
etib$seas[etib$month==12|etib$month==1] <- "wint" 
etib$seas[etib$month==2] <- "wint" 
etib$seas[etib$month==4|etib$month==5] <- "spring"
etib$seas[etib$month==3] <- "wint"
etib$seas[etib$month==6] <- "spring"
etib$seas[etib$month==7|etib$month==8] <- "summer" 
etib18 <- etib %>% filter(year == 2018)
etib19 <- etib %>% filter(year == 2019)
etib18 <- etib18 %>% filter(Collar.ID %in% c(19077,19079,20740,20742,20743,21559,21561,25563,25566,25567))
etib19 <- etib19 %>% filter(Collar.ID %in% c(19077,19079,21559,35800,35801,35809,21561,25569,25566,25567))
etib <- etib %>% filter(Collar.ID %in% c(19077,19079,21559,35800,35801,35809,21561,25569,25566,25567,20740,20742,20743,25563))

# analysis with single nested tibble with year time of day individual and season
##    Create a nested tibble for 
  # All years 'elks' 
  # 2018 'elk18'
  # 2019 'elk19'

tfor <- 0.8103353
tope <- 0.07139944
tdev <- 0.03782066
tpub <- 0.01848612
tshel <- 0.01848612
tovrs <- 0.01757376
tcuts <- tshel + tovrs

## Both 2018 AND 2019  ####
etib <- etib %>% nest(-c(animal_ID, year))
etib2 <- etib %>% nest(-c(animal_ID, year,seas)) 
etib2 <- 
  etib2 %>% 
  mutate(trk = lapply(data, function(d){
    make_track(d, Longitude..deg.,Latitude..deg., date_time,
               id = Collar.ID, sex=sex,crs = CRS("+init=epsg:4326")) %>%  
      transform_coords(crs(trug))
}))
etib <- etib %>% 
  mutate(trk = lapply(data, function(d){
    make_track(d, Longitude..deg.,Latitude..deg., date_time,
               id = Collar.ID, sex=sex,crs = CRS("+init=epsg:4326")) %>%  
      transform_coords(crs(trug))
  }))
trk <- make_track(etib,Longitude..deg.,Latitude..deg.,  # All years (2018 and 2019)
                  date_time, id = Collar.ID,year = year, month = month,sex=sex,season = seas, crs = CRS("+init=epsg:4326"), sex = sex) %>%  
  transform_coords(crs(trug))
elks <- etib %>%
  mutate(n = map_int(data, nrow)) %>% 
  filter(n > 50)
elks_2 <- etib2 %>%
  mutate(n = map_int(data, nrow)) %>% 
  filter(n > 50)
etib <- etib %>% filter(animal_ID %in% unique(elks_2$animal_ID))
hrs_yr <- 
  elks %>%
  mutate(
    "hr_od_ou" = map(trk, ~ hr_od(., model = fit_ctmm(., "ou"))),
    "hr_od_50" = map(trk, ~ hr_od(., model = fit_ctmm(., "ou"), levels = c(0.5)))
)
hrs_yr_seas <- 
  elks_2 %>%
  mutate(
    "hr_od_ou" = map(trk, ~ hr_od(., model = fit_ctmm(., "ou"))),
    "hr_od_50" = map(trk, ~ hr_od(., model = fit_ctmm(., "ou"), levels = c(0.5)))
)

hrs_yr_seas <- 
  elkex %>% filter(!duplicated(animal)) %>% 
  mutate(
    "hr_od_ou" = map(trk, ~ hr_od(., model = fit_ctmm(., "ou"))),
    "hr_od_50" = map(trk, ~ hr_od(., model = fit_ctmm(., "ou"), levels = c(0.5)))
  )

# annual HOME RANGE ####
annu.elks <- trk %>% nest(data = -c(id, year)) %>% 
  mutate(n = map_int(data, nrow)) %>% 
  filter(n > 20)
annu.elks1 <- annu.elks %>%
  mutate(
    "95% Home Range" = map(data, ~ hr_od(., model = fit_ctmm(., "ou"))),
    "50% Core Area" = map(data, ~ hr_od(., model = fit_ctmm(., "ou"), levels = c(0.5)))
  )
annu.cover <- annu.elks1 %>% select(-data) %>%
  pivot_longer('95% Home Range':'50% Core Area', names_to = "estimator", values_to = "hr") %>%
  mutate(forest = map(hr, ~ raster::extract(forest, hr_isopleths(.))),
         open = map(hr, ~ raster::extract(open, hr_isopleths(.))),
         develop = map(hr, ~ raster::extract(dev, hr_isopleths(.))),
         dcnr = map(hr, ~ raster::extract(dcnr, hr_isopleths(.))),
         pgc = map(hr, ~ raster::extract(pgc, hr_isopleths(.))),
         shelter = map(hr, ~ raster::extract(shelter, hr_isopleths(.))),
         overstry = map(hr, ~ raster::extract(over, hr_isopleths(.))))
         #elevation = map(hr, ~ raster::extract(elev, hr_isopleths(.)))) 
annu.area <- annu.elks1 %>% 
  #select(-data) %>% 
  pivot_longer('95% Home Range':'50% Core Area', names_to = "estimator",values_to = "hr") %>% 
  mutate(hr_area = map(hr, hr_area),
         mean_sq_dist = map(data, msd),
         sinuos = map(data, sinuosity),
         intens = map(data, intensity_use),
         straight = map(data, straightness),
         t_dist = map(data, tot_dist)) %>% 
  unnest(cols = c(hr_area,mean_sq_dist, sinuos, straight, intens, t_dist))


annu.elk_prop <- elk_cvr %>% 
  mutate( prop_forest = map_dbl(forest, ~ mean(unlist(.))), 
          prop_open = map_dbl(open, ~ mean(unlist(.))),
          prop_dev = map_dbl(develop, ~ mean(unlist(.))),
          prop_dcnr = map_dbl(dcnr, ~ mean(unlist(.))),
          prop_pgc = map_dbl(pgc, ~ mean(unlist(.))),
          prop_shelt = map_dbl(shelter, ~ mean(unlist(.))),
          prop_ovrst = map_dbl(overstry, ~mean(unlist(.))),
          mu_elev = map_dbl(elevation, ~ mean(unlist(.))),
          area = map(hr, hr_area)) %>%
  select(id, year,season,estimator, mu_elev, prop_forest, prop_open, prop_dev, prop_dcnr, prop_pgc, prop_shelt, prop_ovrst, area) %>% 
  unnest(cols = area) %>%
  pivot_longer(prop_forest:prop_ovrst, names_to = "landscape", values_to = "prop")


ggplot(annu.area, aes(x=estimator, y = area)) + 
  geom_boxplot( aes(fill = estimator),alpha = 0.5) + 
  geom_point(aes(color = factor(id), shape = estimator)) + 
  scale_color_viridis_d() + 
  ylab("Home Range Area (meters squared)") + 
  facet_grid(~year) + 
  guides(size = FALSE, alpha = F, shape = FALSE, color = FALSE) + 
  theme_light()

ggplot(annu.elk_prop, aes(x=landscape, y = prop)) + 
  geom_boxplot( aes(fill = landscape),alpha = 0.5) + 
  geom_point(aes(color = factor(id), shape = estimator)) + 
  scale_color_viridis_d() + 
  ylab("Home Range Area (meters squared)") + 
  facet_grid(~year) + 
  guides(size = FALSE, alpha = F, shape = FALSE, color = FALSE) + 
  theme_light()

dat <- elk_sz %>% filter(estimator == "hr_od_ou",season == "fall")
mean(data.frame(dat %>% select(area))[,1])
sd(data.frame(dat %>% select(area))[,1])



#######

hrs_yr2 <- hrs_yr %>% 
  mutate(HomeRange = hr_od_ou) %>% 
  mutate(CoreArea = hr_od_50) %>% 
  #select(-data) %>% 
  pivot_longer(HomeRange:CoreArea, names_to = "estimator",values_to = "hr") %>% 
  mutate(hr_area = map(hr, hr_area),
         mean_sq_dist = map(trk, msd),
         sinuos = map(trk, sinuosity),
         intens = map(trk, intensity_use),
         straight = map(trk, straightness),
         t_dist = map(trk, tot_dist)) %>% 
  unnest(cols = c(hr_area,mean_sq_dist, sinuos, straight, intens, t_dist))

elk_sz <- hrs_yr_seas %>% 
  #select(-data) %>% 
  pivot_longer(hr_od_ou:hr_od_50, names_to = "estimator",values_to = "hr") %>% 
  mutate(hr_area = map(hr, hr_area),
         mean_sq_dist = map(trk, msd),
         sinuos = map(trk, sinuosity),
         intens = map(trk, intensity_use),
         straight = map(trk, straightness),
         t_dist = map(trk, tot_dist)) %>% 
  unnest(cols = c(hr_area,mean_sq_dist, sinuos, straight, intens, t_dist))


elk_mov <- elk_sz %>% 
  pivot_longer(mean_sq_dist:t_dist, names_to = "metric", values_to = "metric_val")

system.time(
elk_cvr <- hrs_yr_seas %>% select(-data) %>%
  pivot_longer(hr_od_ou:hr_od_50, names_to = "estimator", values_to = "hr") %>%
  mutate(forest = map(hr, ~ raster::extract(forest, hr_isopleths(.))),
         open = map(hr, ~ raster::extract(open, hr_isopleths(.))),
         develop = map(hr, ~ raster::extract(dev, hr_isopleths(.))),
         dcnr = map(hr, ~ raster::extract(dcnr, hr_isopleths(.))),
         pgc = map(hr, ~ raster::extract(pgc, hr_isopleths(.))),
         shelter = map(hr, ~ raster::extract(shelter, hr_isopleths(.))),
         overstry = map(hr, ~ raster::extract(over, hr_isopleths(.))),
         timbercuts = map(hr, ~ raster::extract(timb.cut, hr_isopleths(.))))
)
  



 elk_prop <- elk_cvr %>% 
  mutate( prop_forest = map_dbl(forest, ~ mean(unlist(.))), 
          prop_open = map_dbl(open, ~ mean(unlist(.))),
          prop_dev = map_dbl(develop, ~ mean(unlist(.))),
          prop_dcnr = map_dbl(dcnr, ~ mean(unlist(.))),
          prop_pgc = map_dbl(pgc, ~ mean(unlist(.))),
          prop_shelt = map_dbl(shelter, ~ mean(unlist(.))),
          prop_ovrst = map_dbl(overstry, ~mean(unlist(.))),
          prop_timber = map_dbl(timbercuts, ~mean(unlist(.))),
         # mu_elev = map_dbl(elevation, ~ mean(unlist(.))),
          area = map(hr, hr_area)) %>%
  # select(animal_ID, year,season,estimator, prop_forest, prop_open, prop_dev, prop_dcnr, prop_pgc, prop_shelt, prop_ovrst, area) %>% 
  unnest(cols = area)

elk_selection <- elk_prop %>% 
  mutate(f_sel = prop_forest / tfor,
         op_sel = prop_open / tope,
         dv_sel = prop_dev / tdev,
         sw_sel = prop_shelt / tshel,
         ov_sel = prop_ovrst / tovrs,
         timb_sel = prop_timber / tcuts)

elk_prop_long <- elk_prop %>% 
  mutate(Forest = prop_forest) %>%
  mutate(Open = prop_open) %>%
  mutate(Developed = prop_dev) %>%
  mutate(DCNR = prop_dcnr) %>%
  mutate(PGC = prop_pgc) %>%
  mutate(Shelterwood = prop_shelt) %>%
  mutate(Overstory = prop_ovrst) %>%
  mutate(Timbercuts = prop_timber) %>%
  mutate(Shelterwood_Forest = prop_shelt / prop_forest) %>%
  pivot_longer(Forest:Shelterwood_Forest, names_to = "landscape", values_to = "prop")

elk_sel2 <- elk_selection %>% 
  pivot_longer(f_sel:timb_sel, names_to = "landscape", values_to = "selec")

elk_sz <- elk_sz %>% mutate(prop_forest = elk_prop$prop_forest, 
                            prop_dev = elk_prop$prop_dev, 
                            prop_open = elk_prop$prop_open, 
                            prop_public = elk_prop$prop_dcnr + elk_prop$prop_pgc,
                            prop_shelt = elk_prop$prop_shelt,
                            prop_ovrst = elk_prop$prop_ovrst)

####################################################################
####################################################################
####################################################################
## Here and below begins plotting and visualization of the data ####
####################################################################
##  plot home range size by season for 2018 and 2019 ########
ggplot(hrs_yr2, aes(x= factor(year), y = area)) + 
  geom_boxplot(aes(fill = factor(year)), alpha = 0.5) + 
  scale_color_viridis_b(aes(fill = (animal_ID)),option = "E") + 
  geom_point(aes( shape = estimator)) + 
  ylab("Home Range Area (meters squared)") + 
  facet_grid(sex~estimator, scales = "free") + 
  guides(size = FALSE, alpha = F, shape = FALSE, color = FALSE) + 
  theme_light() + 
  theme(strip.text = element_text(size=19)) 

elk_prop <- elk_prop %>% 
  mutate(Area = (area / 10000)) 


labs <- c(hr_od_50 = "Core Area", hr_od_ou = "Home Range")
area_plot <- ggplot(elk_prop, aes(x=seas, y = Area,fill = seas)) + 
  geom_boxplot( alpha = 0.5) + 
  scale_fill_brewer(palette="BuPu") + 
  geom_jitter(aes(color = factor(animal_ID), shape = estimator), alpha = 0.5,width = 0.1) + 
  scale_color_viridis_d() + 
  ylab("Home Range Area (Hectares)") + 
  xlab("Season") +
  facet_grid(~sex,labeller = labeller(estimator = labs)) + 
  guides(size = FALSE, alpha = F, shape = FALSE, color = FALSE) + 
  labs(fill = "Season") + 
  scale_fill_discrete(name = "Season",labels = c("Fall","Spring","Summer","Winter")) +
  scale_x_discrete(name = "Season",labels = c("Fall","Spring","Summer","Winter")) +
  theme_light() +
  theme(strip.text = element_text(size=18),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 16)) 
area_sex_plot <-ggplot(elk_prop, aes(x=season, y = Area, fill = season)) + 
  geom_jitter(aes(color = factor(year), alpha = 0.5),width = .1) + 
  geom_boxplot( alpha = 0.5) + 
  scale_fill_brewer(palette="BuPu") + 
  scale_color_viridis_d() + 
  ylab("Home Range Area (Hectares)") + 
  facet_grid( sex ~ estimator,scales = "free",labeller = labeller(estimator = labs)) + 
  guides(size = FALSE, alpha = F, shape = FALSE, color = FALSE) + 
  theme_light() + 
  theme(strip.text = element_text(size=19)) 
# plotting some of the movement metrics and how they vary by season ####
# total distance moved - fall appears to be have longer distances
dist_plt <- ggplot(elk_sz, aes(x= seas , y =mean_sq_dist)) + 
  geom_boxplot(aes(fill = seas, alpha = 0.8)) + 
  geom_point(aes(color = factor(year), alpha = 0.4)) + 
  scale_color_viridis_d(aes(factor(year))) + 
  facet_grid(estimator ~ sex, labeller = labeller(estimator = labs)) + 
  ylab("Mean Squared Distance") + 
  guides(size = FALSE, alpha = F,color = F, shape = FALSE) + 
  theme_light()

# track sinuosity - doesnt seem to be any variation
sinu_plt <- ggplot(elk_sz, aes(x= seas , y = sinuos)) + 
  geom_boxplot(aes(fill = seas, alpha = 0.8)) + 
  geom_point(aes(color = factor(year), alpha = 0.4)) + 
  scale_color_viridis_d(aes(factor(year))) + 
  facet_grid(estimator ~ sex, labeller = labeller(estimator = labs)) + 
  ylab("Track Sinuosity") + 
  guides(size = FALSE, alpha = F,color = F, shape = FALSE) + 
  theme_light()
strt_plt <- ggplot(elk_sz, aes(x= seas, y = straight)) + 
  geom_boxplot(aes(fill = seas, alpha = 0.8)) + 
  geom_point(aes(color = factor(year), alpha = 0.4)) + 
  scale_color_viridis_d(aes(factor(year))) + 
  facet_grid(estimator ~ sex, labeller = labeller(estimator = labs)) + 
  ylab("Track Straigtness") + 
  guides(size = FALSE, alpha = F,color = F, shape = FALSE) + 
  theme_light()
# Plot Grid of Movement Metrics ####
plot_grid(dist_plt + guides(fill = F)+xlab("Season") , intens_plt + guides(fill = F)+xlab("Season") , 
          sinu_plt + guides(fill = F)+xlab("Season") , strt_plt + guides(fill = F)+xlab("Season") )

# mean squared distance - as expected, this reflects the patterns seen in total distance
ggplot(elk_sz, aes(x= seas , y = mean_sq_dist)) + 
  geom_boxplot(aes(fill = seas, alpha = 0.8)) + 
  geom_point(aes(color = animal_ID)) + 
  scale_color_viridis_d(aes(color)) + 
  ylab("Mean Squared Distance") + 
  #facet_grid(estimator ~ year) + 
  guides(size = FALSE, alpha = F, shape = FALSE) + 
  theme_light()
# boxplot of Intensity Use 
intens_plt <- ggplot(elk_sz, aes(x= seas , y = intens)) + 
  geom_boxplot(aes(fill = seas, alpha = 0.8)) + 
  geom_point(aes(color = factor(year))) + 
  scale_color_viridis_d(aes(factor(year)), alpha = 0.4) + 
  ylab("Intensity Use") + 
  facet_grid(estimator ~ sex, labeller = labeller(estimator = labs)) + 
  guides(size = FALSE, alpha = F,color = F, shape = FALSE) + 
  theme_light()
# Mean Sq Distance as a function of home range size ###
msqdist <- ggplot(elk_sz , aes(y= mean_sq_dist , x = area)) + 
  geom_point(aes(color = sex)) + 
  scale_color_viridis_d(aes(color)) + 
  geom_smooth(aes(color = viridis(1),lty = sex), method = "gam", se = F) +
  ylab("Mean Sq Distance") + 
  facet_grid(estimator ~ seas, scales = "free") + 
  guides(size = FALSE, color = F ,alpha = F, shape = FALSE) + 
  theme_light()
# Mean Sq Distance as a function of proportion forest ###
ggplot(elk_sz, aes(y= mean_sq_dist , x = prop_forest)) + 
  geom_point(aes(color = factor(id))) + 
  scale_color_viridis_d(aes(color)) + 
  geom_smooth(aes(color = viridis(1),lty = season), method = "gam", se = F) +
  ylab("Mean Sq Distance") + xlab("Proportion of Home Range Forested") +
  facet_wrap( ~ year) + 
  guides(size = FALSE, color = F ,alpha = F, shape = FALSE) + 
  theme_light()
# Intensity Use as a function of proportion forest ###
ggplot(elk_sz, aes(y= intens , x = prop_forest)) + 
  geom_point(aes(color = factor(id))) + 
  scale_color_viridis_d(aes(color)) + 
  geom_smooth(aes(color = viridis(1),lty = season), method = "gam", se = F) +
  ylab("Intensity Use") + xlab("Proportion of Home Range Forested") +
  facet_wrap( ~ year) + 
  guides(size = FALSE, color = F ,alpha = F, shape = FALSE) + 
  theme_light()
# Intensity Use as a function of home range size ###
ggplot(elk_sz, aes(y= intens , x = area)) + 
  geom_point(aes(color = factor(id))) + 
  scale_color_viridis_d(aes(color)) + 
  geom_smooth(aes(color = viridis(1),lty = season), method = "gam", se = F) +
  ylab("Intensity Use") + 
  facet_grid(estimator ~ year) + 
  guides(size = FALSE, color = F ,alpha = F, shape = FALSE) + 
  theme_light()
# Mean Squared Distance vs Intensity Use
ggplot(elk_sz, aes(y= mean_sq_dist , x = intens)) + 
  geom_point(aes(color = factor(id))) + 
  scale_color_viridis_d(aes(color)) + 
  geom_smooth(aes(color = viridis(1),lty = season), method = "gam", se = F) +
  ylab("Mean Sq Distance") + 
  xlab("Intensity Use") + 
  facet_grid(estimator ~ year) + 
  guides(size = FALSE, color = F ,alpha = F, shape = FALSE) + 
  theme_light()
# Sinuosity vs Intensity Use
ggplot(elk_sz , aes(y= sinuos , x = intens)) + 
  geom_point(aes(color = factor(id))) + 
  scale_color_viridis_d(aes(color)) + 
  geom_smooth(aes(color = viridis(1),lty = season), method = "gam", se = F) +
  ylab("Track Sinuosity") + 
  xlab("Intensity Use") + 
  facet_grid(estimator ~ year) + 
  guides(size = FALSE, color = F ,alpha = F, shape = FALSE) + 
  theme_light()



ggplot(elk_prop, aes(y = area, x = mu_elev)) +
  geom_point(aes(color = factor(id), shape = factor(year), alpha = 0.8)) + 
  geom_smooth(aes(lty = estimator, color = estimator), method = "gam", se = F) +
  scale_color_viridis_d(aes(color = estimator), direction =  -1) + 
  guides(size = F, alpha = F, color = FALSE) + 
  facet_wrap(~season) +
  xlab("Elevation (m)") +
  ylab("Home Range Size (Hectares)") +
  theme_light()
ggplot(elk_prop, aes(y = area, x = prop_forest)) +
  geom_point(aes(color = (animal_ID), shape = factor(year))) +
  guides(color = FALSE) + 
  geom_smooth(aes(lty = estimator), method = "gam", se = F) +
  guides(size = F, alpha = F, estimator = F, lty = FALSE) + 
  facet_wrap(~seas) +
  xlab("Proportion of Forest") + 
  geom_hline(aes(yintercept = 0), color = "red", lty = 3) +
  ylab("Home Range Size (Hectares)") +
  theme_light()
## Plot of Timber Harvest against Proportion Forested ######
plt.data <- elk_prop %>% filter(estimator == "hr_od_ou")
ggplot(plt.data, aes(x = prop_forest, y = prop_timber)) +
  geom_point(aes(color = as.factor(year), shape = factor(year))) + 
  geom_smooth(method = "loess", se = T) +
  guides(size = F, alpha = F, estimator = F, lty = FALSE) + 
  facet_wrap(~seas, scales = "free") +
  xlab("Proportion of Forest") + 
  geom_hline(aes(yintercept = 0), color = "red", lty = 3) +
  ylab("Proportion of Timber Harvest") +
  theme(strip.text = element_text(size = 15),
        axis.title = element_text(size = 12))
ggplot(elk_prop, aes(y = area, x = timbercuts)) +
  geom_point(aes(color = (animal_ID), shape = factor(year))) + 
  guides(color = FALSE) + 
  geom_smooth(aes(lty = estimator), method = "loess", se = F) +
  guides(size = F, alpha = F, estimator = F, lty = FALSE) + 
  facet_wrap(~seas) +
  xlab("Proportion of Shelterwood Cuts") + 
  geom_hline(aes(yintercept = 0), color = "red", lty = 3) +
  ylab("Home Range Size (Hectares)") +
  theme_light()
ggplot(elk_prop, aes(y = prop_forest, x = prop_ovrst)) +
  geom_point(aes(color = (animal_ID), shape = factor(year))) + 
  #geom_smooth(aes(lty = estimator), method = "loess", se = F) +
  guides(size = F, alpha = F, estimator = F, lty = FALSE, color= F) + 
  facet_wrap(~seas) +
  xlab("Proportion of Overstory Removal Cuts") + 
  geom_hline(aes(yintercept = 0), color = "red", lty = 3) +
  ylab("Proportion of Forest") +
  theme_light()
ggplot(elk_prop, aes(y = area, x = prop_timber)) +
  geom_point(aes(color = prop_open)) + 
  geom_smooth(aes(lty = estimator), color = viridis(1),method = "gam", se = F) +
  guides(size = F, color = F,alpha = F, estimator = F, lty = FALSE) + 
  facet_grid(year~seas, scales = "free") +
  xlab("Proportion Timber Harvests") + 
  geom_hline(aes(yintercept = 0), color = "red", lty = 3) +
  ylab("Home Range Size (Hectares)") +
  theme_light()
ggplot(elk_prop, aes(y = area, x = prop_dev)) +
  geom_point(aes(color = animal_ID)) + 
  geom_smooth(aes(lty = estimator), color = viridis(1),method = "gam", se = F) +
  guides(size = F, color = F,alpha = F, estimator = F, lty = FALSE) + 
  facet_grid(year~seas) +
  xlab("Proportion Developed") + 
  geom_hline(aes(yintercept = 0), color = "red", lty = 3) +
  ylab("Home Range Size (Hectares)") +
  theme_light()
ggplot(elk_prop, aes(y = area, x = prop_open)) +
  geom_point(aes(color = animal_ID)) + 
  geom_smooth(aes(lty = estimator), color = viridis(1),method = "gam", se = F) +
  guides(size = F, alpha = F, color = F,estimator = F, lty = FALSE) + 
  facet_grid(rows= vars(seas), cols = vars(year)) +
  xlab("Proportion Open") + 
  geom_hline(aes(yintercept = 0), color = "red", lty = 3) +
  ylab("Home Range Size (Hectares)") +
  theme_light()
ggplot(elk_prop, aes(y = area, x = prop_forest)) +
  geom_point(aes(color = prop_timber)) + 
  scale_color_viridis_c(aes(color = prop_timber), option = "A",direction = -1) + 
  geom_smooth(aes(lty = estimator), color = viridis(5)[2],method = "gam", se = F) +
  guides(size = F, alpha = F,estimator = F, lty = FALSE) + 
  facet_grid(rows= vars(seas), cols = vars(year)) +
  xlab("Proportion Forest") + 
  geom_hline(aes(yintercept = 0), color = "red", lty = 3) +
  ylab("Home Range Size (Hectares)") +
  theme(strip.text = element_text(size=15),
        axis.title = element_text(size = 15))
ggplot(elk_prop, aes(y = area, x = prop_open)) +
  geom_point(aes(color = prop_timber)) + 
  scale_color_viridis_c(aes(color = prop_timber), option = "A",direction = -1) + 
  geom_smooth(aes(lty = estimator), color = viridis(5)[2],method = "gam", se = F) +
  guides(size = F, alpha = F,estimator = F, lty = FALSE) + 
  facet_grid(rows= vars(seas), cols = vars(year)) +
  xlab("Proportion Open") + 
  geom_hline(aes(yintercept = 0), color = "red", lty = 3) +
  ylab("Home Range Size (Hectares)") +
  theme(strip.text = element_text(size=15),
        axis.title = element_text(size = 13)) 
ggplot(elk_prop, aes(y = logarea, x = mu_elev)) +
  geom_point(aes(color = factor(id))) + 
  geom_smooth(aes(lty = estimator), color = viridis(10)[8],method = "gam", se = F) +
  guides(size = F, alpha = F, estimator = F, lty = FALSE) + 
  facet_grid(rows= vars(season), cols = vars(year)) +
  xlab("Mean Elevation (m)") + 
  geom_hline(aes(yintercept = 0), color = "red", lty = 3) +
  ylab("Home Range Size (Hectares)") +
  theme_light()
# DESCRIPTION OF THIS PLOT
t1<-ggplot(elk_prop_long %>% filter( landscape %in% c("Developed","Forest","Open","Timbercuts")), 
       aes(x = landscape, y = prop)) + 
  geom_jitter(aes(color = factor(year), alpha = 0.5),width = .1) + 
  geom_boxplot(aes(fill = landscape, alpha = 0.5),outlier.alpha = 0.3) + 
  guides(alpha = FALSE,color = F) + 
  ylab("Proportion") + xlab("Landscape Type") +
  facet_grid(sex ~ estimator, labeller = labeller(estimator = labs)) + 
  #facet_grid(cols=vars(year), rows=vars(season)) + 
  theme_light() +
  theme(strip.text = element_text(size=15),
        axis.text = element_text(size = 12)) 
ggplot(elk_prop %>% filter( landscape %in% c("Developed","Forest","Open","Timbercuts")), 
       aes(x = landscape, y = prop)) + 
  geom_jitter(aes(color = factor(year), alpha = 0.5),width = .1) + 
  geom_boxplot(aes(fill = landscape, alpha = 0.5),outlier.alpha = 0.3) + 
  guides(alpha = FALSE,color = F) + 
  ylab("Proportion") + xlab("Landscape Type") +
  facet_grid(sex ~ estimator, labeller = labeller(estimator = labs)) + 
  #facet_grid(cols=vars(year), rows=vars(season)) + 
  theme_light() +
  theme(strip.text = element_text(size=15),
        axis.text = element_text(size = 12)) 

sLabs <- c("Fall","Spring","Summer","Winter")
names(sLabs) <- c("fall","spring","summer","wint")
eLabs <- c("Female","Male")
names(eLabs) <- c("female","male")
t2<-ggplot(elk_prop_long %>% filter( landscape %in% c("Developed","Forest","Open","Timbercuts")), 
       aes(x = landscape, y = prop)) + 
  geom_boxplot(aes(fill = landscape, alpha = 0.8),outlier.alpha = 0.2) + 
  geom_jitter(aes(color = factor(year), alpha = 0.4),width = .1) + 
  guides(alpha = FALSE,color = F) + 
  scale_fill_viridis_d(name = "Landscape Type", labels = c("Developed","Forest","Open","Timber Harvests")) +
  scale_x_discrete(name = "Landscape Type", labels = c("Developed","Forest","Open","Timber Harvests")) +
  ylab("Proportion") + xlab("Landscape Type") +
  facet_grid(sex ~ seas,
             labeller = labeller(sex = eLabs,seas = sLabs)) + 
  labs(fill = "Landscape Type") + 
  theme_light() +
  theme(strip.text = element_text(size=18),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45,hjust = 1,size = 12),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 16),
        legend.position = "bottom") 

plot_grid(t2,t1)
# DESCRIPTION OF THIS PLOT
shelbox <- ggplot(elk_prop_long %>% filter(landscape %in% c("Timbercuts")), aes(x = seas, y = log(prop))) + 
  geom_boxplot(aes(fill = seas, alpha = 0.4), outlier.alpha = 0.3) + 
  geom_jitter(aes(color = factor(animal_ID),alpha = 0.4),width = .1) + 
  guides(alpha = FALSE, color = F) + 
  facet_grid( ~ sex) + 
  ylab("Log of Timbercut Proportion") + 
  xlab("Landscape Type") + 
  theme_light() +
  #facet_grid(cols=vars(year), rows=vars(season)) + 
  theme(strip.text = element_text(size=15),
        axis.text = element_text(size = 12)) 
shel_frst <- ggplot(elk_prop , aes(y = prop_forest, x = prop_shelt)) + 
  geom_point(aes(color = factor(sex), shape = factor(year), alpha = 0.6)) + 
  guides(alpha = FALSE, color = F, shape = F) + 
  geom_smooth(method = "gam", se = F) + 
  facet_grid(sex~seas) +
  #scale_y_continuous(limits = c(0, 0.25)) +
  xlab("Percent Shelterwood") + 
  ylab("Percent Forest Cover") +
  theme_light() +
  theme(strip.text = element_text(size=19)) 
plot_grid(shel_frst, shelbox)
# DESCRIPTION OF THIS PLOT
labels <- c(prop_dcnr = "DCNR",prop_dev = "Developed", prop_forest = "Forest", prop_open ="Open",prop_timb="Timbercut",prop_pgc="PGC")
ggplot(elk_prop_long %>% filter(landscape %in% c("DCNR","Developed","Forest","Open","PGC","Timbercuts")), 
       aes(x = prop, y = area)) + 
  geom_point(aes(color = area), alpha = 0.6) + 
  scale_color_viridis_c(option = "I", direction = 1) +
  geom_smooth(aes(lty = estimator), method = "glm", se = T) + 
  scale_y_continuous(limits = c(0, 100000000)) +
  facet_grid(
    landscape ~ seas) + 
  ylab("Home Range Area") + 
  guides(alpha = FALSE, color = F) + 
  theme(strip.text = element_text(size = 12))
# DESCRIPTION OF THIS PLOT
line_plot_sx <- ggplot(elk_prop_long  %>% filter(landscape  %in% c("Forest", "Open", "Developed", "Timbercuts")), aes(x = prop, y = (area/10000))) + 
  geom_point( alpha = 0.4) + 
  geom_smooth(aes(lty = estimator), method = "gam", se = T) + 
  scale_y_continuous(limits = c(0, 10000)) +
  facet_grid(landscape ~ sex, scales = "free") + 
  xlab("Proportion of Habitat") + ylab("Area (Hectares)") + 
  guides(alpha = FALSE, color = FALSE, lty = F) + 
  theme(strip.text = element_text(size = 12)) 
plot_grid(line_plot, line_plot_sx)
#### sexes #####
tmp <- elk_cvr %>% unnest(trk)
tmp <- tmp %>% filter(!duplicated(animal_ID))
ids <- tmp %>% select(animal_ID, sex)
hrs_yr$sex <- "elk"
for(i in 1:nrow(ids)){
  id <- ids$animal_ID[i]
  hrs_yr$sex[hrs_yr$animal_ID==id] <- ids$sex[i]
}

t3<-ggplot(elk_prop_long %>% filter( landscape %in% c("Developed","Forest","Open","Timbercuts")), 
           aes(x = landscape, y = prop)) + 
  geom_boxplot(aes(fill = landscape, alpha = 0.5),outlier.alpha = 0.3) + 
  scale_fill_brewer(palette="BuPu") +
  geom_jitter(aes(color = factor(year), alpha = 0.4),width = .1) + 
  guides(alpha = FALSE,color = F) + 
  facet_grid(sex~ seas, labeller = labeller(estimator = labs)) + 
  ylab("Proportion of Home Range") + xlab("Landcover Type") + 
  theme(strip.text = element_text(size=17),
        axis.text = element_text(size = 11)) 
t4<-ggplot(elk_prop_long %>% filter( landscape %in% c("Developed","Forest","Open","Timbercuts")), 
           aes(x = landscape, y = prop)) + 
  geom_boxplot(aes(fill = landscape, alpha = 0.5),outlier.alpha = 0.3) + 
  geom_jitter(aes(color = factor(year), alpha = 0.4),width = .1) + 
  guides(alpha = FALSE,color = F, fill = F) + 
  facet_wrap(~ seas, nrow = 2) + 
  #facet_grid(cols=vars(year), rows=vars(season)) + 
  theme(strip.text = element_text(size=19)) 
plot_grid(t2,t1)
# fit some test models #####
# KDE
elk_prop <- elk_prop %>% 
  mutate(logarea = log(area))

summary(m95.seasons <- glm(logarea ~ season + factor(year), data = elk_prop %>% filter(estimator == "hr_od_ou")))
summary(m50.seasons <- glm(logarea ~ season + factor(year), data = elk_prop %>% filter(estimator == "hr_od_50")))
summary(m.seas_int <- glm(logarea ~ season * (prop_forest + prop_dev + prop_open + prop_shelt + prop_ovrst), data = elk_prop %>% filter(estimator == "hr_od_ou")))
summary(m50.seas_int <- glm(logarea ~ season * (prop_forest + prop_dev + prop_open + prop_shelt + prop_ovrst), data = elk_prop %>% filter(estimator == "hr_od_50")))

# Univariate Models ####
# forest
m.forest.95 <- lmer(logarea ~ prop_forest | animal_ID, data = elk_prop %>% filter(estimator == "hr_od_ou"))
m.forest.50 <- lmer(logarea ~ prop_forest | animal_ID, data = elk_prop %>% filter(estimator == "hr_od_50"))
# developed
m.devel.95 <- lmer(logarea ~ prop_dev | animal_ID, data = elk_prop %>% filter(estimator == "hr_od_ou"))
m.devel.50 <- lmer(logarea ~ prop_dev | animal_ID, data = elk_prop %>% filter(estimator == "hr_od_50"))

# shelterwoods
m.timb.95 <- lmer(logarea ~ prop_timber | animal_ID, data = elk_prop %>% filter(estimator == "hr_od_ou"))
m.timb.50 <- lmer(logarea ~ prop_timber | animal_ID, data = elk_prop %>% filter(estimator == "hr_od_50"))
m.shelt.seas.95 <- lmer(prop_shelt ~ season | animal_ID, data = elk_prop %>% filter(estimator == "hr_od_ou"))
m.shelt.seas.50 <- lmer(prop_shelt ~ season | animal_ID, data = elk_prop %>% filter(estimator == "hr_od_50"))


###### Predicting and plotting #####
# forest Cover
n_dat <- data.frame(prop_forest = seq(min(elk_prop$prop_forest), max(elk_prop$prop_forest), length.out = 1000))
preds <- predict(m.forest.95, newdata = n_dat, se.fit = TRUE)
n_dat$fit <- preds$fit
plot(exp(n_dat$fit) ~ n_dat$prop_forest, type = "l")
ggplot(elk_prop, aes(y = area, x = c(prop_forest))) +
  geom_point(aes(color = factor(id), shape = factor(year))) + 
  geom_smooth(aes(lty = estimator), method = "glm", se = F) +
  guides(color = F,size = F,shape = FALSE,alpha = F, estimator = T, lty = FALSE) + 
  facet_wrap(~season) +
  xlab("Proportion of Forest") + 
  geom_hline(aes(yintercept = 0), color = "red", lty = 3) +
  ylab("Home Range Size (Hectares)") +
  theme_light()

## Model Fitting and AIC ############
# Home range Model Fitting #####
library(AICcmodavg)
library(lme4)
elk_prop <- elk_prop %>% 
  mutate(logarea = log(area),
         prop_public = prop_dcnr + prop_pgc)

summary(m95.seasons <- glm(logarea ~ season + factor(year), data = elk_prop %>% filter(estimator == "hr_od_ou")))
summary(m50.seasons <- glm(logarea ~ season + factor(year), data = elk_prop %>% filter(estimator == "hr_od_50")))
summary(m.seas_int <- glm(logarea ~ season * (prop_forest + prop_dev + prop_open + prop_shelt + prop_ovrst), data = elk_prop %>% filter(estimator == "hr_od_ou")))
summary(m50.seas_int <- glm(logarea ~ season * (prop_forest + prop_dev + prop_open + prop_shelt + prop_ovrst), data = elk_prop %>% filter(estimator == "hr_od_50")))

mod_data_95 <- elk_prop %>% filter(estimator == "hr_od_ou") %>% 
  mutate(logarea = log(area),
         prop_public = prop_dcnr + prop_pgc)
mod_data_95$animal_ID <- as.factor(mod_data_95$animal_ID)
mod_data_95$seas <- as.factor(mod_data_95$seas)
mod_data_95$area <- mod_data_95$area / 10000

mList_95 <- list("null" = lmer(area ~ 1 + (1 | animal_ID), data = mod_data_95),
                 "glob" = lmer(area ~ 1+ sex + seas + prop_forest + prop_open + prop_dev + prop_timber + (1 | animal_ID), data = mod_data_95),
                 "forest" = lmer(area ~ prop_forest + (1 | animal_ID), data = mod_data_95), 
                 "sx_forest" = lmer(area ~ sex * prop_forest + (1 | animal_ID), data = mod_data_95), 
                 "devel" = lmer(area ~ prop_dev + (1 | animal_ID), data = mod_data_95),
                 "sx_devel" = lmer(area ~ sex * prop_dev + (1 | animal_ID), data = mod_data_95),
                 "open"  = lmer(area ~ prop_open + (1 | animal_ID), data = mod_data_95),
                 "sx_open"  = lmer(area ~ sex * prop_open + (1 | animal_ID), data = mod_data_95),
                 "public" = lmer(area ~ prop_public + (1 | animal_ID), data = mod_data_95),
                 "sx_public" = lmer(area ~ sex * prop_public + (1 | animal_ID), data = mod_data_95),
                 "timber" = lmer(area ~ prop_timber + (1 | animal_ID), data = mod_data_95),
                 "sx_timber" = lmer(area ~ sex * prop_timber + (1 | animal_ID), data = mod_data_95),
                 "seas" = lmer(area ~ seas + (1 | animal_ID), data = mod_data_95),
                 "seas_forest" = lmer(area ~ seas * prop_forest + (1 | animal_ID), data = mod_data_95),
                 "seas_open" = lmer(area ~ seas * prop_open + (1 | animal_ID), data = mod_data_95),
                 "seas_dev" = lmer(area ~ seas * prop_dev + (1 | animal_ID), data = mod_data_95),
                 "seas_timber" = lmer(area ~ seas * prop_timber + (1 | animal_ID), data = mod_data_95),
                 "forest_dev" = lmer(area ~ prop_forest * prop_dev + (1 | animal_ID), data = mod_data_95),
                 "forest_open" = lmer(area ~ prop_forest * prop_open + (1 | animal_ID), data = mod_data_95),
                 "forest_timb" = lmer(area ~ prop_forest * prop_timber + (1 | animal_ID), data = mod_data_95),
                 "timb_open" = lmer(area ~ prop_timber * prop_open + (1 | animal_ID), data = mod_data_95))

                 #"S*devel" = lmer(area ~ seas + prop_dev| animal_ID, data = mod_data_95),
                 #"S*open"  = lmer(area ~ seas + prop_open| animal_ID, data = mod_data_95),
                 #"S*public" = lmer(area ~ seas + prop_public| animal_ID, data = mod_data_95),
                 #"S*timber" = lmer(area ~ seas + prop_timber| animal_ID, data = mod_data_95),
                 #"S*forest+open" = lmer(area ~ seas + prop_forest + prop_open | animal_ID, data = mod_data_95), 
                 #"S*forest+publ" = lmer(area ~ seas + prop_forest + prop_public | animal_ID, data = mod_data_95),
                 #"S*forest+timber" = lmer(area ~ seas + prop_forest + prop_timber | animal_ID, data = mod_data_95))

aictab(mList_95)

mod_data_50 <- elk_prop %>% filter(estimator == "hr_od_50") %>% 
  mutate(logarea = log(area),
         prop_public = prop_dcnr + prop_pgc)
mod_data_50$area <- mod_data_50$area / 10000

mList_50 <- list("null" = lmer(area ~ 1 + (1 | animal_ID), data = mod_data_50),
                 "glob" = lmer(area ~ 1 + sex + seas + prop_forest + prop_open + prop_dev + prop_timber + (1 | animal_ID), data = mod_data_50),
                 "forest" = lmer(area ~ prop_forest + (1 | animal_ID), data = mod_data_50), 
                 "sx_forest" = lmer(area ~ sex * prop_forest + (1 | animal_ID), data = mod_data_50), 
                 "devel" = lmer(area ~ prop_dev + (1 | animal_ID), data = mod_data_50),
                 "sx_devel" = lmer(area ~ sex * prop_dev + (1 | animal_ID), data = mod_data_50),
                 "open"  = lmer(area ~ prop_open + (1 | animal_ID), data = mod_data_50),
                 "sx_open"  = lmer(area ~ sex * prop_open + (1 | animal_ID), data = mod_data_50),
                 "public" = lmer(area ~ prop_public + (1 | animal_ID), data = mod_data_50),
                 "sx_public" = lmer(area ~ sex * prop_public + (1 | animal_ID), data = mod_data_50),
                 "timber" = lmer(area ~ prop_timber + (1 | animal_ID), data = mod_data_50),
                 "sx_timber" = lmer(area ~ sex * prop_timber + (1 | animal_ID), data = mod_data_50),
                 "seas" = lmer(area ~ seas + (1 | animal_ID), data = mod_data_50),
                 "seas_forest" = lmer(area ~ seas * prop_forest + (1 | animal_ID), data = mod_data_50),
                 "seas_open" = lmer(area ~ seas * prop_open + (1 | animal_ID), data = mod_data_50),
                 "seas_dev" = lmer(area ~ seas * prop_dev + (1 | animal_ID), data = mod_data_50),
                 "seas_timber" = lmer(area ~ seas * prop_timber + (1 | animal_ID), data = mod_data_50),
                 "forest_dev" = lmer(area ~ prop_forest * prop_dev + (1 | animal_ID), data = mod_data_50),
                 "forest_open" = lmer(area ~ prop_forest * prop_open + (1 | animal_ID), data = mod_data_50),
                 "forest_timb" = lmer(area ~ prop_forest * prop_timber + (1 | animal_ID), data = mod_data_50),
                 "timb_open" = lmer(area ~ prop_timber * prop_open + (1 | animal_ID), data = mod_data_50))

# 95% home range size model selection ####
aictab(mList_95)  # the top model here by a large margin is Seasons * (Forest + Shelterwood)
########## Second model at 3.57 dAIC is Seasons * (Forest + Overstory)
summary(mList_95$seas_forest)
t_95 <- mList_95$glob.2
par(mfrow=c(2,2))
plot(t_95)
summary(t_95)

sjPlot::tab_model(c(mList_95$seas_forest,mList_50$seas_forest),
                  show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Spring", "Summer", "Winter","Forest Cover","Spring*Forest","Summer*Forest","Winter*Forest"),
                  dv.labels= c("Effects of Season and Forest Cover on Home Range Size","Effects of Season and Forest Cover on Core Area Size"))

# 50% home range core model selection ####
aictab(mList_50) # dynamics at the core range are slighlty more variable according to model selection
### TOP model: Seasons * (Forest + Shelterwood)
#### SECOND model (0.64 dAIC): Seasons * Forest
##### THRID model (1.09 dAIC): Seasons * (Forest + Overstory)
# summary TOP
summary(mList_50$seas_forest)
# summary numero 2 
summary(mList_50$`S*forest`)
# summary numero 3 
summary(mList_50$`S*forest+overstory`)

t_50 <- mList_50$glob.2
par(mfrow=c(2,2))
plot(t_50)
summary(t_50)

elk_mov
mdat_dist <- elk_sz %>% filter(estimator == "hr_od_ou")
mDist_95 <- list("null" = glm(mean_sq_dist ~ 1, data = mdat_dist),
                 "global" = glm(mean_sq_dist ~ season + prop_forest + prop_dev + prop_open + prop_public + prop_shelt + prop_ovrst + mu_elev, data = mdat_dist),
                 "forest" = glm(mean_sq_dist ~ prop_forest, data = mdat_dist), 
                 "devel" = glm(mean_sq_dist ~ prop_dev, data = mdat_dist),
                 "open"  = glm(mean_sq_dist ~ prop_open, data = mdat_dist),
                 "public" = glm(mean_sq_dist ~ prop_public, data = mdat_dist),
                 "shelter" = glm(mean_sq_dist ~ prop_shelt, data = mdat_dist),
                 "overst" = glm(mean_sq_dist ~ prop_ovrst, data = mdat_dist),
                 "elev" = glm(mean_sq_dist ~ mu_elev, data = mdat_dist),
                 "season" = glm(mean_sq_dist ~ season, data = mdat_dist),
                 "S*forest" = glm(mean_sq_dist ~ season * prop_forest, data = mdat_dist), 
                 "S*devel" = glm(mean_sq_dist ~ season * prop_dev, data = mdat_dist),
                 "S*open"  = glm(mean_sq_dist ~ season * prop_open, data = mdat_dist),
                 "S*public" = glm(mean_sq_dist ~ season * prop_public, data = mdat_dist),
                 "S*shelter" = glm(mean_sq_dist ~ season * prop_shelt, data = mdat_dist),
                 "S*overst" = glm(mean_sq_dist ~ season * prop_ovrst, data = mdat_dist),
                 "S*elev" = glm(mean_sq_dist ~ season * mu_elev, data = mdat_dist),
                 "S*forest+open" = glm(mean_sq_dist ~ season * (prop_forest + prop_open), data = mdat_dist), 
                 "S*forest+publ" = glm(mean_sq_dist ~ season * (prop_forest + prop_public), data = mdat_dist),
                 "S*forest+shelt" = glm(mean_sq_dist ~ season * (prop_forest + prop_shelt), data = mdat_dist),
                 "S*forest+overstory" = glm(mean_sq_dist ~ season * (prop_forest + prop_ovrst), data = mdat_dist))
aictab(mDist_95)
summary(mDist_95$`S*shelter`)

mSinu_95 <- list("null" = glm(sinuos ~ 1, data = mdat_dist),
                 "global" = glm(sinuos ~ season + prop_forest + prop_dev + prop_open + prop_public + prop_shelt + prop_ovrst + mu_elev, data = mdat_dist),
                 "forest" = glm(sinuos ~ prop_forest, data = mdat_dist), 
                 "devel" = glm(sinuos ~ prop_dev, data = mdat_dist),
                 "open"  = glm(sinuos ~ prop_open, data = mdat_dist),
                 "public" = glm(sinuos ~ prop_public, data = mdat_dist),
                 "shelter" = glm(sinuos ~ prop_shelt, data = mdat_dist),
                 "overst" = glm(sinuos ~ prop_ovrst, data = mdat_dist),
                 "elev" = glm(sinuos ~ mu_elev, data = mdat_dist),
                 "season" = glm(sinuos ~ season, data = mdat_dist),
                 "S*forest" = glm(sinuos ~ season * prop_forest, data = mdat_dist), 
                 "S*devel" = glm(sinuos ~ season * prop_dev, data = mdat_dist),
                 "S*open"  = glm(sinuos ~ season * prop_open, data = mdat_dist),
                 "S*public" = glm(sinuos ~ season * prop_public, data = mdat_dist),
                 "S*shelter" = glm(sinuos ~ season * prop_shelt, data = mdat_dist),
                 "S*overst" = glm(sinuos ~ season * prop_ovrst, data = mdat_dist),
                 "S*elev" = glm(sinuos ~ season * mu_elev, data = mdat_dist),
                 "S*forest+open" = glm(sinuos ~ season * (prop_forest + prop_open), data = mdat_dist), 
                 "S*forest+publ" = glm(sinuos ~ season * (prop_forest + prop_public), data = mdat_dist),
                 "S*forest+shelt" = glm(sinuos ~ season * (prop_forest + prop_shelt), data = mdat_dist),
                 "S*forest+overstory" = glm(sinuos ~ season * (prop_forest + prop_ovrst), data = mdat_dist))
aictab(mSinu_95)


mdat_dist <- elk_sz %>% filter(estimator == "hr_od_50")
mDist_50 <- list("null" = glm(mean_sq_dist ~ 1, data = mdat_dist),
                 "global" = glm(mean_sq_dist ~ season + prop_forest + prop_dev + prop_open + prop_public + prop_shelt + prop_ovrst + mu_elev, data = mdat_dist),
                 "forest" = glm(mean_sq_dist ~ prop_forest, data = mdat_dist), 
                 "devel" = glm(mean_sq_dist ~ prop_dev, data = mdat_dist),
                 "open"  = glm(mean_sq_dist ~ prop_open, data = mdat_dist),
                 "public" = glm(mean_sq_dist ~ prop_public, data = mdat_dist),
                 "shelter" = glm(mean_sq_dist ~ prop_shelt, data = mdat_dist),
                 "overst" = glm(mean_sq_dist ~ prop_ovrst, data = mdat_dist),
                 "elev" = glm(mean_sq_dist ~ mu_elev, data = mdat_dist),
                 "season" = glm(mean_sq_dist ~ season, data = mdat_dist),
                 "S*forest" = glm(mean_sq_dist ~ season * prop_forest, data = mdat_dist), 
                 "S*devel" = glm(mean_sq_dist ~ season * prop_dev, data = mdat_dist),
                 "S*open"  = glm(mean_sq_dist ~ season * prop_open, data = mdat_dist),
                 "S*public" = glm(mean_sq_dist ~ season * prop_public, data = mdat_dist),
                 "S*shelter" = glm(mean_sq_dist ~ season * prop_shelt, data = mdat_dist),
                 "S*overst" = glm(mean_sq_dist ~ season * prop_ovrst, data = mdat_dist),
                 "S*elev" = glm(mean_sq_dist ~ season * mu_elev, data = mdat_dist),
                 "S*forest+open" = glm(mean_sq_dist ~ season * (prop_forest + prop_open), data = mdat_dist), 
                 "S*forest+publ" = glm(mean_sq_dist ~ season * (prop_forest + prop_public), data = mdat_dist),
                 "S*forest+shelt" = glm(mean_sq_dist ~ season * (prop_forest + prop_shelt), data = mdat_dist),
                 "S*forest+overstory" = glm(mean_sq_dist ~ season * (prop_forest + prop_ovrst), data = mdat_dist))
aictab(mDist_50)
summary(mDist_50$`S*forest`)


tmpdf <- data.frame(elk_prop %>% filter(estimator == "hr_od_50") %>% select(prop_ovrst, prop_shelt))
plot(tmpdf[,2]~tmpdf[,1])
tmpdf <- data.frame(elk_prop %>% filter(estimator == "hr_od_50") %>% select(prop_ovrst, prop_shelt, prop_public, prop_open, prop_dev, prop_forest))
library(corrplot)
plot(tmpdf)
corrplot.mixed(cor(tmpdf),  upper = "circle",lower = "number")
