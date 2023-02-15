collars <- elk_sz %>% filter(year == 2018) %>% select(id) %>% unique() 
collars <- collars[[1]][c(4,5,6,9)]
over18 <- list()
means18 <- data.frame( "id" = collars)
for(i in 1:length(collars)){
  tmp <- elk_sz %>% filter(year == 2018, estimator == "hr_od_ou", id == collars[i])
  tmp.Win <- tmp %>% filter(season == "wint")
  tmp.Spr <- tmp %>% filter(season == "spring")
  tmp.Fal <- tmp %>% filter(season == "fall")
  tmp.Sum <- tmp %>% filter(season == "summer")
  # calculate pairwise overlap 
  hrs <- list("winter" = tmp.Win$hr[[1]], 
              "summer" = tmp.Sum$hr[[1]], 
              "spring" = tmp.Spr$hr[[1]],
              "fall" =  tmp.Fal$hr[[1]])
  over18[[i]] <- data.frame(hr_overlap(hrs, consecutive.only = F))
  over18[[i]]$mean <- mean(over18[[i]]$overlap)
  means18$mean[i] <- mean(over18[[i]]$overlap)
}
names(over18) <- collars

collars <- elk_sz %>% filter(year == 2019) %>% select(id) %>% unique()
collars <- collars[[1]][-c(1,4,6,7)]
over19 <- list()
means19 <- data.frame( "id" = collars)
for(i in 1:length(collars)){
  tmp <- elk_sz %>% filter(year == 2019, estimator == "hr_od_ou", id == collars[i])
  tmp.Win <- tmp %>% filter(season == "wint")
  tmp.Spr <- tmp %>% filter(season == "spring")
  tmp.Fal <- tmp %>% filter(season == "fall")
  tmp.Sum <- tmp %>% filter(season == "summer")
  # calculate pairwise overlap 
  hrs <- list("winter" = tmp.Win$hr[[1]], 
              "summer" = tmp.Sum$hr[[1]], 
              "spring" = tmp.Spr$hr[[1]],
              "fall" =  tmp.Fal$hr[[1]])
  over19[[i]] <- data.frame(hr_overlap(hrs, consecutive.only = F))
  over19[[i]]$mean <- mean(over19[[i]]$overlap)
  means19$mean[i] <- mean(over19[[i]]$overlap)
}
names(over19) <- collars

for(i in 1:length(collars[[1]])){
  print(elk_sz %>% filter(year == 2019, estimator == "hr_od_ou", id == collars[[1]][i]) %>% nrow())
}



########## HR overlapp for revisions 9/20/2022 ####################

elk_cvr
rngs <- elk_cvr %>% filter(estimator == "hr_od_ou")
wanted <- table(rngs$animal_ID)
IDs <- names(wanted[wanted>=2])
rngs <- rngs %>% filter(animal_ID %in% IDs)
seasonal_20 <- seasonal_19 <- seasonal_18 <- list()
seasonal_overlap <- list(seasonal_18,seasonal_19,seasonal_20)
yrs <- c(2018,2019,2020)

rng <- rngs %>% #filter(year == 2020) %>% 
  mutate(sfHR = hr_to_sf(hr))

mDF <- rng %>% filter(sex=="male")
ggplot() + 
  annotation_map_tile(type= "osm", zoom = 8) + 
  layer_spatial(e_area, fill = NA) +
  geom_sf(data = mDF$sfHR, fill = as.numeric(factor(mDF$seas)), alpha = 0.5)
fDF <- rng %>% filter(sex=="female",year==2018)
p2018 <- ggplot() + 
  annotation_map_tile(type= "osm", zoom = 8) + 
  layer_spatial(e_area, fill = NA) +
  geom_sf(data = fDF$sfHR, fill = as.numeric(factor(fDF$seas)), alpha = 0.5) 

plot_grid(p2018,p2019,p2020)

for(j in 1:3){
  yearframe <- rngs %>% filter(year == yrs[j])
  wants <- table(yearframe$animal_ID)
  animIDs <- names(wants[wants>=2])
  yearframe <- yearframe %>% filter(animal_ID %in% animIDs)
  yr.list <- list()
  for(i in 1:length(animIDs)){
    tmp <- yearframe %>% filter(animal_ID == animIDs[i])
    ovr <- hr_overlap(tmp$hr, type = "hr", conditional = T, which = "all")
    ovr$animalID <- animIDs[i]
    yr.list[[i]] <- ovr
    
  }
  names(yr.list) <- animIDs
  seasonal_overlap[[j]] <- yr.list
}
for(i in 1:length(IDs)){
  tmp <- rngs %>% filter(animal_ID == IDs[i])
  ovr <- hr_overlap(tmp$hr, which = "all")
  seasonal_overlap[[i]] <- ovr
}
names(seasonal_overlap) <- IDs

df18 <- seasonal_overlap[[1]];df19 <- seasonal_overlap[[2]]; df20 <- seasonal_overlap[[3]] 
df18 <- do.call(rbind,df18);df18$Year <- '2018'
df19 <- do.call(rbind,df19);df19$Year <- '2019'
df20 <- do.call(rbind,df20);df20$Year <- '2020'
dfAll <- rbind(df18,df19,df20)
dfAll <- dfAll %>% mutate(seasons = paste(from, to, sep="_")) 
dfAll <- dfAll %>% 
  mutate(seasons = str_replace(seasons, "1_2", "Winter-Fall")) %>% 
  mutate(seasons = str_replace(seasons, "1_3", "Winter-Summer")) %>% 
  mutate(seasons = str_replace(seasons, "1_4", "Winter-Spring")) %>% 
  mutate(seasons = str_replace(seasons, "2_1", "Fall-Winter")) %>% 
  mutate(seasons = str_replace(seasons, "2_3", "Fall-Summer")) %>% 
  mutate(seasons = str_replace(seasons, "2_4", "Fall-Spring")) %>% 
  mutate(seasons = str_replace(seasons, "3_1", "Summer-Winter")) %>% 
  mutate(seasons = str_replace(seasons, "3_2", "Summer-Fall")) %>% 
  mutate(seasons = str_replace(seasons, "3_4", "Summer-Spring")) %>% 
  mutate(seasons = str_replace(seasons, "4_1", "Spring-Winter")) %>% 
  mutate(seasons = str_replace(seasons, "4_2", "Spring-Fall")) %>% 
  mutate(seasons = str_replace(seasons, "4_3", "Spring-Summer"))
dfAll$Sex <- "Female"
dfAll$Sex[grep('_male',dfAll$animalID)] <- "Male"

dfP <- do.call(rbind,hro20)
dfP <- dfP %>% mutate(seasons = paste(from, to, sep="_")) 
dfP$Sex <- "Female"
dfP$Sex[grep('_male',dfP$animalID)] <- "Male"

ggplot(dfAll,aes(x=seasons,y=overlap)) + 
  geom_boxplot(aes(fill = as.factor(from))) + 
  facet_grid(Year~Sex) +
  guides(fill = F) + 
  xlab("Seasons") + 
  ylab("% Overlap") +
  theme(axis.text.x = element_text(angle = 90))

dfAll %>% pivot_wider(names_from = "seasons",values_from = "overlap")

dfAll %>% filter(Sex == "Male",Year == "2020") %>% select("overlap") %>% summary()

dfAll %>% 
  group_by(Sex,Year, seasons) %>%
  summarise(qs = quantile(overlap,c(0.25,0.75)),prob=c(0.25,0.75),mean = mean(overlap))

tabDF <- dfAll %>% 
  filter(Year == 2020) %>%
  group_by(Sex, seasons) %>%
  summarise(qs = quantile(overlap,c(0.25,0.75)),prob=c(0.25,0.75),Mean = mean(overlap)) %>% 
  pivot_wider(names_from = "prob",values_from = "qs") %>% 
  mutate(Season = paste(Sex, seasons, sep = ": ")) %>%
  select("Mean","0.25","0.75","Season") %>% 
  ungroup() 
tabDF %>%
  select(-"Season") %>%
  gt() %>% 
  tab_header(title = "2020") 


tabDF <- data.frame("Sex" = c("Female","Male","Female","Male","Female","Male"),
                    "Year" = c("2018","2018","2019","2019","2020","2020"),
                    "Mean" = c(0.4625,0.3941,0.5142,0.3705,0.4358,0.3949),
                    "Lower" = c(0.2567,0.1836,0.3433,0.1848,0.2197,0.1813),
                    "Upper" = c(0.6426,0.6241,0.6903,0.5378,0.6300,0.5999))

tabDF %>% gt()
tF18 <- dfAll %>%   filter(Year == "2018", Sex == "Female") %>% 
  select(c("seasons","overlap")) %>% 
  #pivot_wider(names_from = Year, values_from = overlap) %>% 
  gt() %>% 
  tab_header(title = "Female 2018") 

plot_grid(tF18,tM18,tF19,tM19,tF20,tM20)

summary(lm(overlap ~ seasons, dfAll))

p8 <- ggplot(dfP,aes(x=seasons,y=overlap)) + 
  geom_boxplot(aes(fill = as.factor(from))) + 
  facet_wrap(~sex) +
  theme_gray()
plot_grid(p8,p9,p2,ncol = 1)

summary(dfP$overlap[dfP$sex=="male"])
summary(dfP$overlap[dfP$sex=="female"])

hr_yr<- hrs_yr2 %>% filter(estimator == "HomeRange")
wanted <- table(hr_yr$animal_ID)
IDs <- names(wanted[wanted>=2])

annuRng <- hr_yr %>% filter(animal_ID %in% IDs, estimator == "HomeRange")
annual_overlap <- list()

for(i in 1:length(IDs)){
  tmp <- annuRng %>% filter(animal_ID == IDs[i])
  ovr <- hr_overlap(tmp$hr_od_ou, conditional = T)
  annual_overlap[[i]] <- ovr
}
names(annual_overlap) <- IDs
males <- IDs[grep("_male",IDs)]
females <- IDs[grep("_female",IDs)]

annual_overlap[males]

mal_laps <- do.call(rbind, annual_overlap[males])
summary(mal_laps$overlap)

female_laps <- do.call(rbind, annual_overlap[females])

female_laps$sex <- "Female"; mal_laps$sex <- "Male"

ovrlp <- rbind(mal_laps,female_laps)

summary(lm(overlap ~ sex , ovrlp))

ggplot(ovrlp,aes(x=sex,y=overlap)) + 
  geom_boxplot(aes(fill = sex)) + 
  theme_gray()

all_annual <- annual_overlap
annuRng <- annuRng %>% mutate(sfHR = hr_to_sf(hr))
plot(annuRng$sfHR)
maData <- annuRng %>% filter(sex == "male")
mPlot <- ggplot() + 
  annotation_map_tile(type= "osm", zoom = 8) + 
  layer_spatial(e_area, fill = NA,lwd = 2) +
  geom_sf(data = maData$sfHR, alpha = 0.5) + 
  annotation_north_arrow(pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in")) + 
  annotation_scale(location = 'bl') + 
  ggtitle("Annual Home Range - Male") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5)) 
library(ggpubr)
ggarrange(fPlot, mPlot, labels = c("(A)", "(B)"))

## Overlap Table ####
install.packages("gt")
library(gt)


### states plot ####
install.packages(c("rnaturalearth","rnaturalearthdata"))
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

ggplot(data = states) +
  geom_sf() +
  layer_spatial(e_area, fill = NA,lwd = 2) +
  coord_sf(xlim = c(-90,-70),ylim = c(35,46),expand = FALSE)

