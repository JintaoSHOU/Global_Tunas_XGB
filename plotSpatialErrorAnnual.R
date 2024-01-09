library(mlr)
library(tidyverse)
library(parallel)
library(parallelMap)
library(RColorBrewer)
library(metR)
library(gganimate)
library(gifski)
library(patchwork)
#library(rgeos)
#library(rgdal)
FGColor = c("#08519C","#3182BD","#6BAED6","#BDD7E7","#EFF3FF","white", "#FEE5D9","#FCAE91","#FB6A4A","#DE2D26","#A50F15")
####ALB####
AO_ALB = read_csv('AO/AO_ALB_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("AO/AO_ALB_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
AO_ALB$pre = predict(tunedXGBModel, newdata = AO_ALB %>% select(TopVars$variable))$data$response
AO_ALB$Ocean = 'AO'

PO_ALB = read_csv('PO/PO_ALB_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("PO/PO_ALB_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
PO_ALB$pre = predict(tunedXGBModel, newdata = PO_ALB %>% select(TopVars$variable))$data$response
PO_ALB$Ocean = "PO"

IO_ALB = read_csv('IO/IO_ALB_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("IO/IO_ALB_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
IO_ALB$pre = predict(tunedXGBModel, newdata = IO_ALB %>% select(TopVars$variable))$data$response
IO_ALB$Ocean = 'IO'

ALB = bind_rows((AO_ALB %>% select('Ocean', "Year", 'Month', 'Lon', 'Lat', 'ALB', 'pre')),
          (IO_ALB %>% select('Ocean',"Year", 'Month', 'Lon', 'Lat', 'ALB', 'pre')),
          (PO_ALB %>% select('Ocean',"Year", 'Month', 'Lon', 'Lat', 'ALB', 'pre')))

ALB = ALB %>% 
  mutate(Res = pre-ALB)

####BET####
AO_BET = read_csv('AO/AO_BET_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("AO/AO_BET_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
AO_BET$pre = predict(tunedXGBModel, newdata = AO_BET %>% select(TopVars$variable))$data$response
AO_BET$Ocean = 'AO'

PO_BET = read_csv('PO/PO_BET_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("PO/PO_BET_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
PO_BET$pre = predict(tunedXGBModel, newdata = PO_BET %>% select(TopVars$variable))$data$response
PO_BET$Ocean = 'PO'

IO_BET = read_csv('IO/IO_BET_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("IO/IO_BET_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
IO_BET$pre = predict(tunedXGBModel, newdata = IO_BET %>% select(TopVars$variable))$data$response
IO_BET$Ocean = 'IO'

BET = bind_rows((AO_BET %>% select("Ocean", "Year", 'Month', 'Lon', 'Lat', 'BET', 'pre')),
                (IO_BET %>% select('Ocean', "Year", 'Month', 'Lon', 'Lat', 'BET', 'pre')),
                (PO_BET %>% select('Ocean', "Year", 'Month', 'Lon', 'Lat', 'BET', 'pre')))

BET = BET %>% 
  mutate(Res = pre-BET)

####SKJ####
AO_SKJ = read_csv('AO/AO_SKJ_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("AO/AO_SKJ_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
AO_SKJ$pre = predict(tunedXGBModel, newdata = AO_SKJ %>% select(TopVars$variable))$data$response
AO_SKJ$Ocean = 'AO'

PO_SKJ = read_csv('PO/PO_SKJ_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("PO/PO_SKJ_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
PO_SKJ$pre = predict(tunedXGBModel, newdata = PO_SKJ %>% select(TopVars$variable))$data$response
PO_SKJ$Ocean = 'PO'

IO_SKJ = read_csv('IO/IO_SKJ_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("IO/IO_SKJ_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
IO_SKJ$pre = predict(tunedXGBModel, newdata = IO_SKJ %>% select(TopVars$variable))$data$response
IO_SKJ$Ocean = 'IO'

SKJ = bind_rows((AO_SKJ %>% select('Ocean', "Year", 'Month', 'Lon', 'Lat', 'SKJ', 'pre')),
                (IO_SKJ %>% select('Ocean', "Year", 'Month', 'Lon', 'Lat', 'SKJ', 'pre')),
                (PO_SKJ %>% select('Ocean', "Year", 'Month', 'Lon', 'Lat', 'SKJ', 'pre')))

SKJ = SKJ %>% 
  mutate(Res = pre-SKJ)
####YFT####
AO_YFT = read_csv('AO/AO_YFT_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("AO/AO_YFT_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
AO_YFT$pre = predict(tunedXGBModel, newdata = AO_YFT %>% select(TopVars$variable))$data$response
AO_YFT$Ocean = 'AO'

PO_YFT = read_csv('PO/PO_YFT_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("PO/PO_YFT_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
PO_YFT$pre = predict(tunedXGBModel, newdata = PO_YFT %>% select(TopVars$variable))$data$response
PO_YFT$Ocean = 'PO'

IO_YFT = read_csv('IO/IO_YFT_PA3Ys.csv') %>%
  mutate(Chl_2.5 = Chl_2.5*10^6,
         Chl_10 = Chl_10*10^6,
         Chl_20 = Chl_20*10^6,
         Chl_32.5 = Chl_32.5*10^6,
         Chl_51.25 = Chl_51.25*10^6,
         Chl_75 = Chl_75*10^6,
         Chl_100 =Chl_100*10^6,
         Chl_125=Chl_125*10^6,
         Chl_156.25=Chl_156.25*10^6,
         Chl_200 = Chl_200*10^6,
         Chl_250 = Chl_250*10^6,
         Chl_312.5 = Chl_312.5*10^6,
         Chl_400 = Chl_400*10^6,
         Chl_500 = Chl_500*10^6)

load("IO/IO_YFT_XGBModel.RData")
TopVars =  getFeatureImportance(tunedXGBModel)$res
IO_YFT$pre = predict(tunedXGBModel, newdata = IO_YFT %>% select(TopVars$variable))$data$response
IO_YFT$Ocean = 'IO'

YFT = bind_rows((AO_YFT %>% select('Ocean', "Year", 'Month', 'Lon', 'Lat', 'YFT', 'pre')),
                (IO_YFT %>% select('Ocean', "Year", 'Month', 'Lon', 'Lat', 'YFT', 'pre')),
                (PO_YFT %>% select('Ocean', "Year", 'Month', 'Lon', 'Lat', 'YFT', 'pre')))

YFT = YFT %>% 
  mutate(Res = pre-YFT)
####plot####
pALBYearly = ggplot(ALB %>% group_by(Year, Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  coord_sf(xlim=c(0,360), ylim=c(-45,50))+
  geom_polygon(data=map_data('world2'), aes(x=long, y=lat, group=group), fill='gray30', colour='gray30')+
  scale_fill_gradient2(low='blue', high='red', midpoint = 0, limits=c(-0.4, 0.4))+
  facet_wrap(vars(Year), ncol = 3)+
  labs(fill='Spatial Residuals')+
  scale_x_longitude(NULL)+
  scale_y_latitude(NULL)+
  theme_test()+
  theme(legend.position = c(0.5, 0.05),
        legend.direction = 'horizontal',
        text = element_text(family='serif', size=5),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))

ggsave(filename='Fig/AnnualRes_ALB.png', width=170, height=200, units='mm', dpi=500)

pALBYearly = ggplot(BET %>% group_by(Year, Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  coord_sf(xlim=c(0,360), ylim=c(-45,50))+
  geom_polygon(data=map_data('world2'), aes(x=long, y=lat, group=group), fill='gray30', colour='gray30')+
  scale_fill_gradient2(low='blue', high='red', midpoint = 0, limits=c(-0.4, 0.4))+
  facet_wrap(vars(Year), ncol = 3)+
  labs(fill='Spatial Residuals')+
  scale_x_longitude(NULL)+
  scale_y_latitude(NULL)+
  theme_test()+
  theme(legend.position = c(0.5, 0.05),
        legend.direction = 'horizontal',
        text = element_text(family='serif', size=5),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))

ggsave(filename='Fig/AnnualRes_BET.png', width=170, height=200, units='mm', dpi=500)

pALBYearly = ggplot(SKJ %>% group_by(Year, Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  coord_sf(xlim=c(0,360), ylim=c(-45,50))+
  geom_polygon(data=map_data('world2'), aes(x=long, y=lat, group=group), fill='gray30', colour='gray30')+
  scale_fill_gradient2(low='blue', high='red', midpoint = 0, limits=c(-0.4, 0.4))+
  facet_wrap(vars(Year), ncol = 3)+
  labs(fill='Spatial Residuals')+
  scale_x_longitude(NULL)+
  scale_y_latitude(NULL)+
  theme_test()+
  theme(legend.position = c(0.5, 0.05),
        legend.direction = 'horizontal',
        text = element_text(family='serif', size=5),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))

ggsave(filename='Fig/AnnualRes_SKJ.png', width=170, height=200, units='mm', dpi=500)

pALBYearly = ggplot(YFT %>% group_by(Year, Lon, Lat) %>% summarise(Res = mean(Res)))+
  geom_raster(aes(x=Lon, y=Lat, fill=Res))+
  coord_sf(xlim=c(0,360), ylim=c(-45,50))+
  geom_polygon(data=map_data('world2'), aes(x=long, y=lat, group=group), fill='gray30', colour='gray30')+
  scale_fill_gradient2(low='blue', high='red', midpoint = 0, limits=c(-0.4, 0.4))+
  facet_wrap(vars(Year), ncol = 3)+
  labs(fill='Spatial Residuals')+
  scale_x_longitude(NULL)+
  scale_y_latitude(NULL)+
  theme_test()+
  theme(legend.position = c(0.5, 0.05),
        legend.direction = 'horizontal',
        text = element_text(family='serif', size=5),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))

ggsave(filename='Fig/AnnualRes_YFT.png', width=170, height=200, units='mm', dpi=500)
