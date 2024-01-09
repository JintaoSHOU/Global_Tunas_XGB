library(mlr)
library(tidyverse)
library(parallel)
library(parallelMap)
library(RColorBrewer)
library(metR)
library(gganimate)
library(gifski)
library(rgeos)
library(rgdal)


for(tuna in c( 'ALB','BET', 'SKJ', 'YFT'))
{
  data = read_csv(paste0('AO_', tuna, '_PA3Ys.csv')) %>%
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
  
  
  #### Full Model####
  FullData = data %>%
    select(all_of(tuna), c(6:17, 20:31, 34:45,48:59, 62, 63))
  
  taskFullXGB = makeRegrTask(data=FullData, target = tuna)
  randSearch = makeTuneControlRandom(maxit = 100)
  learnerXGB = makeLearner('regr.xgboost')
  kFold = makeResampleDesc('CV', iters=10)
  
  XGBParamSpace = makeParamSet(
    makeNumericParam('eta', lower=0, upper=1),
    makeNumericParam('gamma', lower=0, upper=10),
    makeIntegerParam('max_depth', lower=1, upper=20),
    makeNumericParam('min_child_weight', lower=1, upper=10),
    makeNumericParam('subsample', lower=0.5, upper=1),
    makeNumericParam('colsample_bytree', lower=0.5, upper=1),
    makeIntegerParam('nrounds', lower=5, upper=100)
  )
  
  parallelStartSocket(cpus = detectCores()) # 开始多线程
  tunedXGBParams = tuneParams(learnerXGB, task=taskFullXGB, resampling = kFold,
                              par.set = XGBParamSpace, control = randSearch)
  parallelStop()
  
  tunedXGB = setHyperPars(learnerXGB, par.vals = tunedXGBParams$x)
  tunedXGBModel = train(tunedXGB, taskFullXGB)
  
  ggplot(getFeatureImportance(tunedXGBModel)$res)+
    geom_bar(aes(x=importance, y=reorder(variable, importance)), stat='identity', color='grey', fill="grey")+
    labs(y='Variable')+
    theme_test()+
    theme(text=element_text(size=6))
  
  FullPer = performance(predict(tunedXGBModel, newdata = FullData), measures = list(mse, rsq))
  
  #### top Model####
  TopVars =  getFeatureImportance(tunedXGBModel)$res %>%
    arrange(desc(importance)) %>%
    slice(1:10) 
  
  topData = data %>%
    select(all_of(tuna),TopVars$variable)
  
  taskTopXGB = makeRegrTask(data=topData, target = tuna)
  
  parallelStartSocket(cpus = detectCores()) # 开始多线程
  tunedXGBParams = tuneParams(learnerXGB, task=taskTopXGB, resampling = kFold,
                              par.set = XGBParamSpace, control = randSearch)
  parallelStop()
  
  tunedXGB = setHyperPars(learnerXGB, par.vals = tunedXGBParams$x)
  tunedXGBModel = train(tunedXGB, taskTopXGB)
  
  TopPer = performance(predict(tunedXGBModel, newdata = topData), measures = list(mse, rsq))
  
  ggplot(getFeatureImportance(tunedXGBModel)$res)+
    geom_bar(aes(x=importance, y=reorder(variable, importance)), stat='identity', color='grey', fill="grey")+
    labs(y='Variable')+
    theme_test()+
    theme(text=element_text(size=6))

  
  save(tunedXGBModel, file=paste0('AO_',tuna,'XGBModel.RData'))
}
#### Predict 1900-2100 ####
YMPre = AOEnv %>%
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
         Chl_500 = Chl_500*10^6) %>%
  select(Year, Month, Lon, Lat, TopVars$variable)

YMPre$pre = predict(tunedXGBModel, newdata = YMPre %>% select(-c(1:4)))$data$response

#### gganimate ####
preYearly = YMPre %>%
  group_by(Year, Lon, Lat) %>%
  summarise(Abundance = median(pre))

all = ggplot(preYearly)+
  coord_sf(xlim=c(300, 360), ylim=c(-50, 50))+
  geom_contour_filled(aes(x=Lon, y=Lat, z=Abundance))+
  geom_polygon(data=map_data('world2'), aes(x=long, y=lat, group=group), fill='grey', colour='grey')+
  scale_fill_manual(values = Colors)+
  labs(fill='Abundance', title = 'Year: {frame_time}   ssp: {sp}')+
  theme_test()+
  theme(text=element_text(family="serif", size=8),
        legend.position='none',
        axis.title = element_blank())+
  transition_time(Year)+
  ease_aes('linear')

animate(all, width=6, height = 10, nframes = 201, res=500, fps=5, units='in',renderer = gifski_renderer())
anim_save(paste0(tuna,'_AO_ssp',sp,'.gif'), animation = last_animation())
