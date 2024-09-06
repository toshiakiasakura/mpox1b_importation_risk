#DRC Scenario 1---------------------------------------------------------------------------
raw = read.csv("./dt_flight/Scenario1.csv")

data = data.frame(country=raw$Country,volume=raw$Volume)
data = data[data$volume>0,]

totalvolume = sum(data$volume)
data$prob=data$volume/totalvolume

size = 10000
result= t(sapply(1:nrow(data),function(r){
  cases_quantile = qgeom(c(0.025,0.5,0.975),data$prob[r]) # median & CI of number of all-country imported cases before the first import happens in country r

  casesbeforeimport = rgeom(size,data$prob[r]) # sample number of all-country cases before the first import
  othercountryprobs = data$prob[-r]
  countriesbeforeimport = sapply(casesbeforeimport,function(cases){
    sum(rmultinom(1, cases, othercountryprobs)>0)
  })
  countries_quantile = quantile(countriesbeforeimport,c(0.025,0.5,0.975),type=1)
  c(cases_quantile,countries_quantile)
}))
colnames(result)=c(paste("casesbeforeimport", c("lower", "median", "upper")),paste("countriesbeforeimport", c("lower", "median", "upper")))

data=cbind(data[,1:3], data.frame(result))
write.csv(data,"./res/results1.csv")

#DRC+Burundi Scenario 2------------------------------------------------------------------
raw = read.csv("./dt_flight/Scenario2.csv")

data = data.frame(country=raw$Country,volume=raw$Volume)
data = data[data$volume>0,]

totalvolume = sum(data$volume)
data$prob=data$volume/totalvolume

size = 10000
result= t(sapply(1:nrow(data),function(r){
  cases_quantile = qgeom(c(0.025,0.5,0.975),data$prob[r]) # median & CI of number of all-country imported cases before the first import happens in country r

  casesbeforeimport = rgeom(size,data$prob[r]) # sample number of all-country cases before the first import
  othercountryprobs = data$prob[-r]
  countriesbeforeimport = sapply(casesbeforeimport,function(cases){
    sum(rmultinom(1, cases, othercountryprobs)>0)
  })
  countries_quantile = quantile(countriesbeforeimport,c(0.025,0.5,0.975),type=1)
  c(cases_quantile,countries_quantile)
}))
colnames(result)=c(paste("casesbeforeimport", c("lower", "median", "upper")),paste("countriesbeforeimport", c("lower", "median", "upper")))

data=cbind(data[,1:3], data.frame(result))
write.csv(data,"./res/results2.csv")

#DRC+Burundi+Uganda+Kenya+Rwanda Scenario 3----------------------------------------------------
raw = read.csv("./dt_flight/Scenario3.csv")

data = data.frame(country=raw$Country,volume=raw$Volume)
data = data[data$volume>0,]

totalvolume = sum(data$volume)
data$prob=data$volume/totalvolume

size = 10000
result= t(sapply(1:nrow(data),function(r){
  cases_quantile = qgeom(c(0.025,0.5,0.975),data$prob[r]) # median & CI of number of all-country imported cases before the first import happens in country r

  casesbeforeimport = rgeom(size,data$prob[r]) # sample number of all-country cases before the first import
  othercountryprobs = data$prob[-r]
  countriesbeforeimport = sapply(casesbeforeimport,function(cases){
    sum(rmultinom(1, cases, othercountryprobs)>0)
  })
  countries_quantile = quantile(countriesbeforeimport,c(0.025,0.5,0.975),type=1)
  c(cases_quantile,countries_quantile)
}))
colnames(result)=c(paste("casesbeforeimport", c("lower", "median", "upper")),paste("countriesbeforeimport", c("lower", "median", "upper")))

data=cbind(data[,1:3], data.frame(result))
write.csv(data,"./res/results3.csv")

