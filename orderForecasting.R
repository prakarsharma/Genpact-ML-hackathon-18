train<-read.csv("train.csv")

# exploration ####

head(train)

max(train$center_id)

length(unique(train$meal_id))

nrow(unique(train[,c('center_id','meal_id')]))

library(stringr)
library(reshape2)
library(magrittr)
library(DescTools)
library(forecast)
library(foreach)
library(purrr)
library(plyr)

ex<-function(f,x,i) f(x,i)

(function(x) plot(x$week,x$./1000,type="l",xlab="week",ylab="orders (thousands)")) (dcast(train,week~.,sum,na.rm=T,value.var='num_orders'))

# center-wise ts forecasting ####

orderTS<-split(train,train[,c("center_id","meal_id")])%>%lapply(function(x) ts(ZeroIfNA(merge(x,data.frame(week=1:145),all=T)[,"num_orders"]),frequency=52))

fit<-lapply(orderTS,function(x) if(!all(x==0)) auto.arima(x,xreg=))

coefs<-ldply(fit,function(x) data.frame(ar=x$arma[1],ma=x$arma[2]))
dcast(coefs,ar~ma,value.var=".id")

forecasting<-function(i){
  if(!is.null(fit[[i]])){
    dot=str_locate(names(orderTS)[i],"\\.")[,"start"]
    center=as.integer(str_sub(names(orderTS)[i],1,dot-1))
    meal=as.integer(str_sub(names(orderTS)[i],dot+1,nchar(names(orderTS)[i])))
    orders=round(as.numeric(forecast(fit[[i]],h=10)$mean),0)
    return(data.frame(center_id=center,meal_id=meal,num_orders=orders))
  }
}

orderForecast<-foreach(i=1:length(orderTS),.combine=rbind.data.frame) %do% forecasting(i)
orderForecast$week<-rep(146:155,times=nrow(orderForecast)/10)

test<-read.csv("test_QoiMO9B.csv")

submission<-modify_at(merge(test,orderForecast,all.x=T,sort=F)[,c("id","num_orders")],"num_orders",~replace(.,is.na(.)|.<0,0))

write.csv(submission,'submission1.csv',row.names=F)

# center-wise ts forecasting with external regressors ####

orders<-split(train,train[,c("center_id","meal_id")])%>%lapply(function(x) sapply(merge(within(x,discount<-base_price-checkout_price),data.frame(week=1:145),all=T),ZeroIfNA))

sparse<-ldply(orders,function(x) c('zeroes'=sum(x[,'num_orders']==0)*100/145))

rich<-list()
dump<-lapply(orders,function(x) if(sum(x[,'num_orders']==0)*100/145<10) rich<<-c(rich,list(x)))
rm(dump)

fit2<-lapply(rich,function(x) if(!all(x[,"num_orders"]==0)) auto.arima(ts(x[,"num_orders"],frequency=52)))

coefs<-ldply(fit2,function(x) ex(`names<-`,data.frame(t(x$arma[1:2])),c('ar','ma')))
dcast(coefs,ar~ma)

fit3<-lapply(rich,function(x) if(!all(x[,"num_orders"]==0)) tryCatch(arima(ts(x[,"num_orders"],frequency=52),xreg=x[,c("base_price","checkout_price","emailer_for_promotion","homepage_featured")])))
