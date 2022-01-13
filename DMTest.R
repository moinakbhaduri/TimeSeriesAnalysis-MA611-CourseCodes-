UKElectricity
data=as.data.frame(UKElectricity)
data
coal=data[,2]
oil=data[,3]
gas=data[,4]
nuclear=data[,5]
hydro=data[,6]
windsolar=data[,7]
bio=data[,8]

autoplot(ts(cbind(coal,oil,gas,nuclear,hydro,windsolar,bio),start = c(1998,1),frequency = 4),facets = T)+ggtitle("Electricity sources")

#####---Defining variables---#####
interest=ts(oil,start = c(1998,1),frequency = 4)
nonlinearityTest(interest)
interest.train=window(interest, end=end(interest)-c(2,0))
interest
interest.train

interest.test=window(interest,start=c(2018,3))
interest.test

#---Some low-scale trials---#
trial.ann=ets(interest.train,model="ANN")
trial.ar000=Arima(interest.train,c(0,0,0))
accuracy(forecast(trial.ann),interest)
accuracy(forecast(trial.ar000),interest)
dm.test(residuals(trial.ann),residuals(trial.ar000),h=1)$p.value


#---Now a large-scale study-----#
#---Naive modeling---#
naive.model=naive(interest.train)
naive.model
snaive.model=snaive(interest.train)
accuracy(snaive.model)
#---Simple stl---#
stl.randomwalk.model=stl(interest.train,t.window = 5,s.window="periodic", robust=TRUE)
#---ETS bunch---#
incorrect.ann=ets(interest.train,model = "ANN")
improbable.aan=ets(interest.train,model="AAN")
best.ets=ets(interest.train)
#---Transformed---#
best.log.ets=ets(interest.train,lambda=0)
sadjlogstl=stlf(interest.train,lambda = 0)
#---ARIMA--#
best.arima=auto.arima(interest.train,stepwise = F)
#--NNET--#
neural=nnetar(interest.train)
#--Bagged model---#
bm=baggedModel(interest.train, bootstrapped_series = bld.mbb.bootstrap(hydro.train, 15))


#--Measuring accuracies the usual way---#

accuracy(forecast(naive.model),interest)
accuracy(forecast(snaive.model),interest)
accuracy(forecast(stl.randomwalk.model),interest)
accuracy(forecast(incorrect.ann),interest)
accuracy(forecast(improbable.aan),interest)
accuracy(forecast(best.ets),interest)
accuracy(forecast(best.log.ets),interest)
accuracy(forecast(sadjlogstl),interest)
accuracy(forecast(best.arima),interest)
accuracy(forecast(neural),interest)
accuracy(forecast(bm),interest)

table=rbind(accuracy(forecast(naive.model),interest),
            accuracy(forecast(snaive.model),interest),
            accuracy(forecast(stl.randomwalk.model),interest),
            accuracy(forecast(incorrect.ann),interest),
            accuracy(forecast(improbable.aan),interest),
            accuracy(forecast(best.ets),interest),
            accuracy(forecast(best.log.ets),interest),
            accuracy(forecast(sadjlogstl),interest),
            accuracy(forecast(best.arima),interest),
            accuracy(forecast(neural),interest),
            accuracy(forecast(bm),interest))
library("xtable")
print(xtable(table), include.rownames = TRUE, include.colnames = TRUE, sanitize.text.function = I)

#---Coding convenience---#
m=list()
m[[1]]=naive.model
m[[2]]=snaive.model
m[[3]]=stl.randomwalk.model
m[[3]]=incorrect.ann
m[[4]]=improbable.aan
m[[5]]=best.ets
m[[6]]=best.log.ets
m[[7]]=sadjlogstl
m[[8]]=best.arima
m[[9]]=neural
m[[10]]=bm
m


Pval.Matrix=matrix(0,length(m),length(m))
for(i in 1:length(m))
{
  for(j in (i+1):length(m))
  {
    Pval.Matrix[i,j]=dm.test(residuals(m[[i]]),residuals(m[[j]]),alternative = c("less"),h=1)$p.value
  }
}

Pval.Matrix
rownames(Pval.Matrix)<-c("Naive","SNaive","ETS(ANN)","ETS(AAN)","BestETS","BestLogETS","Sadjlogstl","BestARIMA","Neural","Bagged")
colnames(Pval.Matrix)<-c("Naive","SNaive","ETS(ANN)","ETS(AAN)","BestETS","BestLogETS","Sadjlogstl","BestARIMA","Neural","Bagged")
Pval.Matrix
heatmap(Pval.Matrix)

library("pheatmap")
pheatmap(Pval.Matrix, display_numbers = T)

dm.test(residuals(snaive.model),residuals(naive.model),h=1)


###---Fixing the diagonal issue----##

Pval.Matrix.diag=matrix(0,length(m),length(m))
for(i in 1:length(m))
{
  for(j in 1:length(m))
  {
    if(j==i)
    {
      Pval.Matrix.diag[i,j]=1
    }
    else
    {
      Pval.Matrix.diag[i,j]=dm.test(residuals(m[[i]]),residuals(m[[j]]),alternative = c("less"),h=1)$p.value
    }
  }
  
}

Pval.Matrix.diag
rownames(Pval.Matrix.diag)<-c("Naive","SNaive","ETS(ANN)","ETS(AAN)","BestETS","BestLogETS","Sadjlogstl","BestARIMA","Neural","Bagged")
colnames(Pval.Matrix.diag)<-c("Naive","SNaive","ETS(ANN)","ETS(AAN)","BestETS","BestLogETS","Sadjlogstl","BestARIMA","Neural","Bagged")
Pval.Matrix.diag
heatmap(Pval.Matrix.diag)

library("pheatmap")
pheatmap(Pval.Matrix.diag, display_numbers = T)







###---Test on out sample forecasts---###

#---Naive modeling---#
naive.model2=naive(interest.test,model=naive.model)
naive.model2

snaive.model2=snaive(interest.test,model=snaive.model)
snaive.model2

accuracy(naive.model2)
accuracy(snaive.model2)

