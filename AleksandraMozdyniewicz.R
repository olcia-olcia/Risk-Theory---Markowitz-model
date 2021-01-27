library(quantmod) #biblioteka do to.weekly
library(zoo)
library(xtable)
library(fGarch)

#pobieramy dane
setwd("C:/Users/Olcia/Desktop/Projektryzyko")
apl=read.csv('aapl_us_d.csv')
dis=read.csv('dis_us_d.csv')
fb=read.csv('fb_us_d.csv')
nke=read.csv('nke_us_d.csv')
hpq=read.csv('hpq_us_d.csv')
int=read.csv('intc_us_d.csv')
coc=read.csv('ko_us_d.csv')
kor=read.csv('kors_us_d.csv')
nfx=read.csv('nflx_us_d.csv')
pm=read.csv('pm_us_d.csv')
s1<-apl
s2=dis
s3=fb
s4=nke
s5=hpq
s6=int
s7=coc
s8=kor
s9=nfx
s0=pm
#bedziemy generowaæ wykresy wiêc zamieniamy dane po to aby by³t w formacie szeregu czasowego i zamieniamy na dane tygodniowe
spolki=list(s1,s2,s3,s4,s5,s6,s7,s8,s9,s0)
names=c('Apple','Disney','Facebook','Nike','HP','Intel','CocaCola','MichaelKors','Netflix','PhillipMorris')
for(i in 1:10){
  colnames(spolki[[i]])=c("Date","Open","High","Low","Close","Volume")
  spolki[[i]]$Date=as.Date(spolki[[i]]$Date)
  spolki[[i]]=read.zoo(spolki[[i]],header=TRUE)
  spolki[[i]]=to.weekly(spolki[[i]])
}
#generujemy wykresy OHLC
for (i in 1:10){
setEPS()
postscript(paste(names[i],".eps",sep=""),width = 10, heigh=5.25)
chartSeries((spolki[[i]]),theme=chartTheme('white'),type='candlesticks',name=names[i])
graphics.off()
}



#robimy tabele data zamkniêcie
apl<-apl[,c(1,5)] 
dis<-dis[,c(1,5)]
fb<-fb[,c(1,5)]
nke<-nke[,c(1,5)]
hpq<-hpq[,c(1,5)]
int<-int[,c(1,5)]
coc<-coc[,c(1,5)]
kor<-kor[,c(1,5)]
nfx<-nfx[,c(1,5)]
pm<-pm[,c(1,5)]
#³¹czymy w jedn¹ tablicê ceny zamkniêcia po datach
dane=merge(apl,dis,by=1)
dane=merge(dane,fb,by=1)
dane=merge(dane,nke,by=1)
dane=merge(dane,hpq,by=1)
dane=merge(dane,int,by=1)
dane=merge(dane,coc,by=1)
dane=merge(dane,kor,by=1)
dane=merge(dane,nfx,by=1)
dane=merge(dane,pm,by=1)
names(dane)=c('Date','apl','dis','fbk','nke','hpq','int','coc','kor','nfx','phm') #zmieniam nazwy kolumn
dane_all<-dane #tworzymy tablicê dla dziennych cen zamkniêcia



dotyg<-dane_all
dotyg[,1]=as.Date(dotyg[,1]) #konwertujemy 1 col na datê do szeregu czasowego bo chcemy mieæ tygodniowe stopy

dotyg=read.zoo(dotyg) #wczytujemy dane jako szereg czasowy
tygodniowe=to.weekly(dotyg,OHLC=FALSE) #zamieniamy dane dzienne na tygodniowe
dat=index(tygodniowe)
tygodniowe=as.matrix(tygodniowe) #zamieniamy na macierz ¿eby nam ³adnie stopy zwrotu liczy³o bo inaczej nie policzy
return_all=tygodniowe[-1,]/tygodniowe[-nrow(tygodniowe),]-1 #powsta³y tygodniowe proste stopy zwrotu

return_all

mu_all=colMeans(return_all) #wektor œrednich dla prostych tygodniowych stóp zwrotu
cov_all=cov(return_all)  #macierz kowariancji prostych tygodniowych stóp zwrotu


####### czêœæ 2

#tworzymy wagi przy minimalnj wariancji dla pierwszych 12 okresów
wagi_all=array(0,c(92,10)) #tworzymy pust¹ tablicê w której bêdziemy zapisywaæ wagi
#bêdziemy chcieli wektor wag wiêc definiujemy macierze do policzenia wag ze wzoru z wyk³adu

jpoz=rep(1,10) #biorê sobie wektor jednostkowy
j=as.matrix(jpoz)




tyg_mu<-array(0,c(92,10)) #œrednie ze spó³ek z pierwszych 12 okresów i kolejnych

rX=rep(0,92)
srednia=NULL
odchylenie=NULL
for (i in 1:92){ #mam 104 stopy 92+12 #bêdê wyznaczaæ wagi optymalne ze wzoru
  
  k=11+i
  kk=k+1
  
  tyg_mu[i,]=colMeans(return_all[i:k,]) #œrednie stopy dla itego okresu
  tyg_cov=as.matrix(cov(return_all[i:k,]))
  
  
 
  A=tyg_mu[i,]%*%(solve(tyg_cov))%*%j 
  B=tyg_mu[i,]%*%(solve(tyg_cov))%*%as.numeric(t(tyg_mu[i,]))
  C=jpoz%*%solve(tyg_cov)%*%j
  
  
  me=mean(colMeans(return_all[i:k,]))
   
  
  
  wagiX=(C*me-A)/(B*C-A^2)*as.numeric(tyg_mu[i,]%*%solve(cov(return_all[i:k,])))+(B-A*me)/(B*C-A^2)*as.numeric(jpoz%*%solve(cov(return_all[i:k,])))
  wagi_all[i,]=as.numeric(wagiX)
  
  rX[i]=return_all[kk,]%*%wagiX #tu bêdzie zapisywaæ stopy zwrotu
  
  srednia[i]=wagiX%*%tyg_mu[i,] #to moze nie byæ potrzebne, a jednak potrzebne do sharpa ;)
  odchylenie[i]=sqrt(wagi_all[i,]%*%tyg_cov%*%wagi_all[i,])
  
}  
sum(wagi_all)
X=1000*cumprod(1+rX) 
portfelX=c(1000,X) #wartoœæ portfela X
X
wagiY=rep(1/10,10)
rY=return_all[13:104,]%*%wagiY #stopa mno¿ymy macierzowo ka¿dy wiersz przez 1/10 i sumujemy 
Y=1000*cumprod(rY+1)
portfelY=c(1000,Y)

as.matrix(cbind(rX,rY))


#generujemy wykresy stóp zwrotu i wartoœci portfela

setEPS()
postscript('stopy.eps',width=10,height=8)
plot(rX,type="l",main="Stopy zwrotu dla portfeli X i Y",xlab="Numer okresu inwestycyjnego",ylab="Wysokoœæ stopy")
lines(rY,col="red",type="l")
legend("topright",c("stopy zwrotu z portfela X","stopy zwrotu z portfela Y"),lty=c(1,1),col=c("black","red"))
graphics.off()


setEPS()
postscript('wartosc.eps',width=10,height=8)
plot(portfelX,type="l",main="Wartoœæ portfeli X i Y",xlab="Numer okresu inwestycyjnego",ylab="Wartoœæ portfela")
lines(portfelY,col="red",type="l")
legend("topleft",c("Portfel X","Portfel Y"),lty=c(1,1),col=c("black","red"))
graphics.off()


shX=mean(rX)/sd(rX) #sharp ratio
SHy=mean(rY)/sd(rY)

View(wagi_all)





#tworzymy jak w tresci

portfel_ret_all<-cbind(rX,rY)
portfel_mu_all<-c(mean(rX),mean(rY))
portfel_sd_all<-c(sd(rX),sd(rY))
##### bêdziemy rysowaæ efficient frontier !





analityczna=Vectorize(function(m) #analiza dla drugiego okresu analityczna zwraca nam dla œredniej odchylenie te¿ j¹ wektoryzujemy
{ return_cov=cov(return_all[2:13,]);
  return_mu=colMeans(return_all[2:13,]);
  jedynka=rep(1,times=10)
  A=return_mu%*%(solve(return_cov))%*%jedynka
  B=return_mu%*%(solve(return_cov))%*%return_mu
  C=jedynka%*%solve(return_cov)%*%jedynka
  ciag=seq(from=min(return_mu),to=max(return_mu),length=100)
  
  result=sqrt((C*m^2-2*A*m+B)/(B*C-A^2))
  return(result)
})


portf.satat=function(wagi) #ta funkcja zwraca nam dla wag œredni¹ i odchylenie
{ wagi=as.numeric(wagi)
  srednia<-wagi%*%colMeans(return_all[2:13,])
  odchylenie<-sqrt(wagi%*%cov(return_all[2:13,])%*%wagi)
  return(c(odchylenie,srednia))
}
o2x=portf.satat(wagi_all[2,]) #bêdziemy liczyæ sharpa dla okresu tego
o2y=portf.satat(rep(1/10,10))

okres2x=c(portf.satat(wagi_all[2,])[1],portf.satat(rep(1/10,10))[1]) #do wykresu
okres2y=c(portf.satat(wagi_all[2,])[2],portf.satat(rep(1/10,10))[2]) #do wykresu dla nazw portfeli
#odchylenie #œrednia
#tu sobie sprawdza³am czy maj¹ równe œrednie




#sprawdzam sobie wartoœæ odchylenia isredniej dla portfeli
portf.satat(wagi_all[2,])
portf.satat(rep(1/10,10))
ciag=seq(-0.01,0.025,0.001)
#generuje wykres efficient frontier
setEPS()
postscript('ef.eps',width = 10,height = 6)
plot(analityczna(ciag),ciag,ylim=c(-0.025,0.04),xlim=c(0.01,0.03),type="l",col="blue",main="Efficient Frontier dla okresu 2", xlab="odchylenie",ylab="œrednia")
points(x=portf.satat(wagi_all[2,])[1],y=portf.satat(wagi_all[2,])[2],pch=19)
points(x=portf.satat(rep(1/10,10))[1],y=portf.satat(rep(1/10,10))[2],pch=19,col="red")

legend("topright","granica efektywnoœci",lwd=1,col="blue")
text(okres2x,okres2y,c("portfel X","portfel Y"),pos=4)

graphics.off()

xtable(round(rbind(wagi_all[2,],rep(1/10,10),wagi_all[2,]-rep(1/10,10)),6),digits = 6)


############################################################################################################################





####analiza ryzyka


varh=NULL

Varhist=function(x,a=0.05) #estymator var emp
{       x=sort(x)
p=-1*(x[floor(length(x)*a)+1])
return(p)
}


Varnorm=function(x,a=0.05) #var normalny
{
  v=-(mean(x)+sd(x)*qnorm(a))
  return(v)
}

#estymacja var dla rozk³adu t-studenta
#i analiza wraz z wy³¹czon¹ jedn¹ stop¹

studentX=NULL
which.max(rX) #wartoœæ odstaj¹ca
rXp=rX[-32] #usuwamy bo nam coœ nie gra estymatory nie sa podobne funkcja std fit generuje b³¹d

stX=as.numeric(stdFit(rX)$par)
studentX=sort(rstd(100000,stX[1],stX[2],stX[3]))
quantile(studentX,0.05,type=6) #VAR DLA TSTUDENtA

stXp=as.numeric(stdFit(rXp)$par)
studentXp=sort(rstd(100000,stXp[1],stXp[2],stXp[3]))
qstd(0.05,stXp[1],stXp[2],stXp[3])
quantile(studentXp,0.05,type=6) #VAR POusuniêciu obserwacji


stY=as.numeric(stdFit(rY)$par) #dopasowanie dla portf Y
studentY=sort(rstd(100000,stY[1],stY[2],stY[3]))



hist=c(Varhist(rX),Varhist(rY))
norm=c(Varnorm(rX),Varnorm(rY))
tstud=c(-quantile(studentX,0.05,type=6),-quantile(studentY,0.05,type=6))

hist1=c(Varhist(rXp),Varhist(rY))
norm1=c(Varnorm(rXp),Varnorm(rY))
tstud1=c(-quantile(studentXp,0.05,type=6),-quantile(studentY,0.05,type=6))
rbind(hist,norm1,tstud1)




  
risk_all=rbind(hist,norm,tstud)#przed usunieciem, popraw¹
risk_all1=rbind(hist1,norm1,tstud1)#po usunieciu

colnames(risk_all)=c('X','Y')
rownames(risk_all)=c('hist','norm','t-stud')
colnames(risk_all1)=c('X','Y')
rownames(risk_all1)=c('hist','norm','t-stud')


#################333

es1=function(x,a=0.08)
{
  k=sort(x)
  M=mean(k[1:(floor(length(x)*a)+1)])
  return(-M)
}

pnX=rnorm(100000,mean(rX),sd(rX)) #generujemy rozk³ad normalny do monte carlo
pnY=rnorm(100000,mean(rY),sd(rY))
pnXp=rnorm(100000,mean(rXp),sd(rXp)) #po usuniêciu stopy


cvar_all=rbind(c(es1(rX),es1(rY)),c(es1(pnX),es1(pnY)),c(es1(studentX),es1(studentY)))
cvar_all1=rbind(c(es1(rXp),es1(rY)),c(es1(pnXp),es1(pnY)),c(es1(studentXp),es1(studentY)))



colnames(cvar_all)=c('X','Y')
rownames(cvar_all)=c('hist','norm','t-stud')
colnames(cvar_all1)=c('X','Y')
rownames(cvar_all1)=c('hist','norm','t-stud')






es1=function(x) #expected shortfall historyczne
{a=0.08
k=sort(x)
M=mean(k[1:(floor(length(x)*a)+1)])
return(-M)
}





#minimalizujemy ryzyko dla portfela wektoryzujemy

minimize=Vectorize(function(a){
  return(es1(a*rX+(1-a)*rY))
})



wrisk=optimize(minimize,c(0,1))$minimum
wagi_risk=c(wrisk,1-wrisk) #otrzymane wagi dajemy





wynik=list(dane_all,return_all,mu_all,cov_all,wagi_all,portfelX,portfelY,portfel_ret_all,
           portfel_mu_all,portfel_sd_all,risk_all,cvar_all,wagi_risk)
names(wynik)=(c('dane_all','return_all','mu_all','cov_all','wagi_all','portfelX','portfelY',
                'portfel_ret_all','portfel_mu_all','portfel_sd_all','risk_all','cvar_all','wagi_risk'))

#dane_all,return_all,mu_all,cov_all,wagi_all,portfelX,portfelY, portfel_ret_all,
#portfel_mu_all,portfel_sd_all
