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

stX=as.numeric(stdFit(rX)$par) #dopasowujesz dane do rozk³adu tstudenta jak nie bêdzie dzia³aæ to sprawdŸ w necie z jakiej to 
#biblioteki i ewentualnie zainstaluj
studentX=sort(rstd(100000,stX[1],stX[2],stX[3])) #sortujesz
quantile(studentX,0.05,type=6) #VAR DLA TSTUDENtA #bierzesz var na poziomie 5% type 6, bo R kwantyle liczy na wiele sposobów,
#a nr 6 liczy tak jak my chcemy