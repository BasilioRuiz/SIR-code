pro leemoderr,nom

nommod=nom+'.mod'
nomerr=nom+'.err'


leemod,nommod,tau,t,p,vmic,b,v,g,f,vmac,fill,d
leemod,nomerr,taue,te,pe,vmice,be,ve,ege,efe,vmace,fille,de

!p.multi=[0,1,2]

taux=1.2*tau(0)
taum=1.2*min(tau)

plot,tau,t,yrange=[.8*min(t-te),1.2*max(t+te)],xrange=[taum,taux],xstyle=1,ystyle=1
oploterr,tau,t,te

v=v*1.e-5
ve=ve*1.e-5
vv=min(v-abs(ve))
vmin= .9*vv<1.1*vv 
vmin=vmin-.1*max(v+ve)

plot,tau,v,yrange=[vmin,1.1*max(v+ve)],xrange=[taum,taux],xstyle=1,ystyle=1

oploterr,tau,v,ve

end



