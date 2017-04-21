pro write_model,nom,logtau,t,p,vmic,b,v,g,f,z,pg,ro,vmac,fill,stray

a=fltarr(11,n_elements(t))
a(0,*)=logtau
a(1,*)=t
a(2,*)=p
a(3,*)=vmic
a(4,*)=b
a(5,*)=v
a(6,*)=g
a(7,*)=f
a(8,*)=z
a(9,*)=pg
a(10,*)=ro

openw,1,nom,width=200
  printf,1,vmac,fill,stray
  printf,1,a
close,1

end
