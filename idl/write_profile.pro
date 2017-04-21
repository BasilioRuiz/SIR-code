; ************************* WRITE_PROFILE *********************************

pro write_profile,nom,indice,lamda,i,q,u,v

n=n_elements(lamda)
dat=fltarr(6,n)
dat(0,*)=indice
dat(1,*)=lamda
dat(2,*)=i
dat(3,*)=q
dat(4,*)=u
dat(5,*)=v

openw,1,nom
  printf,1,dat
close,1

end
