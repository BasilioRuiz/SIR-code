; **************************** READ_PROFILE *********************************

pro read_profile,nom,indi,x,i,q,u,v

openr,1,nom
nlam=0
while not eof(1) do begin
   readf,1,ilin,lam,i00,q00,u00,v00
   nlam=nlam+1
endwhile
close,1

dat=fltarr(6,nlam)
openr,1,nom
readf,1,dat
close,1
 
x=fltarr(nlam)
i=x
q=x
u=x
v=x
indi=intarr(nlam)
 
indi(*)=dat(0,*)
x(*)=dat(1,*)
i(*)=dat(2,*)
q(*)=dat(3,*)
u(*)=dat(4,*)
v(*)=dat(5,*)

end
