pro read_model,nom,logtau,t,p,vmic,b,v,g,f,z,pg,ro,vmac,fill,stray

openr,1,nom
readf,1,vmac,fill,stray
n=0
while not eof(1) do begin
   readf,1,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11
   n=n+1
endwhile
close,1

dat=fltarr(11,n)
openr,1,nom
readf,1,vmac,fill,stray
readf,1,dat
close,1

logtau=fltarr(n)
logtau(*)=dat(0,*)
t=logtau
p=t
vmic=t
b=t
g=t
f=t
v=t
z=t
pg=t
ro=t
t(*)=dat(1,*)
p(*)=dat(2,*)
vmic(*)=dat(3,*)
b(*)=dat(4,*)
g(*)=dat(6,*)
f(*)=dat(7,*)
v(*)=dat(5,*)
z(*)=dat(8,*)
pg(*)=dat(9,*)
ro(*)=dat(10,*)


end
