pro Pgderivatives,T,Pe,Pg,dlogPgdT,dlogPgdPe

Tch=string(t)
Pech=string(pe)
spawn, 'echo '+Tch+','+Pech+' | /home/brc/SIR/program/Pgderivatives_i.x > borrame'
openr,1,'output_Pgderivatives_i'
a=' '
readf,1,a
data=fltarr(3)
readf,1,data
close,1
spawn,'rm borrame output_Pgderivatives_i'

Pg=data(0)
dlogPgdT=data(1)
dlogPgdPe=data(2)

return
end
