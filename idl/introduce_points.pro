pro introduce_points,x,i,q,u,v,x1,i1,q1,u1,v1

i1=interpol(i,x,x1) & q1=interpol(q,x,x1) & u1=interpol(u,x,x1)& v1=interpol(v,x,x1)
n1=n_elements(x1)
thres=(x1(1)-x1(0))/5.

for inew=0,n1-1 do begin
   dw=where(abs(x1(inew)-x) lt thres, ndw)
   if(ndw ne 1)then begin
     i1(inew)=-1. & q1(inew)=-1. & u1(inew)=-1. & v1(inew)=-1.
   endif
endfor

return
end