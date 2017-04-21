@ readfits
@ writefits
@ coord_cart_helio_BRC
@ FFF
;...................................................... 
function to_plus_minus_180,y

to_plus_minus_180=atan(sin(y*!dtor),cos(y*!dtor))/!dtor

return,to_plus_minus_180
end
;......................................................

pro desam_brc,B,gamma,phi,phi_output,pixel=pixel,r0=r0,l0=l0,b0=b0,p0=p0,x_center=x_center,y_center=y_center 

; ejemplo de ejecucion
; desam_brc,B,gamma,phi,pixel=[0.136,0.136],r0=945.5,l0=0.,b0=1.27,p0=-8.85,x_center=187.95,y_center=-324.70

;-------------------------------------------------------------------------------------------------------
;  Hace la desambiguacion de un mapa B,gamma,fi siguiendo el siguiente procedimiento:
;
; 1) busca la solucion del azimuth que forma un angulo agudo con un campo potencial
; que tenga la misma Bz (una vez rotado para que en una cierta zona especificada
; el azimuth potencial tenga el mismo promedio que el input)
;
; 2) Se modifica el azimuth iterativamente de tal forma que se minimice divBh^2+rotBh^2
; donde divBh=dBx/x+dBy/y (es decir la divergencia de las componentes horizontales del campo)
; y rotBh=dBy/dx-dBx/dy (es decir la componente z del rotacional de B, que es proporcional a
; la componete z de la fuerza magnetica
; 3) Como el resultado tiene en algunos puntos una solucion ajedrezada, le paso un filtro
; (cambiando el pixel central de cad damero por el promedio)
; 4) Puede ser UPDATED añadiendo la derivada dBz/dz a la divergencia (cuando dBz/dz sea conocida) 
;-------------------------------------------------------------------------------------------------------

; INPUT
; B,gamma,fi  : mapas 2-D (angulos en grados)
; pixel=      : array 2 elementos con el tamaño del pixel en segundos de arco. Ejemplo [0.136,0.136]   
; r0          : radio del Sol en segundos de arco.     Ejemplo  r0=945.5             
; l0          : Longitude at center of disk (deg).     Ejemplo  l0=0.
; b0          : Latitude at center of disk (deg).      Ejemplo  b0=1.27
; p0          : Position angle of rotation axis (deg). Ejemplo  p=-8.85       
; x_center    : Position of the disk center (arcsec)   Ejemplo  x_center=187.95       
; y_center    : Position of the disk center (arcsec)   Ejemplo  y_center=-324.70     

; OUTPUT 
; phi_output  : mapa de azimuth desaambigueado

;--------------------------------------------------------------------------------
;1) 
; llamamos a coord_cart_helio_BRC,r0,l0,b0,p0,x_center,y_center,x,y,l,b
;   OUTPUTS     
; 		l	-  heliographic longitude of position (X,Y) grados
;
;               b	-  heliographic latitude of position (X,Y)  grados
;

loadct,39
window,0

;pixel=[0.136,0.136]   ; pixel en segundos de arco
;r0=945.5              ; sec
;l0=0.                 ; no lo se
;b0=1.27               ; deg
;p=-8.85               ; deg
;x_center=187.95       ; arcsec
;y_center=-324.70      ; arcsec

coord_cart_helio_BRC,r0,l0,b0,p0,x_center,y_center,l,bparam

;transformamos todo a radianes
lrad=l*!dtor
brad=bparam*!dtor
b0rad=b0*!dtor
prad=p0*!dtor

;--------------------------------------------------------------------------------
;2) calculamos el campo potencial

phi_input=phi
bzin=B*cos(gamma*!dtor)         ; esta es la componente vertical del campo de entrada

Hz=0.03 ; altura maxima en Mm de la region a extrapolar
alpha=0. ; alpha = Force free alpha in units of 1/(x pixels).  rotacional(B)=alpha*B asi alpha=0. es potencial, alpha= cte = linear force free

bff = FFF(b0=b0rad, pangle=prad, cmd=lrad, lat=brad, bzin, PIXEL=pixel,alpha=alpha,/image,normal=0)
BXout=bff(*,*,0) & BYout=bff(*,*,1) & BZout=bff(*,*,2)

bout=sqrt(BXout*BXout+ BYout*BYout+ BZout*Bzout)
gamma_potencial=acos(BZout/BOUT)/!dtor
azi_potencial=atan(BYout,BXout)/!dtor

;--------------------------------------------------------------------------------
;4)
; definimos una zona de referencia donde nos guste el azimuth de la imagen original
xg1=60 & xg2=75 & yg1=0 & yg2=15 ; media az input       101.24759 media az pot     -22.9092 offset=      -124.15677
xg1=55 & xg2=56 & yg1=0 & yg2=10 

!p.multi=[0,2,3]
!p.charsize=2.

; ¿hay un desplazamiento de la direccion?
busca_maximocoseno,gamma,xmxgamma,ymxgamma
busca_maximocoseno,gamma_potencial,xmxgammapot,ymxgammapot

tvframe,gamma,/bar,/sample,/aspect,title='gamma IN',charsize=2.                     & ver,xmxgamma    & hor,ymxgamma
tvframe,gamma_potencial,/bar,/sample,/aspect,title='gamma POTENCIAL',charsize=2.    & ver,xmxgammapot & hor,ymxgammapot

tvframe,phi_input,/bar,/sample,/aspect,title='az IN',charsize=2.                    & ver,xmxgamma       & hor,ymxgamma     & rectangulo,xg1,xg2,yg1,yg2,0
;tvframe,azi_potencial,/bar,/sample,/aspect,title='az POTENCIAL',charsize=2.        & ver,xmxgammapot    & hor,ymxgammapot  & rectangulo,xg1,xg2,yg1,yg2,0

print,'gamma image displacement (pixels)', xmxgammapot-xmxgamma,ymxgammapot-ymxgamma

media_phi_input_region=mean(phi_input(xg1:xg2,yg1:yg2))
media_azi_potencial_region=mean(azi_potencial(xg1:xg2,yg1:yg2))
offset=media_azi_potencial_region-media_phi_input_region
print,'media az input',media_phi_input_region,' media az pot',media_azi_potencial_region,' offset=',offset
azi_potencial=to_plus_minus_180(azi_potencial-offset)

tvframe,azi_potencial,/bar,/sample,/aspect,title='az POTENCIAL',charsize=2.         & ver,xmxgammapot    & hor,ymxgammapot  & rectangulo,xg1,xg2,yg1,yg2,0

; azi_potencial  esta entre -180 y 180 pero phi_input está entre 0 y 180
; buscamos la solución que forme un angulo agudo con la potencial, es decir
; hay que sumar 180 grados a phi_input si el producto escalar de los vectores unitarios en las direcciones
; de phi_input y azi_potencial es negativo

scalar_product=cos(phi_input*!dtor)*cos(azi_potencial*!dtor)+sin(phi_input*!dtor)*sin(azi_potencial*!dtor)
dw=where(scalar_product lt 0)

modifica=azi_potencial*0.
modifica(dw)=+180.              ; sumo 180 grados al azimuth en dichos puntos
phi_output=to_plus_minus_180(phi_input+modifica)

tvframe,phi_output,/bar,/sample,/aspect,title='az OUT',charsize=2.     & ver,xmxgammapot    & hor,ymxgammapot  & rectangulo,xg1,xg2,yg1,yg2,0

Bx_output=B*sin(gamma*!dtor)*cos(phi_output*!dtor)
By_output=B*sin(gamma*!dtor)*sin(phi_output*!dtor)
Bz_output=B*cos(gamma*!dtor)

Bx1=Bx_output & By1=By_output             & fi1=atan(By1,Bx1)/!dtor
corrijo,Bx_output,By_output,Bz_output

Bx2=Bx_output & By2=By_output             & fi2=atan(By2,Bx2)/!dtor
corrijo,Bx_output,By_output,Bz_output

Bx3=Bx_output & By3=By_output             & fi3=atan(By3,Bx3)/!dtor
corrijo,Bx_output,By_output,Bz_output

Bx4=Bx_output & By4=By_output             & fi4=atan(By4,Bx4)/!dtor

ajedrez,fi4,fi1,fiout

phi_output=fiout
tvframe,phi_output,/bar,/sample,/aspect,title='az FINAL',charsize=2.     & ver,xmxgammapot    & hor,ymxgammapot  & rectangulo,xg1,xg2,yg1,yg2,0

;Bx_output=B*sin(gamma*!dtor)*cos(phi_output*!dtor)
;By_output=B*sin(gamma*!dtor)*sin(phi_output*!dtor)
;Bz_output=B*cos(gamma*!dtor)
;Bx5=Bx_output & By5=By_output             & fi5=atan(By4,Bx4)/!dtor
;corrijo,Bx_output,By_output,Bz_output
;                                            fi6=atan(By_output,Bx_output)/!dtor
;
;tvframe,fi6,/bar,/sample,/aspect,title='az FINAL',charsize=2.     & ver,xmxgammapot    & hor,ymxgammapot  & rectangulo,xg1,xg2,yg1,yg2,0
;ajedrez,fi6,fi1,fiout
;tvframe,fiout,/bar,/sample,/aspect,title='az FINAL',charsize=2.
;stop

return
end


;------------------------------------------------------------------------------------------
pro busca_maximocoseno,gamma,xmaxgamma,ymaxgamma

ap=5 ; tamaño ventana de suavizado
ss=size(gamma) & nx=ss(1) & ny=ss(2) 

gss=smooth(abs(cos(gamma*!dtor)),ap) 
mn=min(gss(ap:nx-ap-1,ap:ny-ap-1))& gss1=gss*0+mn & gss1(ap:nx-ap-1,ap:ny-ap-1)=gss(ap:nx-ap-1,ap:ny-ap-1)
dwmax=where(gss1 eq max(gss1))  & xmaxgamma=dwmax mod nx & ymaxgamma=dwmax/nx

return
end
;......................................................
pro rectangulo,xg1,xg2,yg1,yg2,col

plots,[xg1,xg2],[yg1,yg1],col=col
plots,[xg1,xg2],[yg2,yg2],col=col
plots,[xg1,xg1],[yg1,yg2],col=col
plots,[xg2,xg2],[yg1,yg2],col=col
return
end
;......................................................
pro divergencia_rot,Bx,By,Bz,div,rota
; suponemos que el espaciado de la malla en ambas direcciones es 0.5
; si a escala fuera delta, los resultados deberan ser multiplicados por
; 1/(2*delta)
; las divergencias y rotacionales se evaluan en el centro de cada malla
; Si la malla tiene nx*ny valores el resultado sera por tanto evaluado en
; (nx-1)*(ny-1) puntos pero escrito en el vertice inferior izquierdo
; asi la divergencia en el cenro de la malla con fila (B11,B21) y fila (B12,B22)
; se guarda en divB11

ss=size(Bx)
nx=ss(1) & ny=ss(2)
div=Bx*0. & rota=div
for ix=0,nx-2 do begin
  for iy=0,ny-2 do begin
     Bx11=Bx(ix,iy)   & Bx21=Bx(ix+1,iy)   & By11=By(ix,iy)   & By21=By(ix+1,iy)
     Bx12=Bx(ix,iy+1) & Bx22=Bx(ix+1,iy+1) & By12=By(ix,iy+1) & By22=By(ix+1,iy+1)
     div(ix,iy) =(Bx22+By22-Bx11-By11-Bx12+By12+Bx21-By21)
     rota(ix,iy)=(Bx22-By22-Bx11+By11+Bx12+By12-Bx21-By21)
  endfor
endfor

return
end
;......................................................
pro corrijo,Bx,By,Bz

Bxinicial=Bx & Byinicial=By
divergencia_rot,Bx,By,Bz,divinicial,rotainicial
errorinicial=smooth(rotainicial^2+divinicial^2,2) & errtotinicial=total(errorinicial)

ss=size(Bx) & nx=ss(1) & ny=ss(2)
icambio=0
nmax=((nx-2)*(ny-2))-1.

dwcambio=intarr(nmax)
cambio=intarr(nx,ny)

while( icambio lt nmax) do begin
  Bx0=Bx & By0=By & Bz0=Bz 
  divergencia_rot,Bx0,By0,Bz0,div,rota
  errorold=smooth(rota^2+div^2,2) & errtotold=total(errorold)
  errornew=errorold               
  if(icambio gt 0)then for ii=0,icambio-1 do errornew(dwcambio(ii))=0 

  dw=where(errornew eq max(errornew)) & dwx=dw(0) mod nx & dwy=dw(0)/nx & dwcambio(icambio)=dw(0)
  Bx0(dwx,dwy)=-Bx0(dwx,dwy)
  By0(dwx,dwy)=-By0(dwx,dwy) 
  divergencia_rot,Bx0,By0,Bz0,div1,rota1
  error1=smooth(rota1^2+div1^2,2)& errtot1=total(error1)
  if(errtot1 lt errtotold)then begin
    Bx=Bx0 &  By=By0 
    cambio(dwx,dwy)=1
  endif ;else begin
  icambio=icambio+1

endwhile
print,'Hemos cambiado ',total(cambio),' pixeles'

divergencia_rot,Bx,By,Bz,divfinal,rotfinal
errorfinal=smooth(rotfinal^2+divfinal^2,2) & errtotfinal=total(errorfinal)

print,'El error ha cambiado de  ',errtotinicial,' a',errtotfinal

return
end
;......................................................

pro ajedrez,fi3,fi1,fiout

fiout=fi3
ss=size(fi3) & nx=ss(1)
dif=fi3-fi1
dif2=dif-smooth(dif,2)
dw=where(dif2^2 gt 100.^2, ndw)
if(ndw gt 0)then begin
  dwx=dw mod nx & dwy=dw/nx
  for ii=0,ndw-1 do begin
    i=dwx(ii) & j=dwy(ii)   
    fiout(i,j)=0.25*(fi3(i-1,j)+fi3(i+1,j)+fi3(i,j-1)+fi3(i,j+1))
  endfor  
endif

return
end