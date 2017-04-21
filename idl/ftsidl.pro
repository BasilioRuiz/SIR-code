pro ftsidl,inicio,final

inicio=inicio-1
final=final-1


close,/all
openr,1,'FTSLINES
lambda=fltarr(500)
indice=fltarr(500)
potexc=fltarr(500)
loggf=fltarr(500)
n=0
print,'The following lines will be extracted:'
while not eof(1) do begin
  readf,1,ii,'abc',jj,ll,zef,pot,gf

  print,ii
  
  indice(n)=ii
  lambda(n)=ll
  potexc(n)=pot
  loggf(n)=gf
  
   print,indice(n),lambda(n)
  n=n+1
endwhile
close,1
lambda=lambda(inicio:final)
indice=indice(inicio:final)
potexc=potexc(inicio:final)
loggf=loggf(inicio:final)

loadct,2



minimo=fix(min(lambda)-2.5)
maximo=fix(max(lambda)+2.5)
ftsread,si,minimo,maximo,xlam=wave
si=si/10000.

sn=''
s=''
for i=0,final-inicio do begin
    print,'Spectral line at lambda=',lambda(i)
    a=where(wave gt lambda(i)-.5 and wave lt lambda(i)+.5)
t:  plot,wave(a)-lambda(i),si(a),/ynoz,xsty=1
    xyouts,.60,.25,/normal,strtrim(lambda(i)),size=1.7
    oplot,wave(a)-lambda(i),si(a),color=30,thick=2
    print,'Click with the mouse to select the first wavelength of the line:'
    cursor,waveini,y,3
    ver,waveini,color=80,linestyle=2
    print,'Select the last wavelength of the line'
    cursor,wavefin,y,3
    ver,wavefin,color=80,linestyle=2

    print,'Is it OK? yes=y, no=n, exit=e'
    read,sn
    if sn eq 'n' then goto,t
    if sn eq 'e' then goto,e
    
    print,'Do you want to normalize the profile to the continuum intensity? (y/n)'
    read,s
    factor=1.
    
    a=where(wave gt waveini+lambda(i) and wave lt wavefin+lambda(i))
    
    if s eq 'y' then begin
       apin=where(wave gt lambda(i)-1. and wave lt lambda(i)+1.)
tt:    plot,wave(apin)-lambda(i),si(apin),/ynoz,yrange=[0.85,1.01],ysty=1,thick=2
       oplot,wave(a)-lambda(i),si(a),color=80,thick=2
       hor,1.
       print,'Select with the mouse the wavelength of the continuum'
       cursor,wavecont,y,3
;       hor,y,color=80
       ver=min(where(wave ge wavecont+lambda(i)))
       hor,si(ver),color=80
       print,'Continuum at lambda=',wave(ver)
       print,'Is it OK? yes=y, no=n, exit=e'
       read,sn
       if sn eq 'n' then goto,tt
       if sn eq 'e' then goto,e

       factor=si(ver)    
     endif 

    print,'The intensity profile is being divided by ',factor 

    landas=a
    ii=a
    v=a
    print,wave(a)
    landas=(wave(a)-lambda(i))*1000.


    ii=si(a)/factor
    v=0.
    wperfiles,'idl'+strtrim(fix(indice(i)),2)+'.per',indice(i),landas,ii,ii*0.,ii*0.,v
    

endfor

e:
   
end




