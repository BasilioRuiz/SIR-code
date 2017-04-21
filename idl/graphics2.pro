;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;		     PROCEDURE GRAPHICS & RELATED ROUTINES

;Purpose: Plotting of model atmosphere and Stokes profile files with the 
;format required and/or produced by the Inversion of the Radiative Transfer
;Ecuation code by B. Ruiz Cobo & J.C. del Toro Iniesta (1992, ApJ, 398, 375)

;Category: Widgets.

;Calling sequence: GRAPHICS

;Common blocks: No previous common blocks required. See below for description
;		of internal ones.

;Side effects: As with other widget programs, abortion of the program by 
;		error avoids new compilations within the current IDL session.

;Restrictions: 
  ;	-Files must accomplish the above mentioned ASCII format:
;	   - Model files: 
;		Two float scalars followed by an 11 columns 2-D float array.
;	   - Profile files:
;	        6 columns 2-D float array.
;	-The SCALES file must be in ~/bin

;Modification history:
;       Based on TAUMENU
;	(J.C. del Toro Iniesta, March, 18th, 1994
;        C. Westendorp Plaza, February, 10th, 1995
;        L. Bellot Rubio y B. Ruiz Cobo, alla por el verano del 97)
;
;       B. Ruiz Cobo,  February, 7th, 2001
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PRO CWPAL2
;
R=[0,255L,234L,0  ,0  ,255L,0  ,255L,255L,0  ,175L,255L]
G=[0,255L,0  ,140L,216L,235L,235L,0  ,148L,126L,0  ,67L ]*256L
B=[0,255L,0  ,234L,0  ,0  ,228L,200L,0  ,0  ,201L,67L ]*256L*256L
TVLCT, R, G, B
;
END

function col2,col

;R=[0,255L,234L,0  ,0  ,255L,0  ,255L,255L,0  ,175L,255L]
;G=[0,255L,0  ,140L,216L,235L,235L,0  ,148L,126L,0  ,67L ]*256L
;B=[0,255L,0  ,234L,0  ,0  ,228L,200L,0  ,0  ,201L,67L ]*256L*256L
;TVLCT, R, G, B
;
;col2=r(col)+g(col)+b(col)
col2=col

return,col2
end
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;***************************************************************************
;***************************************************************************
pro errcolor,x,y,err,col,miny,maxy

tam=abs(max(x)-min(x))/100.
tamy=(maxy-miny)/30.

donde=where(err ne 0.)
if(donde(0) ne -1)then begin
   num=n_elements(donde)
   for j=0,num-1 do begin
    i=donde(j)

    ymenos=y(i)-err(i)   
    if(ymenos lt miny) then begin
       ymenos=miny   ;+tamy
       ;oplot,[x(i)-tam,x(i)],[ymenos+tamy,ymenos],color=col2(col+1)
       ;oplot,[x(i),x(i)+tam],[ymenos,ymenos+tamy],color=col2(col+1)
    endif else begin
        oplot,[x(i)-tam,x(i)+tam],[ymenos,ymenos],color=col2(col)
    endelse

    ymas=y(i)+err(i)
    if(ymas gt maxy) then begin
      ymas=maxy   ;-tamy
      ;oplot,[x(i)-tam,x(i)],[ymas-tamy,ymas],color=col2(col+1)
      ;oplot,[x(i),x(i)+tam],[ymas,ymas-tamy],color=col2(col+1)
    endif else begin
      oplot,[x(i)-tam,x(i)+tam],[ymas,ymas],color=col2(col)
    endelse

    oplot,[x(i),x(i)],[ymenos,ymas],color=col2(col)
   endfor
endif

return
end
;***************************************************************************
;***************************************************************************
;				FUNCTION QUITAEX 

;Purpose: Extract a filename without extension '.ext', where ext is a string 
;of any length

;Argument: 
;	- name: Filename. Input/output. Scalar string

;Routines called: None.

;***************************************************************************
;***************************************************************************
function quitaex,name

name1=name
ande=strpos(name1,'/')

if ande ne -1 then begin   ; BUG "ppepe"  (CW)
   andesta=0
endif else andesta = -1

while ande ne -1 do begin
   name1=strmid(name1,ande+1,1000)
   ande=strpos(name1,'/')
   andesta=andesta+ande+1
endwhile

donde=strpos(name1,'.')

if donde ne -1 then name=strmid(name,0,andesta+1)+strmid(name1,0,donde)

return,name

end

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;***************************************************************************
;***************************************************************************

;				FUNCTION REORDENA

;Purpose: From two lists of filenames (the second with extension '.per') 
;it sorts them out and compares them to construct a final list. This is such
;that contains those filenames only belonging to the first, followed by those 
;shared by the two initial lists appearing with extension '.1stext\per', and
;finally, those filenames that only belong to the second list. If one of the 
;initial lists is empty, the final one contains the other.

;Arguments:
;	-name1,name2: Initial lists. Input. String arrays.
;	-name: Resulting list. Output. String array.

;Routines called: QUITAEX

;***************************************************************************
;***************************************************************************
function reordena,name1,name2

n1=0
n2=0
if name1(0) ne '' then begin
   name1=name1(sort(name1))
   n1=n_elements(name1)
endif else return,name2
if name2(0) ne '' then begin
   name2=name2(sort(name2))
   n2=n_elements(name2)
endif else return,name1
name11=name1
name22=name2
for i=0,n1-1 do name11(i)=quitaex(name1(i))
for i=0,n2-1 do name22(i)=quitaex(name2(i))

for i=0,n1-1 do begin
   n22=n_elements(name22)
   d=where(name22 eq name11(i))
   d=d(0)
   if d ge 0 then begin
      if d eq 0 then begin
         if n22 eq 1 then begin
	    name22=''
         endif else name22=name22(1:*)
      endif else begin                                    ; (CW)
         if d+1 gt n_elements(name22)-1 then begin        ; Fix bug
            name22=[name22(0:d-1)]			  ;
         endif else name22=[name22(0:d-1),name22(d+1:*)]  ;
      endelse
      name11(i)=name1(i)+'\per'
   endif else name11(i)=name1(i)
endfor

;STOP

n22=n_elements(name22)

for i=0,n22-1 do name22(i)=name22(i)+'.per'

name=[name11,name22]
if name22(0) eq '.per' then name=name11

return,name
end

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;***************************************************************************
;***************************************************************************

;			PROCEDURE MENSAJE_EVENT

;Purpose: Event-handler of the widgets created by MENSAJE.PRO

;Argument: 
;	-suceso: Event.

;Routines called: None.

;***************************************************************************
;***************************************************************************
pro mensaje_event,suceso

widget_control,suceso.id,get_value=valor

if valor ne 'OK' then begin
   bell
endif else widget_control,suceso.top,/destroy

end

;***************************************************************************
;***************************************************************************

;				PROCEDURE MENSAJE

;Purpose: Creates a warning message widget, displaying text

;Argument:
;	-texto: Text. Input. Scalar string.

;Notes: Managed through XMANAGER with 'supermenu' as group leader

;Routines called: None.

;***************************************************************************
;***************************************************************************
pro mensaje,texto

mensa=widget_base(/column)
cartel=widget_text(mensa,value=texto,font='lucidasans-12',xsize=20,/scroll)
boton=widget_button(mensa,value='OK')

bell
widget_control,mensa,/realize

xmanager,'mensaje',mensa,group_leader=supermenu

end

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;***************************************************************************
;***************************************************************************

;			PROCEDURE TEXTOEDITABLE_EVENT

;Purpose: Event-handler of TEXTOEDITABLE.PRO

;Argument:
;	-suceso: Event

;Routines called: None.

;***************************************************************************
;***************************************************************************
pro textoeditable_event,suceso
common fichps,nameps,encaps     ;(CW) Variable "encaps"

widget_control,suceso.id,get_value=valor

if valor(0) eq 'eps' then encaps=1    ;(CW)

if valor(0) ne 'OK' then begin
   nameps=valor(0)
endif else begin
   widget_control,suceso.top,/destroy
endelse

end

;***************************************************************************
;***************************************************************************

;			PROCEDURE TEXTOEDITABLE

;Purpose: Creates an editable widget to display and edit text.

;Notes: Managed through XMANAGER with 'supermenu' as group leader
 
;Routines called: None.

;***************************************************************************
pro textoeditable
common fichps,nameps,encaps

nameps='menu'  ;(CW)
mensa1=widget_base(/column)
cartel=widget_text(mensa1,value=nameps,font='lucidasans-12',xsize=20,$
/scroll,/editable)

mensa2=widget_base(mensa1,/row,/exclusive)        ; (CW)
botps=widget_button(mensa2,value='Postcript',/no_release)   ; 
boteps=widget_button(mensa2,value='Encapsulated-ps',/no_release) ;
boton=widget_button(mensa1,value='OK')

bell
widget_control,mensa1,/realize

widget_control,botps,/set_button   ;(CW)
encaps=0                           ; ps por defecto

xmanager,'textoeditable',mensa1,group_leader=supermenu

end

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;***************************************************************************
;***************************************************************************

;				PROCEDURE ESCALABLE

;Purpose: Sets maximum and minimum values of the second dimension of two 
;3-D arrays containing model atmospheres and Stokes profiles as read by 
;RMODPROF.PRO for displaying purposes. It has two options: automatic and 
;manual. 

;Arguments:
;	-nmod: Number of model atmospheres included (size of the 1st dimension
;	       of RMOD). Input. Scalar integer.
;	-nper: Number of Stokes profiles included (size of the 1st dimension
;	       of RPER). Input. Scalar integer.
;	-rmod: Variable containing the model atmospheres selected by 
;	       TAUMENU.PRO. Input. 3-D Float array.
;	-rper: Variable containing the Stokes profiles selected by 
;	       TAUMENU.PRO. Input. 3-D Float array.
;	-ntau: Variable containing the number of points of each model 
;	       atmosphere selected by TAUMENU.PRO. Input. Float array. (The 
;	       size of the 3rd dimension of RMOD is max(ntau))
;	-nlam: Variable containing the number of points of each Stokes 
;	       profile selected by TAUMENU.PRO. Input. Float array. (The 
;	       size of the 3rd dimension of RPER is max(lam))

;Notes: None of the input parameters is modified within this procedure

;Common blocks:
;	-VARIABLES:
;		-imag: Variable containing information about selected 
;		       physical quantities by TAUMENU.PRO. Byte array of 
;		       10 elements. Each element is 1 if the quantity is 
;		       selected and 0 otherwise.
;		-istokes: Variable containing information about selected 
;		       Stokes parameters by TAUMENU.PRO. Byte array of 
;		       4 elements. Each element is 1 if the parameter is 
;		       selected and 0 otherwise.
;		-win:  Identifier of the displaying window used by 
;		       TAUMENU.PRO.
;		-ips:  Identifier of 'ps'-device for plotting with 
;		       TAUMENU.PRO. Equals 1 if true.
;		-itau: Identifier of the independent variable to plot 
;		       the physical quantities vs. log(tau)=1, z=0.
;		-dm:   Location of model files in the final list generated 
;		       by REORDENA.PRO.
;		-dp:   Location of profile files in the final list generated 
;		       by REORDENA.PRO.
;	-ESCALAS:
;		-maxmod: Variable containing the maximum values of the 
;		       selected physical quantities within the set of models
;		       selected. Output of ESCALABLE.PRO. Float array.
;		-minmod: Variable containing the minimum values of the 
;		       selected physical quantities within the set of models
;		       selected. Output of ESCALABLE.PRO. Float array.
;		-maxper: Variable containing the maximum values of the 
;		       selected Stokes profiles within the set of profile 
;		       files selected. Output of ESCALABLE.PRO. Float array.
;		-minper: Variable containing the minimum values of the 
;		       selected Stokes profiles within the set of profile
;		       files selected. Output of ESCALABLE.PRO. Float array.
;		-iscale: Variable containing the scaling option: 0=automatic,
;		       1=manual.
 
;Important note!!!!: It always reads the ESCALAOR file which is assumed to be
;in ~/bin. This file contains information concerning both the physical 
;parameters and the Stokes profiles through 3 numbers separated by commas:
;iscale, minimum (manual option), maximum (manual option)

;Routines called: None.

;***************************************************************************
;***************************************************************************
pro escalable,nmod,nper,rmod,rper,ntau,nlam

common variables,imag,istokes,win,ips,itau,dm,dp
common escalas,maxmod,minmod,maxper,minper,iscale
common todoslosparametros,tlpmod

im=imag
is=istokes
nm=total(im)
ns=total(is)

dondemag=where(im eq 1)
dondesto=where(is eq 1)

maxmod=fltarr(11)-1.e30
minmod=fltarr(11)+1.e30

maxper=fltarr(4)-1.e30
minper=fltarr(4)+1.e30

a=strarr(15)
b='b'
openr,1,'~/bin/scales'  ; RUTA DIRECTORIO SCALE FILE
for i=0,9 do readf,1,b
readf,1,a
close,1
b=a
c=a
iscale=a

for i=0,14 do begin
   d=strmid(a(i),strpos(a(i),':')+2,strpos(a(i),',')-strpos(a(i),':')-2)
   iscale(i)=d
   d=strmid(a(i),strpos(a(i),',')+1,30)
   b(i)=strmid(d,0,strpos(d,','))
   c(i)=strmid(d,strpos(d,',')+1,30)
endfor
b=float(b)
iscale=byte(float(iscale))
c=float(c)


if nm ne 0 then begin
if itau eq 1 then begin ;!!!!!!!!!!!!!!! si pinto taus
if iscale(0) eq 0 then begin
   xx= rmod(0,0,0:ntau(0)-1)
   maxmod(0)=max(xx)
   minmod(0)=min(xx)
   for j=1,nmod-1 do begin
      xx= rmod(j,0,0:ntau(j)-1)
      maxmod(0)=max([maxmod(0),max(xx)])
      minmod(0)=min([minmod(0),min(xx)])
   endfor
endif else begin
      maxmod(0)=c(0)
      minmod(0)=b(0)
endelse
endif else begin  ;!!!!!!!!!!!!!!!!! si pinto zetas
if iscale(8) eq 0 then begin
   xx= tlpmod(0,8,0:ntau(0)-1)
   maxmod(0)=max(xx)
   minmod(0)=min(xx)
   for j=1,nmod-1 do begin
      xx= tlpmod(j,8,0:ntau(j)-1)
      maxmod(0)=max([maxmod(0),max(xx)])
      minmod(0)=min([minmod(0),min(xx)])
   endfor
endif else begin
      maxmod(0)=c(8)
      minmod(0)=b(8)
endelse

endelse ;!!!!!!!!!!!!!!!!!!!!1


ii=0
for i=1,10 do begin
   if ((iscale(i) eq 0) and (im(i-1) eq 1)) then begin
      ii=ii+1
      xx= rmod(0,ii,0:ntau(0)-1)
      maxmod(i)=max(xx)
      minmod(i)=min(xx)
      for j=1,nmod-1 do begin
         xx= rmod(j,ii,0:ntau(j)-1)
         maxmod(i)=max([maxmod(i),max(xx)])
         minmod(i)=min([minmod(i),min(xx)])
      endfor
   endif else begin
      if (iscale(i) eq 1 and (im(i-1) eq 1) ) then ii=ii+1
      maxmod(i)=c(i)
      minmod(i)=b(i)
   endelse
endfor
   maxmod=[maxmod(0),maxmod(dondemag+1)]
   minmod=[minmod(0),minmod(dondemag+1)]
endif else begin
   maxmod(0:10)=c(0:10)
   minmod(0:10)=b(0:10)
endelse

if ns ne 0 then begin
ii=-1
for i=11,14 do begin
   if ((iscale(i) eq 0) and (is(i-11) eq 1)) then begin
      ii=ii+1
      for j=0,nper-1 do begin
         maxper(i-11)=max([maxper(i-11),max(rper(j,ii,0:nlam(j)-1))])
         minper(i-11)=min([minper(i-11),min(rper(j,ii,0:nlam(j)-1))])
      endfor
   endif else begin
      if iscale(i) eq 1 then ii=ii+1
      maxper(i-11)=c(i)
      minper(i-11)=b(i)
   endelse
endfor
   maxper=maxper(dondesto)
   minper=minper(dondesto)
endif else begin
   maxper(0:3)=c(11:14)
   minper(0:3)=b(11:14)
endelse

if ((nm ne 0) and (ns ne 0)) then begin
   iscale=[iscale(0),iscale(dondemag+1),iscale(dondesto+11)]
endif else begin
   if nm ne 0 then begin
      iscale=[iscale(0),iscale(dondemag+1)]
   endif 
   if ns ne 0 then begin
      iscale=[iscale(0),iscale(dondesto+11)]
   endif
endelse

end

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;***************************************************************************
;***************************************************************************

;				PROCEDURE RMODPROF

;Purpose: Reads model and profile files selected by TAUMENU.PRO and stores
;the selected physical quantities and Stokes profiles into two separated 
;3-D vairables.

;Arguments:
;	-nm:   Total number of physical quantities selected. Input. Scalar 
;	       integer.
;	-nmod: Number of model files selected. Output. Scalar integer.
;	-nper: Number of profile files selected. Output. Scalar integer.
;	-resultmod: Variable containing the selected quantities in the 
;	       selected models. Output. 3-D Float array. Size of the 1st 
;	       dimension: nmod. Size of the 2nd dimension: nm. Size 
;	       of the 3rd dimension: max(ntau).
;	-resultper: Variable containing the selected Stokes profiles in the 
;	       selected files. Output. 3-D Float array. Size of the 1st 
;	       dimension: nper. Size of the 2nd dimension: total number 
;	       of Stokes profiles selected (internally calculated). Size 
;	       of the 3rd dimension: max(nlam).
;	-ntau: Variable containing the number of points of each model 
;	       atmosphere selected. Input. Float array.
;	-nlam: Variable containing the number of points of each Stokes 
;	       profile selected. Input. Float array.

;Common blocks:
;	-FICHEROS:
;		-listado1: Listing of the files available to TAUMENU.PRO 
;			   in the current directory. String array.
;		-listado2: Listing of the files selected by TAUMENU.PRO 
;			   in the current directory. String array.
;		-directorio: Current directory. Scalar string.
;	-VARIABLES: See comments to ESCALABLE.PRO above.
;	-ETIQUETAS:
;		-etiquetax: X-label for physical quantities plotting. 
;			   Scalar string.
;		-etiquetay: Y-labels for physical quantities plotting. 
;			   String array of 7 elements.
;		-etiquestox: X-label for Stokes profiles plotting. 
;			   Scalar string.
;		-etiquestoy: Y-labels for Stokes profiles plotting. 
;			   String array of 4 elements.
;	-ESCALAS: See comments to ESCALABLE.PRO above.

;Routines called: ESCALABLE.

;***************************************************************************
;***************************************************************************
pro rmodprof,nm,nmod,nper,resultmod,resultper,resulterr,ntau,nlam

common ficheros,listado1,listado2,directorio
common variables,imag,istokes,win,ips,itau,dm,dp
common etiquetas,etiquetax,etiquetay,etiquestox,etiquestoy
common escalas,maxmod,minmod,maxper,minper,iscale
common todoslosparametros,tlpmod
;___________________________________________________________________________
;				       Distinguimos entre modelos y perfiles
;___________________________________________________________________________

lis=listado2
nel=n_elements(lis)
imod=bytarr(nel)
for i=0,nel-1 do begin
   barpos=strpos(lis(i),'\')
   if barpos eq -1 then begin
      dotpos=strpos(lis(i),'.')
      caracter=strmid(lis(i),dotpos+1,1)
      caracter=caracter(0)
      case caracter of
         'm': imod(i)=0
	 'p': imod(i)=1
      endcase
   endif else imod(i)=2
endfor
warnimod=where(imod eq 2)

im=imag
is=istokes
nm=fix(total(im))
ns=fix(total(is))

dm=where((imod eq 0) or (imod eq 2))
dp=where((imod eq 1) or (imod eq 2))
if n_elements(warnimod) ne nel then begin
   if ((nm ne 0) and (ns ne 0)) then begin
      texto='Warning: Color codes may differ between quantities and profiles'
      mensaje,texto
   endif 
endif

nmod=0
nper=0
if dm(0) ne -1 then begin
   nmod=n_elements(dm)
   lismod=strarr(nmod)
   lismod=lis(dm)
   liserr=lismod
   for i=0,nmod-1 do begin
      barpos=strpos(lismod(i),'\')
      if barpos ne -1 then lismod(i)=strmid(lismod(i),0,barpos)
      dotpos=strpos(lismod(i),'.')
      liserr(i)=strmid(lismod(i),0,dotpos)+'.err'
      lismod(i)=directorio+'/'+lismod(i)
      liserr(i)=directorio+'/'+liserr(i)
   endfor
endif else lismod=''
if dp(0) ne -1 then begin
   nper=n_elements(dp)
   lisper=strarr(nper)
   lisper=lis(dp)
   for i=0,nper-1 do begin
      barpos=strpos(lisper(i),'\')
      dotpos=strpos(lisper(i),'.')
      if barpos ne -1 then begin
         lisper(i)=strmid(lisper(i),0,dotpos)+'.per'
      endif
      lisper(i)=directorio+'/'+lisper(i)
   endfor
endif else lisper=''
 
;___________________________________________________________________________
;							  Leemos los modelos
;___________________________________________________________________________



if nmod ne 0 then begin
   ntau=intarr(nmod)
   ncolumnas=intarr(nmod)
if ((nm ne 0) and (lismod(0) ne '')) then begin
;for i=1,nmod do begin
;   spawn,'if(-e tamano) rm tamano'
;   spawn,'wc '+lismod(i-1)+' > tamano'
;   nlineas=0
;   npalabras=0
;   ncifras=0 
;   nombre=''
;   openr,1,'tamano'
;   readf,1,nlineas,npalabras,ncifras,nombre
;   close,1
;   ntau(i-1)=nlineas-1
;   ncolumnas(i-1)=(npalabras-3)/(nlineas-1)
;;   openr,i,lismod(i-1)
;;   readf,i,vmac,fill,stray
;;         while not eof(i) do begin
;;         readf,i,tau,t,p,vmic,b,v,g,f,pg,ro,z
;;         ntau(i-1)=ntau(i-1)+1
;;      endwhile
;;   close,i
;endfor

; checking the model size
for i=1,nmod do begin
    openr,i,lismod(i-1)
    readf,i,vmac
    readf,i,tau1
    readf,i,tau1
    ntau(i-1)=2
    while not eof(i) do begin
       readf,i,tau
       ntau(i-1)=ntau(i-1)+1
    endwhile
    close,i
    openr,i,lismod(i-1)
    readf,i,vmac
    readf,i,tau2,t,p,vmic,b,v,g,f,pg,ro,z
    readf,i,tau2
    close,i
    ncolumnas(i-1)=8
    if(tau2 eq tau1)then ncolumnas(i-1)=11
endfor


resultmod=fltarr(nmod,nm+1,max(ntau))
resulterr=resultmod
tlpmod=fltarr(nmod,11,max(ntau))


for i=1,nmod do begin
   resulti=fltarr(11,ntau(i-1))
   resultii=fltarr(ncolumnas(i-1),ntau(i-1))
   resultierr=resulti

   openr,i,lismod(i-1)
   readf,i,vmac,fill
   readf,i,resultii
   resultii(1,*)=resultii(1,*)*1.e-3
   resultii(3,*)=resultii(3,*)*1.e-5
   resultii(4,*)=resultii(4,*)*1.e-3
   resultii(5,*)=resultii(5,*)*1.e-5
   resulti(0:ncolumnas(i-1)-1,0:ntau(i-1)-1)= $
          resultii(0:ncolumnas(i-1)-1,0:ntau(i-1)-1)
   close,i

   insulti=resulti([0,where(im eq 1)+1],*)
   resultmod(i-1,indgen(nm+1),0:ntau(i-1)-1)=insulti
   tlpmod(i-1,*,0:ntau(i-1)-1)= resulti(*,0:ntau(i-1)-1)

   openr,i,liserr(i-1),error=error1
   if error1 eq 0 then begin
      readf,i,vmacerr,fillerr
      readf,i,resultierr
      resultierr(1,*)=resultierr(1,*)*1.e-3
      resultierr(3,*)=resultierr(3,*)*1.e-5
      resultierr(4,*)=resultierr(4,*)*1.e-3
      resultierr(5,*)=resultierr(5,*)*1.e-5
    endif
      
   close,i

   insultierr=resultierr([0,where(im eq 1)+1],*)
   resulterr(i-1,indgen(nm+1),0:ntau(i-1)-1)=insultierr
   close,i

endfor
endif else begin
   if ((nm ne 0) and (lismod(0) eq '')) then begin
      texto='No model files in current directory'
      mensaje,texto
   endif
endelse
endif


;___________________________________________________________________________
;							 Leemos los perfiles
;___________________________________________________________________________

if nper ne 0 then begin
   nlam=intarr(nper)
if ((ns ne 0) and (lisper(0) ne '')) then begin
for i=1,nper do begin
   openr,i,lisper(i-1)
   while not eof(i) do begin
      readf,i,ind,lam,ii,qq,uu,vv
      nlam(i-1)=nlam(i-1)+1
   endwhile
   close,i
endfor

resultper=fltarr(nper,ns,max(nlam))

for i=1,nper do begin
   resulti=fltarr(6,nlam(i-1))
   openr,i,lisper(i-1)
   readf,i,resulti
   insulti=resulti(where(is eq 1)+2,*)
   close,i
   resultper(i-1,indgen(ns),0:nlam(i-1)-1)=insulti
endfor
endif else begin
   if ((ns ne 0) and (lisper(0) eq '')) then begin
      texto='No profile files in current directory'
      mensaje,texto
   endif
endelse
endif

escalable,nmod,nper,resultmod,resultper,ntau,nlam

end

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;***************************************************************************
;***************************************************************************

;				PROCEDURE DIBUPAN

;Purpose: Plots in a window widget the selected physical quantities and/or 
;Stokes profiles from the selected models by TAUMENU.PRO.

;Arguments:
;	-nm:   Number of physical quantities selected. Input. Scalar integer.
;	-nmod: Number of model files selected. Input. Scalar integer.
;	-nstokes: Total number of Stokes profiles selected. Input. Scalar
;	       integer.
;	-icolor: Variable containing the color numbers for window plotting.
;	         Input. Integer array of 5 elements (adapted for color table
;		 number 2).

;Common blocks:
;	-VENTANAS:
;		-lista1: Widget_text listing the files available.
;		-lista2: Widget_text listing the files selected.
;		-menu:   Widget_base containing everything except for the 
;		         the 'Working directory' widget_text.
;		-direc:  Editable widget_text containing the current directory.
;		-supermenu: Parent widget_base.
;		-pantalla: Widget_base for display.
;		-pintor: Widget_draw for display.
;	-FICHEROS: See comments to RMODPROF.PRO above.
;	-VARIABLES: See comments to ESCALABLE.PRO above.
;	-ETIQUETAS: See comments to RMODPROF.PRO above.
;	-ESCALAS: See comments to ESCALABLE.PRO above.

;Routines called: RMODPROF.

;***************************************************************************
;***************************************************************************
pro dibupan,nm,nmod,nstokes,icolor

common ventanas,lista1,lista2,menu,direc,supermenu,pantalla,pintor
common ficheros,listado1,listado2,directorio
common variables,imag,istokes,win,ips,itau,dm,dp
common errores,ierr
common etiquetas,etiquetax,etiquetay,etiquestox,etiquestoy
common escalas,maxmod,minmod,maxper,minper,iscale
common todoslosparametros,tlpmod

ncolor=fix(n_elements(icolor))
nplots=nm+nstokes

if nplots ne 0 then begin						;(0)
   rmodprof,nm,nmod,nper,modelres,profileres,errores,ntau,nlam
   case nplots of
      1: tamchar=1.8
      2: tamchar=1.8
      3: tamchar=2.8
      4: tamchar=1.2
      5: tamchar=2
      6: tamchar=2
      7: tamchar=2
      8: tamchar=2
      9: tamchar=2
     10: tamchar=2
     11: tamchar=2
     12: tamchar=2
     13: tamchar=2
     14: tamchar=2
     15: tamchar=2
     16: tamchar=2
   endcase
   case nplots of
      1: !p.multi=[0,0,1,0,0]
      2: !p.multi=[0,0,2,0,0]
      3: !p.multi=[0,0,3,0,0]
      4: !p.multi=[0,2,2,0,0]
      5: !p.multi=[0,2,3,0,0]
      6: !p.multi=[0,2,3,0,0]
      7: !p.multi=[0,3,3,0,0]
      8: !p.multi=[0,3,3,0,0]
      9: !p.multi=[0,3,3,0,0]
     10: !p.multi=[0,3,4,0,0]
     11: !p.multi=[0,3,4,0,0] 
     12: !p.multi=[0,3,4,0,0]
     13: !p.multi=[0,4,4,0,0]
     14: !p.multi=[0,4,4,0,0] 
     15: !p.multi=[0,4,4,0,0]
     16: !p.multi=[0,4,4,0,0]
   endcase
   
   widget_control,pantalla,get_value=win
   if win eq -1 then begin						;(3)
      widget_control,pintor,/realize
   endif							       ;(-3)
   widget_control,pantalla,get_value=win
   wset,win


if nm ne 0 then begin
   dondemag=where(imag eq 1)
   if dm(0) ne -1 then begin

      if iscale(0) eq 0 then begin
         margen=.1*(maxmod(0)-minmod(0))
         if margen eq 0 then margen=1
         maxx=maxmod(0)+margen
         minx=minmod(0)-margen
      endif else begin
         maxx=maxmod(0)
         minx=minmod(0)
      endelse
   for i=0,nm-1 do begin

      if iscale(i+1) eq 0 then begin
         margen=.1*(maxmod(i+1)-minmod(i+1))
         maxy=maxmod(i+1)+margen
         miny=minmod(i+1)-margen
         if (dondemag(i) eq 1 or dondemag(i) eq 8 or  $
            dondemag(i) eq 9 ) then begin
            maxy=1.3*maxmod(i+1)
            miny=.7*minmod(i+1)
         endif
         if maxy eq miny then begin
            maxy=maxy+.1
            miny=miny-.1
         endif
      endif else begin
         maxy=maxmod(i+1)
         miny=minmod(i+1)
      endelse

      xx=modelres(0,0,0:ntau(0)-1)
      if(itau eq 0)then xx=tlpmod(0,8,0:ntau(0)-1)

      if (dondemag(i) eq 1 or dondemag(i) eq 8 or dondemag(i) eq 9) then begin
         magio=modelres(0,i+1,0:ntau(0)-1)
         nneg=where(magio le 0,nn) & if(nn gt 0)then magio(nneg)=1.e-20
         plot_io,xx,magio,color=col2(icolor(0)),charsize=tamchar,xtitle=etiquetax,ytitle=$
         etiquetay(dondemag(i)),xstyle=1,xrange=[minx,maxx] $
         ,ystyle=1,yrange=[miny,maxy]
      endif else begin

         plot,xx,modelres(0,i+1,0:ntau(0)-1),$
         color=col2(icolor(0)),charsize=tamchar,xtitle=etiquetax,ytitle=$
         etiquetay(dondemag(i)),ystyle=1,yrange=[miny,maxy],xstyle=$
         1,xrange=[minx,maxx]
      endelse

      pintaerrores=max(abs(errores(0,i+1,0:ntau(0)-1)))


      if pintaerrores ne 0. and ierr eq 0 then errcolor,xx,modelres(0,i+1,0:ntau(0)-1)$
      ,errores(0,i+1,0:ntau(0)-1),icolor(0),miny,maxy
      

      for j=1,nmod-1 do begin
         nvueltas=fix(j)/ncolor
         colopart=icolor(j-nvueltas*(ncolor))
         case nvueltas of
            0: linepart=0
            1: linepart=2
            2: linepart=3
            3: linepart=4
            4: linepart=5
            5: linepart=6
            6: linepart=7
         endcase
         xx=modelres(j,0,0:ntau(0)-1)
         if(itau eq 0)then xx=tlpmod(j,8,0:ntau(0)-1)
         oplot,xx,modelres(j,i+1,0:ntau(j)-1)$
         ,color=col2(colopart),line=linepart

         pintaerrores=max(abs(errores(j,i+1,0:ntau(j)-1)))

         if pintaerrores ne 0. and ierr eq 0 then errcolor,xx,$
         modelres(j,i+1,0:ntau(j)-1),errores(j,i+1,0:ntau(j)-1),colopart,miny,maxy
      endfor
   endfor

   endif
endif

if nstokes ne 0 then begin
   dondesto=where(istokes eq 1)
   if dp(0) ne -1 then begin

   for i=0,nstokes-1 do begin
     
      if iscale(nm+1+i) eq 0 then begin
         if (maxper(i) ge 0) then begin
            maxy=1.1*maxper(i)
         endif else maxy=.8*maxper(i)
         if (minper(i) ge 0) then begin
            miny=.8*minper(i)
         endif else miny=1.1*minper(i)
         if maxy eq miny then begin
            maxy=maxy+.1
            miny=miny-.1
         endif
      endif else begin
         maxy=maxper(i)
         miny=minper(i)
      endelse

      plot,profileres(0,i,0:nlam(0)-1),color=col2(icolor(0)),charsize=$
      tamchar,xtitle=etiquestox,ytitle=etiquestoy(dondesto(i)),$
      ystyle=1,yrange=[miny,maxy]

      for j=1,nper-1 do begin
         nvueltas=fix(j)/ncolor
         colopart=icolor(j-nvueltas*(ncolor))
         case nvueltas of
            0: linepart=0
            1: linepart=2
            2: linepart=3
            3: linepart=4
            4: linepart=5
            5: linepart=6
            6: linepart=7
         endcase
         oplot,profileres(j,i,0:nlam(j)-1),color=col2(colopart),line=linepart
      endfor
   endfor

   endif
endif

endif else begin						     ;(-0,4)
   texto='Please, select some Stokes parameter or physical quantity to be displayed'
   mensaje,texto
endelse								       ;(-4)

!p.multi=0

end

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;***************************************************************************
;***************************************************************************

;				PROCEDURE DIBUPS

;Purpose: Plots in a ps-file the selected physical quantities and/or 
;Stokes profiles from the selected models by TAUMENU.PRO. Similar to 
;DIBUPAN.PRO but changing the plotting device.

;Arguments:
;	-nm:   Number of physical quantities selected. Input. Scalar integer.
;	-nmod: Number of model files selected. Input. Scalar integer.
;	-nstokes: Total number of Stokes profiles selected. Input. Scalar
;	       integer.
;	-icolor: Variable containing the color numbers for window plotting.
;	         Input. Integer array of 5 elements (adapted for color table
;		 number 2). (March, 18, 1994) By now, a bug is found in the 
;		 device procedure when the 'set_plot,"ps"' is active and the 
;		 keyword 'COLOR' is in effect. So, this argument makes no 
;		 sense in the current status of the procedure. It is kept by 
;		 analogy with DIBUPAN. Colors are right now substituted by 
;		 line styles.


;Common blocks:
;	-VENTANAS: See DIBUPAN.PRO above
;	-FICHEROS: See comments to RMODPROF.PRO above.
;	-VARIABLES: See comments to ESCALABLE.PRO above.
;	-ETIQUETAS: See comments to RMODPROF.PRO above.
;	-FICHPS:
;		-nameps: Name of the ps-file, as selected through 
;			 TEXTOEDITABLE.PRO
;	-ESCALAS: See comments to ESCALABLE.PRO above.

;Routines called: RMODPROF.

;***************************************************************************
;***************************************************************************
pro dibups,nm,nmod,nstokes,icolor

common ventanas,lista1,lista2,menu,direc,supermenu,pantalla,pintor
common ficheros,listado1,listado2,directorio
common variables,imag,istokes,win,ips,itau,dm,dp
common etiquetas,etiquetax,etiquetay,etiquestox,etiquestoy
common fichps,nameps,encaps
common escalas,maxmod,minmod,maxper,minper,iscale
common todoslosparametros,tlpmod

nstokes=fix(total(istokes))
nplots=nm+nstokes

if nplots ne 0 then begin						;(0)
   rmodprof,nm,nmod,nper,modelres,profileres,errores,ntau,nlam
   case nplots of
      1: tamchar=1.8
      2: tamchar=1.6
      3: tamchar=2.8
      4: tamchar=1.
      5: tamchar=2
      6: tamchar=2
      7: tamchar=1.4
      8: tamchar=2
      9: tamchar=2
     10: tamchar=2
     11: tamchar=2
     12: tamchar=2
     13: tamchar=2
     14: tamchar=2
     15: tamchar=2
     16: tamchar=2
   endcase
   case nplots of
      1: !p.multi=[0,0,1,0,0]
      2: !p.multi=[0,0,2,0,0]
      3: !p.multi=[0,0,3,0,0]
      4: !p.multi=[0,2,2,0,0]
      5: !p.multi=[0,2,3,0,0]
      6: !p.multi=[0,2,3,0,0]
      7: !p.multi=[0,3,3,0,0]
      8: !p.multi=[0,3,3,0,0]
      9: !p.multi=[0,3,3,0,0]
     10: !p.multi=[0,3,4,0,0]
     11: !p.multi=[0,3,4,0,0]
     12: !p.multi=[0,3,4,0,0]
     13: !p.multi=[0,4,4,0,0]
     14: !p.multi=[0,4,4,0,0] 
     15: !p.multi=[0,4,4,0,0]
     16: !p.multi=[0,4,4,0,0]
   endcase
   
   set_plot,'ps'

if encaps eq 1 then begin   ;(CW)
   device,filename=directorio(0)+'/'+nameps+'.eps',ysize=24,xsize=18,$
                   /encapsulated
   nlineas=8	;Tipos de linea distintos para el dibujo
endif else begin
   device,filename=directorio(0)+'/'+nameps+'.ps',ysize=24,xsize=18,$
                   xoffset=1.0,yoffset=2.5
   nlineas=8	;Tipos de linea distintos para el dibujo
endelse

if nm ne 0 then begin
   dondemag=where(imag eq 1)
   if dm(0) ne -1 then begin
      if iscale(0) eq 0 then begin
         margen=.1*(maxmod(0)-minmod(0))
         if margen eq 0 then margen=1 
         maxx=maxmod(0)+margen
         minx=minmod(0)-margen
      endif else begin
         maxx=maxmod(0)
         minx=minmod(0)
      endelse
   for i=0,nm-1 do begin

;      if iscale(i+1) eq 0 then begin
;         if maxmod(i+1) ge 0 then begin
;            maxy=1.1*maxmod(i+1)
;         endif else maxy=.8*maxmod(i+1)
;         if (minmod(i+1) ge 0) then begin
;            miny=.8*minmod(i+1)
;         endif else miny=1.1*minmod(i+1)
;         if maxy eq miny then begin
;            maxy=maxy+.1
;            miny=miny-.1
;         endif
;      endif else begin
;         maxy=maxmod(i+1)
;         miny=minmod(i+1)
;      endelse


      if iscale(i+1) eq 0 then begin            ;como arriba!! (CW)
         margen=.1*(maxmod(i+1)-minmod(i+1))    ;  
         maxy=maxmod(i+1)+margen
         miny=minmod(i+1)-margen
         if (dondemag(i) eq 1 or dondemag(i) eq 8 or  $
            dondemag(i) eq 9 ) then begin
            maxy=1.3*maxmod(i+1)
            miny=.7*minmod(i+1)
         endif
         if maxy eq miny then begin
            maxy=maxy+.1
            miny=miny-.1
         endif
      endif else begin
         maxy=maxmod(i+1)
         miny=minmod(i+1)
      endelse

      xx=modelres(0,0,0:ntau(0)-1)
      if(itau eq 0)then xx=tlpmod(0,8,0:ntau(0)-1)


      if (dondemag(i) eq 1 or dondemag(i) eq 8 or dondemag(i) eq 9 ) then begin
         magio=modelres(0,i+1,0:ntau(0)-1)
         nneg=where(magio le 0,nn) & if(nn gt 0)then magio(nneg)=1.e-20
         plot_io,xx,magio* $
         1.,charsize=tamchar,xtitle=etiquetax,ytitle=etiquetay(dondemag(i)),$
         xstyle=1,xrange=[minx,maxx]$
        ,ystyle=1,yrange=[miny,maxy]

      endif else begin
         plot,xx,modelres(0,i+1,0:ntau(0)-1) $
         ,charsize=tamchar,xtitle=etiquetax,ytitle=etiquetay(dondemag(i)),$
         ystyle=1,yrange=[miny,maxy],xstyle=1,xrange=[minx,maxx]
      endelse

      for j=1,nmod-1 do begin
         nvueltas=fix(j)/nlineas
         linepart=j-nvueltas*nlineas
         xx=modelres(j,0,0:ntau(0)-1)
         if(itau eq 0)then xx=tlpmod(j,8,0:ntau(0)-1)
         if dondemag(i) eq 1 then begin 
            oplot,xx,modelres(j,i+1,0:ntau(j)-1)$
            *1.,line=linepart
         endif else begin
            oplot,xx,modelres(j,i+1,0:ntau(j)-1)$
            ,line=linepart
	 endelse
      endfor
   endfor
   
   endif
endif

if nstokes ne 0 then begin
   dondesto=where(istokes eq 1)
   if dp(0) ne -1 then begin

   for i=0,nstokes-1 do begin
     
      if iscale(nm+1+i) eq 0 then begin
         if (maxper(i) ge 0) then begin
            maxy=1.1*maxper(i)
         endif else maxy=.8*maxper(i)
         if (minper(i) ge 0) then begin
            miny=.8*minper(i)
         endif else miny=1.1*minper(i)
         if maxy eq miny then begin
            maxy=maxy+.1
            miny=miny-.1
         endif
      endif else begin
         maxy=maxper(i)
         miny=minper(i)
      endelse

      plot,profileres(0,i,0:nlam(0)-1) $
      ,charsize=tamchar,xtitle=etiquestox,ytitle=etiquestoy(dondesto(i)),$
      ystyle=1,yrange=[miny,maxy]

      for j=1,nper-1 do begin
         nvueltas=fix(j)/nlineas
         linepart=j-nvueltas*nlineas
         oplot,profileres(j,i,0:nlam(j)-1),line=linepart
      endfor
   endfor

   endif
endif
device,/close
endif else begin						     ;(-0,4)
   texto='Please, select some Stokes parameter or physical quantity to be displayed'
   mensaje,texto
endelse								       ;(-4)

ips=0
set_plot,'x'
!p.multi=0

end

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;***************************************************************************
;***************************************************************************

;			   PROCEDURE TAUMENU_EVENT

;Purpose: Event-handler of the widgets created by TAUMENU.PRO.

;Argument:
;	-suc: Event

;Common blocks:
;	-VENTANAS: See DIBUPAN.PRO above
;	-FICHEROS: See comments to RMODPROF.PRO above.
;	-VARIABLES: See comments to ESCALABLE.PRO above.
;	-ETIQUETAS: See comments to RMODPROF.PRO above.
;	-FICHPS: See comments to DIBUPS.PRO above.
;	-ESCALAS: See comments to ESCALABLE.PRO above.

;Routines called: REORDENA
;		  MENSAJE
;		  TEXTOEDITABLE
;		  DIBUPAN
;		  DIBUPS

;***************************************************************************
pro taumenu_event,suc

common ventanas,lista1,lista2,menu,direc,supermenu,pantalla,pintor
common ficheros,listado1,listado2,directorio
common variables,imag,istokes,win,ips,itau,dm,dp
common errores,ierr
common etiquetas,etiquetax,etiquetay,etiquestox,etiquestoy
common fichps,nameps,encaps
common escalas,maxmod,minmod,maxper,minper,iscale
common ventcw,av,st,ib,qb,ub,vb,pp    ;(CW)
common variacw,allstokes,allmag       ;
common oldcw, rold, gold, bold        ;
;---------------------------------------------------------------------------
;                                  ********Identificacion de sucesos********
;---------------------------------------------------------------------------

widget_control,suc.id,get_uvalue=m

;---------------------------------------------------------------------------
if (m eq 1 or m eq 2) then begin    ;(0) *******Seleccion de modelos********
;---------------------------------------------------------------------------

if (m eq 1) then begin							;(1)
   tam=size(listado2)

   if (tam(1) eq 1 and listado2(0) eq '') then begin			;(2)
      listado2(0)=listado1(suc.index)
   endif else begin						     ;(-2,3)
      donde=where(listado2 eq listado1(suc.index))
      if (donde(0) eq -1) then begin				        ;(4)
         listado2=[listado2,listado1(suc.index)]
      endif else begin						     ;(-4,5)
         tam=size(listado2)
         if tam(3) eq 1 then begin					;(6)
            listado2=strarr(1)
         endif else begin					     ;(-6,7)
            if (donde(0) ne tam(3)-1) then $
            listado2(donde(0):tam(3)-2)=listado2(donde(0)+1:tam(3)-1)
            listado2=listado2(0:tam(3)-2)
         endelse						       ;(-7)
      endelse							       ;(-5)
   endelse							       ;(-3)
endif else begin						     ;(-1,8)
   tam=size(listado2)
   if (tam(3) eq 1) then begin						;(9)
      listado2=strarr(1)
   endif else begin						    ;(-9,10)
      if (suc.index ne tam(3)-1) then $
      listado2(suc.index:tam(3)-2)=listado2(suc.index+1:tam(3)-1)
      listado2=listado2(0:tam(3)-2)
   endelse							      ;(-10)
endelse								       ;(-9)
widget_control,lista2,set_value=listado2

endif else begin						    ;(-0,11)

widget_control,suc.id,get_value=mag

;---------------------------------------------------------------------------
;                                     *******Seleccion de magnitudes********
;---------------------------------------------------------------------------

if (m eq 3) then begin						       ;(12)
   case imag(0) of
      0: if mag eq 'Temperature' then imag(0)=1
      1: if mag eq 'Temperature' then imag(0)=0
   endcase
   case imag(1) of
       0: if mag eq 'Electronic pressure' then imag(1)=1
       1: if mag eq 'Electronic pressure' then imag(1)=0
   endcase
   case imag(2) of
      0: if mag eq 'Microturbulence' then imag(2)=1
      1: if mag eq 'Microturbulence' then imag(2)=0
   endcase
   case imag(3) of
      0: if mag eq 'Magnetic field strength' then imag(3)=1
      1: if mag eq 'Magnetic field strength' then imag(3)=0
   endcase
   case imag(4) of
      0: if mag eq 'Line of sight velocity' then imag(4)=1
      1: if mag eq 'Line of sight velocity' then imag(4)=0
   endcase
   case imag(5) of
      0: if mag eq 'Magnetic inclination' then imag(5)=1
      1: if mag eq 'Magnetic inclination' then imag(5)=0
   endcase
   case imag(6) of
      0: if mag eq 'Magnetic azimuth' then imag(6)=1
      1: if mag eq 'Magnetic azimuth' then imag(6)=0
   endcase
   case imag(7) of
      0: if mag eq 'Geometrical height' then imag(7)=1
      1: if mag eq 'Geometrical height' then imag(7)=0
   endcase
   case imag(8) of
      0: if mag eq 'Gas pressure' then imag(8)=1
      1: if mag eq 'Gas pressure' then imag(8)=0
   endcase
   case imag(9) of
      0: if mag eq 'Density' then imag(9)=1
      1: if mag eq 'Density' then imag(9)=0
   endcase


;---------------------------------------------------------------------------
endif                                ;*******Seleccion de errores****
;---------------------------------------------------------------------------

if (m eq 33) then begin
   case ierr of
      0: if mag eq ' NO error bars' $               ;(LB)
         then ierr=1
      1: if mag eq ' NO error bars' $                ;(LB)
         then ierr=0
   endcase

;---------------------------------------------------------------------------
endif                                ;*******Seleccion de magnitudes*(CW)***
;---------------------------------------------------------------------------

if (m eq 11) then begin
   case allmag  of                            ;(CW)
      0: if mag eq ' ALL' then allmag=1        ;
      1: if mag eq ' ALL' then allmag=0        ;
   endcase
   
;---------------------------------------------------------------------------
endif                         ;(-12) ***Seleccion de parametros de Stokes***
;---------------------------------------------------------------------------

if (m eq 4) then begin						       ;(13)
   case istokes (0) of
      0: if mag eq '  I  ' then istokes(0)=1
      1: if mag eq '  I  ' then istokes(0)=0
   endcase
   case istokes (1) of
      0: if mag eq '  Q  ' then istokes(1)=1
      1: if mag eq '  Q  ' then istokes(1)=0
   endcase
   case istokes (2) of
      0: if mag eq '  U  ' then istokes(2)=1
      1: if mag eq '  U  ' then istokes(2)=0
   endcase
   case istokes (3) of
      0: if mag eq '  V  ' then istokes(3)=1
      1: if mag eq '  V  ' then istokes(3)=0
   endcase
   case allstokes  of                              ;(CW)
      0: if mag eq ' ALL ' then allstokes=1        ;
      1: if mag eq ' ALL ' then allstokes=0        ;
   endcase

;---------------------------------------------------------------------------
endif                         ;(-13) *******Seleccion del directorio********
;---------------------------------------------------------------------------

if (m eq 0) then begin						       ;(14)
   directorio=strtrim(mag,2)
   widget_control,direc,set_value=directorio
;   if itau eq 1 then begin                                    ;(LB)
      dirlis1='ls '+strtrim(directorio,2)+'/*.mod'
;   endif else dirlis1='ls '+strtrim(directorio,2)+'/*.moz'   ;(LB)
   dirlis2='ls '+strtrim(directorio,2)+'/*.per'
   spawn,dirlis1,listado11,/SH   ; (CW) Bug al cambiar de dir
   spawn,dirlis2,listado12,/SH   ; 
   listado2=''				
   if ((listado11(0) eq '') and (listado12(0) eq '')) then begin    ;(21)
      texto='Neither model in tau nor profile files in current directory'
      mensaje,texto
   endif else listado1=reordena(listado11,listado12)
   nl1=n_elements(listado1)
   if listado1(0) ne '' then begin				       ;(15)
      ande=strpos(listado1(0),'/')
      while ande ne -1 do begin
         ande=ande+1
         for jj=0,nl1-1 do listado1(jj)=strmid(listado1(jj),ande,300)
         ande=strpos(listado1(0),'/')
      endwhile
;      if nl1 eq 1 then begin					       ;(16)
;         listado1=strarr(1)
;      endif 							      ;(-16)
   endif else begin						   ;(-15,17)
      texto='Neither model nor profile files in current directory'
      mensaje,texto
   endelse							      ;(-17)
   widget_control,lista1,set_value=listado1
   listado2=strarr(1)
   widget_control,lista2,set_value=listado2
   mag='Reset'
endif								      ;(-14)

;---------------------------------------------------------------------------
                                     ; ********Seleccion de acciones********
;---------------------------------------------------------------------------

if mag eq 'Reset' then begin					       ;(18)
   listado2=strarr(1)
   widget_control,lista2,set_value=listado2
   imag=bytarr(10)                            ;(CW)
   istokes=bytarr(4)                         ; 
   widget_control,supermenu,SET_BUTTON=0     ;
   allstokes=0                               ;(CW) Bug de ALL puesto y el
   allmag=0                                  ;     resto quitado
endif								      ;(-18)

;---------------------------------------------------------------------------
                                     ; ********Seleccion de acciones (CW)***
;---------------------------------------------------------------------------

if mag eq ' ALL ' then begin
   if allstokes then begin
      istokes(*)=byte(1)
      widget_control,ib,SET_BUTTON=1    ;(CW)   
      widget_control,qb,SET_BUTTON=1    ;(CW)
      widget_control,ub,SET_BUTTON=1    ;(CW)
      widget_control,vb,SET_BUTTON=1    ;(CW)
   endif else begin
      istokes=bytarr(4)
      widget_control,ib,SET_BUTTON=0    ;(CW)   
      widget_control,qb,SET_BUTTON=0    ;(CW)
      widget_control,ub,SET_BUTTON=0    ;(CW)
      widget_control,vb,SET_BUTTON=0    ;(CW)
   endelse
endif									

;---------------------------------------------------------------------------
                                     ; ********Seleccion de acciones (CW)***
;---------------------------------------------------------------------------

if mag eq ' ALL' then begin
   if allmag then begin
      imag(*)=byte(1)
      widget_control,pp,SET_BUTTON=1    ;(CW)
   endif else begin
      imag(*)=bytarr(10)
      widget_control,pp,SET_BUTTON=0    ;(CW)
   endelse
endif

;---------------------------------------------------------------------------
                                     ; ********Seleccion de variable********
;---------------------------------------------------------------------------

if m eq 9 then begin						       ;(19)
   if mag eq '  t  ' then begin					       ;(20)
      itau=1
      etiquetax='!6log (!7s!6)'
;      dirlis1='ls '+strtrim(directorio,2)+'/*.mod'  ;no considero nuevos ficheros
;      dirlis2='ls '+strtrim(directorio,2)+'/*.per'
;      spawn,dirlis1,listado11,/SH  ; (CW) Bug al cambiar de dir
;      spawn,dirlis2,listado12,/SH  ;
;      listado2=''				
;      if ((listado11(0) eq '') and (listado12(0) eq '')) then begin    ;(21)
;         texto='Neither model in tau nor profile files in current directory'
;         mensaje,texto
;      endif else listado1=reordena(listado11,listado12)
   endif							      ;(-20)
   if mag eq '  z  ' then begin					       ;(22)
      itau=0
      etiquetax='!6z (km)'
;      dirlis1='ls '+strtrim(directorio,2)+'/*.moz'  ;no considero nuevos ficheros
;      dirlis2='ls '+strtrim(directorio,2)+'/*.per'
;      spawn,dirlis1,listado11,/SH  ; (CW) Bug al cambiar de dir
;      spawn,dirlis2,listado12,/SH  ;
;      listado2=''				
;      if ((listado11(0) eq '') and (listado12(0) eq '')) then begin    ;(23)
;         texto='Neither model in z nor profile files in current directory'
;         mensaje,texto
;     endif else listado1=reordena(listado11,listado12)
   endif							      ;(-22)

;;   if listado1(0) ne '' then begin      			       ;(24)
;;      listado1=listado1(sort(listado1))
;;      nl1=n_elements(listado1)
;;      ande=strpos(listado1(0),'/')
;;      while ande ne -1 do begin
;;         ande=ande+1
;;         for jj=0,nl1-1 do listado1(jj)=strmid(listado1(jj),ande,100)
;;         ande=strpos(listado1(0),'/')
;;      endwhile
;;      listado2=strarr(1)
;;   endif							      ;(-24)
;;   widget_control,lista1,set_value=listado1
;;   widget_control,lista2,set_value=listado2
endif								      ;(-19)

;---------------------------------------------------------------------------
                                                 ; ********Impresion********
;---------------------------------------------------------------------------

if mag eq 'Print' then begin					       ;(25)
   ips=1
   textoeditable
endif								      ;(-25) 

;---------------------------------------------------------------------------
                                                   ; ********Escalas********
;---------------------------------------------------------------------------

if mag eq 'Scale' then begin					       ;(25)
      
   spawn,'emacs ~/bin/scales'

endif								      ;(-25) 

;---------------------------------------------------------------------------
                                                    ; ********Dibujo********
;---------------------------------------------------------------------------

if mag eq 'Plot' then begin					       ;(26)
   nmod=n_elements(listado2)
   if (listado2(0) ne '') then begin				       ;(27)
      icolor=[1,2,3,4,5,6,7,8,9,10];  negro (=0),blanco(1) ,rojo(2)
      				   ;  azul (3)  ,verde (4) ,amarillo(5)
				   ;  cian(6)   ,malva(7) ,naranja(8)
				   ;  verdeoscuro(9) ,malva oscuro (10)
;     icolor=[130050,140000,16666630,60000,70000,16666630,7,8,9,10]
      nm=fix(total(imag))
      nstokes=fix(total(istokes))
      if ips eq 0 then begin
         dibupan,nm,nmod,nstokes,icolor
      endif else begin
         dibups,nm,nmod,nstokes,icolor
      endelse
   endif else begin						   ;(-27,28)
      texto='Please, select some model or Stokes parameter to work with'
      mensaje,texto
   endelse							      ;(-28)
endif								      ;(-26)


;---------------------------------------------------------------------------
                                                    ; ********Salida********
;---------------------------------------------------------------------------

if mag eq 'Quit' then begin					       ;(29)
   ; loadct,0                 ; que lo deje como lo encontro! (CW)
   tvlct, rold, gold, bold    ; 
   widget_control,pintor,/destroy
   widget_control,supermenu,/destroy
endif								      ;(-29)

endelse								      ;(-11)
end


;***************************************************************************
;***************************************************************************

;			   	PROCEDURE TAUMENU

;Purpose: Widget creator.

;Arguments: None

;Common blocks:
;	-VENTANAS: See DIBUPAN.PRO above
;	-FICHEROS: See comments to RMODPROF.PRO above.
;	-VARIABLES: See comments to ESCALABLE.PRO above.
;	-ETIQUETAS: See comments to RMODPROF.PRO above.
;	-FICHPS: See comments to DIBUPS.PRO above.
;	-ESCALAS: See comments to ESCALABLE.PRO above.

;Notes: Managed through XMANAGER.
 
;***************************************************************************
;***************************************************************************

pro graphics2

common ventanas,lista1,lista2,menu,direc,supermenu,pantalla,pintor
common ficheros,listado1,listado2,directorio
common variables,imag,istokes,win,ips,itau,dm,dp
common errores,ierr
common etiquetas,etiquetax,etiquetay,etiquestox,etiquestoy
common fichps,nameps,encaps
common escalas,maxmod,minmod,maxper,minper,iscale
common ventcw,av,st,ib,qb,ub,vb,pp    ;(CW)
common variacw,allstokes,allmag       ;
common oldcw, rold, gold, bold        ;
;loadct,2  -> Pasar de usar palette definida  (CW)
;           
tvlct,rold,gold,bold, /get   ; colores que tiene antes de taumenu (CW)
;
r=[0,255,234,0  ,0  ,255,0  ,255,255,0  ,175]  ; Palette de colores con  
g=[0,255,0  ,140,216,235,235,0  ,148,126,0  ]  ; negro, blanco y 9 mas
b=[0,255,0  ,234,0  ,0  ,228,200,0  ,0  ,201]  ;
tvlct,r,g,b                                    ; la carga en memoria 
;

imag=bytarr(10)
istokes=bytarr(4)
allstokes=byte(0)   ;(CW)
allmag=byte(0)      ;(CW)
ierr=byte(0)

close,/all          ;(CW) -bug de Luis

ips=0 				; Por defecto se pinta en la pantalla
itau=1				; Por defecto los modelos son en tau
ierr=0                          ; Por defecto se pintan errores
etiquetax='!6log (!7s!6)'
;etiquetax='!6z (km)'
etiquetay=strarr(10)
;etiquetay(0)='!8T !6(10!E3!N K)'
etiquetay(0)='!8T !6(kK)'
etiquetay(1)='!8P!De!N !6(dyn cm!E-2!N)'
etiquetay(2)='!8v!6!Dmic!N (km s!E-1!N)'
etiquetay(3)='!8B!6 (kG)'
etiquetay(4)='!8v!6 (km s!E-1!N)'
etiquetay(5)='!7c!6 (!9%!6)'
etiquetay(6)='!7u!6 (!9%!6)'
etiquetay(7)='!6z (km)'
etiquetay(8)='!8P!Dg!N !6(dyn cm!E-2!N)'
etiquetay(9)='!7q!6 (gr cm!E-3!N)'

etiquestox='!7Dk!6 (arbitrary units)'
etiquestoy=strarr(4)
etiquestoy(0)='!8I/I!6!Dc,q!N'
etiquestoy(1)='!8Q/I!6!Dc,q!N'
etiquestoy(2)='!8U/I!6!Dc,q!N'
etiquestoy(3)='!8V/I!6!Dc,q!N'
escalamanual=0
dm=-1
dp=-1

spawn,'ls *.mod',listado11,/SH   ;(CW) Bug de caracteres raros en el path
spawn,'ls *.per',listado12,/SH   ;
if ((listado11(0) eq '') and (listado12(0) eq '')) then begin
   texto='Neither model nor profile files in current directory'
   mensaje,texto
   return
endif 
listado1=reordena(listado11,listado12)
listado2=strarr(1)
spawn,'pwd',directorio,/NOSHELL   ;(CW) Bug de caracteres raros en el path 
;directorio='/scratch/carlos/artic/grad/BvGF/US/pru'
;directorio='/scratch/carlos/artic/grad/BVgGgFg/US/pruSirnew'
;directorio='/scratch/carlos/artic/mu/BVcteGg/US
;directorio='/scratch/carlos/artic/mu/BVcteGgFg/US


;supermenu=widget_base(space=20,title='Model atmospheres menu 1.2',xsize=370,ysize=820)  ;(CW)
supermenu=widget_base(space=20,title='Model atmospheres menu 1.2',xsize=380,ysize=740)  ;(CW) brc
menu=widget_base(supermenu,/column,space=10)
d1=widget_base(menu,/column,space=10,xsize=350)
;d2=widget_label(d1,value='Working directory',$
;    font='lucidasans-bolditalic-12')
direc=widget_text(d1,/editable,value=directorio,uvalue=0,$
font='lucidasans-12',/scroll)
listas=widget_base(menu,/row,space=10)
l1=widget_base(listas,/column,space=10,xsize=160)   
;eti1=widget_label(l1,value='    Files    ',$
;     font='lucidasans-bolditalic-10')
lista1=widget_list(l1,value=listado1,uvalue=1,ysize=9,$  ;(CW) ventana files
font='lucidasans-12')
l2=widget_base(listas,/column,space=10,xsize=160)
;eti2=widget_label(l2,value='       Selected       ',$   
;    font='lucidasans-bolditalic-10')
lista2=widget_list(l2,value=listado2,uvalue=2,ysize=9,$  ;(CW) brc (antes 10 en vez de 53)
font='lucidasans-12')

par=widget_base(menu,/column,space=10)
;eti3=widget_label(par,value=' Physical Quantities ',font=$
;     'lucidasans-bolditalic-10')

;xmenu,['Temperature','Electronic pressure','Microturbulence (Gas pressure)'$
;,'Magnetic field strength','Line of sight velocity',$
;'Magnetic inclination (Density)',$
;'Magnetic azimuth (Absorption coefficient)'],par,/nonexclusive,$
;font='lucidasans-10',uvalue=intarr(7)+3   
;
;xmenu,[' ALL'],par,/nonexclusive,$          ;(CW)
;font='lucidasans-bolditalic-12',uvalue=8+3 ;

pp=widget_base(par,/nonexclusive)        ;(CW)
temp=widget_button(pp,value='Temperature',font='lucidasans-bold-12',uvalue=3)
pres=widget_button(pp,value='Electronic pressure',$
font='lucidasans-12',uvalue=3)
mic =widget_button(pp,value='Microturbulence',$
font='lucidasans-12',uvalue=3)
fld =widget_button(pp,value='Magnetic field strength',$
font='lucidasans-bold-12',uvalue=3)
velo=widget_button(pp,value='Line of sight velocity',$
font='lucidasans-bold-12',uvalue=3)
incl=widget_button(pp,value='Magnetic inclination',$
font='lucidasans-bold-12',uvalue=3)
azim=widget_button(pp,value='Magnetic azimuth',$
font='lucidasans-bold-12',uvalue=3)
zeta=widget_button(pp,value='Geometrical height',$
font='lucidasans-12',uvalue=3)
pgas=widget_button(pp,value='Gas pressure',$
font='lucidasans-12',uvalue=3)
dens=widget_button(pp,value='Density',$
font='lucidasans-12',uvalue=3)


am=widget_button(pp,value=' ALL',$
font='lucidasans-bolditalic-12',uvalue=11)  ;(CW)

parerr=widget_base(menu,/column)
pperr=widget_base(parerr,/nonexclusive)        ;(CW)
uncer=widget_button(pperr,value=' NO error bars',$
font='lucidasans-12',uvalue=33)


stokes=widget_base(menu,/column)
;eti4=widget_label(stokes,value='Stokes Parameters',font=$
;     'lucidasans-bolditalic-12')
st=widget_base(stokes,/row,space=30,/nonexclusive)
ib=widget_button(st,value='  I  ',font='lucidasans-bold-12',uvalue=4)
qb=widget_button(st,value='  Q  ',font='lucidasans-bold-12',uvalue=4)
ub=widget_button(st,value='  U  ',font='lucidasans-bold-12',uvalue=4)
vb=widget_button(st,value='  V  ',font='lucidasans-bold-12',uvalue=4)

av=widget_button(st,value=' ALL ',font='lucidasans-bolditalic-12',uvalue=4);(CW)

ac=widget_base(menu,/row,space=10)

nueva=widget_button(ac,value='Reset',font='lucidasans-12'$
,uvalue=5)

var=widget_base(ac,/column)
zeta=widget_button(var,value='  z  ',font='lucidasans-bold-12',uvalue=9)
;taudep=widget_button(var,value='  t  ',font='symbol15',uvalue=9)   ;!brc
taudep=widget_button(var,value=' tau ',font='lucidasans-bold-12',uvalue=9)    ;brc

dibu=widget_button(ac,value='Plot',font='lucidasans-bold-12',uvalue=6)


escribe=widget_button(ac,value='Print',font='lucidasans-12',uvalue=10)
escala=widget_button(ac,value='Scale',font='lucidasans-12',uvalue=17)
bye=widget_button(ac,value='Quit',font='lucidasans-bold-12',uvalue=8)

pintor=widget_base(xoffset=390,title='Model Atmospheres & Profiles Display')
pantalla=widget_draw(pintor,xsize=621,ysize=740,/frame)

widget_control,supermenu,/realize

xmanager,'taumenu',supermenu

end

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;***************************************************************************
;***************************************************************************
