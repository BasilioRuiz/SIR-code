;_______________________________________________________________________
; pro read_RF,name,rfi,rfq,rfu,rfv,ntau,nlam
;_______________________________________________________________________

; Warning the RF to gamma and phi are defined per radian not per degree

; This prodedure reads the RF written by SIR 
; (running SIR wit ncicles=-1, and choosing a non-null number of nodes
; for one or two parameters, for instance Temperature o B)
; If a 2-component model is used, the output RF is the weighted average
; (whith the filling factor) of both RF

;INPUTS
; name is the filename ej:  'model.rt' 

;OUTPUTS
; nlam total number of wavelengts 

; ntau numer of optical depths layers

; rfi=float(nlam,ntau) is the RF of I Stokes
; rfq=float(nlam,ntau) is the RF of Q Stokes
; rfu=float(nlam,ntau) is the RF of U Stokes
; rfv=float(nlam,ntau) is the RF of V Stokes

; Any RF:
;       -- are for absolute perturbatiion 
;       -- are normalized to the HSRA continuum intensity at disc center
;          evaluated at the central wavelegth of each line 
;       -- Are scaled by the logarithm continuum optical depth stepsize (delta logtau)
;          (i.e. the stepsize of the model file)
;_____________________________________________________________

pro read_RF,name,rfi,rfq,rfu,rfv,ntau,nlam

openr,1,name
readf,1,ntau,nlam4

ntau=long(ntau)
nlam4=long(nlam4)
n=ntau*nlam4

data=fltarr(n)
readf,1,data
close,1

nlam=nlam4/4

d2=reform(data,nlam,4,ntau)
rfi=reform(d2(*,0,*))
rfq=reform(d2(*,1,*))
rfu=reform(d2(*,2,*))
rfv=reform(d2(*,3,*))

return
end
;_______________________________________________________________________




