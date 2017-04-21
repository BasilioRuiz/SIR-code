;_______________________________________________________________________
; pro read_RF_nomag,name,rfi,ntau,nlam
;_______________________________________________________________________

; This prodedure reads the RF written by SIR 
; (FOR a model whithout Magnetic Field i.e. B=0
; (running SIR wit ncicles=-1, and choosing a non-null number of nodes
; for one or two parameters, for instance Temperature or velocity)
; If a 2-component model is used, the output RF is the weighted average
; (whith the filling factor) of both RF

;INPUTS
; name is the filename ej:  'model.rt' 

;OUTPUTS
; nlam total number of wavelengts 

; ntau numer of optical depths layers

; rfi=float(nlam,ntau) is the RF of I Stokes

; Any RF:
;       -- are for absolute perturbatiion 
;       -- are normalized to the HSRA continuum intensity at disc center
;          evaluated at the central wavelegth of each line 
;       -- Are scaled by the logarithm continuum optical depth stepsize (delta logtau)
;          (i.e. the stepsize of the model file)
;_____________________________________________________________



openr,1,name
readf,1,ntau,nlam

ntau=long(ntau)
nlam=long(nlam)
n=ntau*nlam

data=fltarr(n)
readf,1,data
close,1

rfi=reform(data,nlam,ntau)


return
end
;_______________________________________________________________________




