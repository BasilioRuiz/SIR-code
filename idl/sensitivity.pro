@ read_profile
@ read_model
@ read_RF
pro sensitivity,Stokes_obs,Stokes_syn,model,RF,logtau,uncertainties

;INPUT
; Stokes_obs= filename of the observed profile
; Stokes_syn= filename of the profile resulting from the inversion (fitted)
; model = filename of model resulting from the inversion (fitted)
; RF= the filename of the response function evaluated for the reuslting model

read_profile,Stokes_obs,indx,wave,Iobs,Qobs,Uobs,Vobs
read_profile,Stokes_syn,indx,wave,Isyn,Qsyn,Usyn,Vsyn
read_model,model,logtau,T,pe,mic,b,vlos,g,phi

read_RF,RF,rfi,rfq,rfu,rfv,ntau,nlam

sigI=stdev(Iobs-Isyn) & sigQ=stdev(Qobs-Qsyn)  
sigU=stdev(Uobs-Usyn) & sigV=stdev(Vobs-Vsyn)

search_max,rfi,ntau,nlam,mxrfi,threshold=20
search_max,rfq,ntau,nlam,mxrfq,threshold=20
search_max,rfu,ntau,nlam,mxrfu,threshold=20
search_max,rfv,ntau,nlam,mxrfv,threshold=20

uncertainties=(sigI/mxrfi)<(sigQ/mxrfq)<(sigU/mxrfu)<(sigV/mxrfv)

return
end
;-------------------------------------------------------
pro search_max,rf,ntau,nlam,mxrf,threshold=threshold

mxrf=fltarr(ntau)
for i=0,ntau-1 do mxrf(i)=max(abs(rf(*,i)))

mxx=max(mxrf)/threshold
mxrf=mxrf>mxx

return
end