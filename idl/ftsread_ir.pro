pro ftsread_ir,data,lam_start,lam_end,xlam=xlam,plot=plot

n=0L
openr,unit,'/home/mcv/idlpro/data/ftsir.dat',/get_lun
readu,unit,n
lam=dblarr(n)
fts=dblarr(n)
readu,unit,lam,fts
close,unit
free_lun,unit

z=where(lam ge lam_start and lam le lam_end)
if(z[0] ne -1) then begin
   xlam=lam[z]
   data=fts[z]
endif else begin
   print,'minimum wavelength: 11106 A'
   print,'maximum wavelength: 54097 A'
endelse

if keyword_set(plot) then begin
   plot,xlam,data,/xsty,tit='Kitt Peak FTS-Spectral-Atlas',xtit='Wavelength [A]',xtickformat='(i5)'
endif

end