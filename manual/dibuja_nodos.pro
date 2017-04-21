pro dibuja_nodos

set_plot,'ps'
device,filename='nodos2.eps',/encapsulated
;hace la figura nodos2.eps del manual de IDL

x=findgen(15)*0.5-5. ; entre -5 y 2
T=exp(x+5)           ;entre 0 y 1100

!p.charsize=1.5
plot,x,t,xstyle=9,xrange=[-5.9,2.9],yrange=[-600,1200],ystyle=5,xtitle='!6 log !7s!6',xmargin=[4,16]
plots,[-5.9,-5.9],[-600,1300]
xyouts,-6.2,0,'Temperature',orientation=90.,size=1.5

plotsym, 0, 1.2, FILL=1
; plotsym, psym, psize, FILL=fill,thick=thick,Color = color
;     PSYM -  The following integer values of PSYM will create the
;             corresponding plot symbols
;     0 - circle
;     1 - downward arrow (upper limit), base of arrow begins at plot value             value
;     2 - upward arrow (lower limt)
;     3 - 5 pointed star
;     4 - triangle
;     5 - upside down triangle
;     6 - left pointing arrow
;     7 - right pointing arrow
;     8 - square

xc=[-5,-4,-3,-2,-1,0,1,2]
yc=xc*0-100.
oplot,xc,yc,psym=8

plotsym, 3, 1.2, FILL=1
xc=[-5,-1.5,2]
yc=xc*0-300.
oplot,xc,yc,psym=8

plotsym, 4, 1.2, FILL=1
xc=[-5,2]
yc=xc*0-500.
oplot,xc,yc,psym=8

xyouts,3,-100,'Location of 8 nodes',size=1.25
xyouts,3,-300,'Location of 3 nodes',size=1.25
xyouts,3,-500,'Location of 2 nodes',size=1.25

device,/close
set_plot,'x'

stop
return
end