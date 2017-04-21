pro coord_cart_helio,index,r,ix,iy,x,y,l,b
;+
; Project     : SOLAR 
;
; Name        : COORD_CART_HELIO()
;
; Purpose     : Coordinate transformation from cartesian image coordinates
;		into heliographic coordinates
;
; Category    : Coordinate systems
;
; Explanation : The coordinate reference system of an image is defined
;		in the structure INDEX. The routine extracts the relevant
;		information from INDEX and calculates for a given
;		(X,Y) position in the image the corresponding
;		heliographic coordinates (L,B) in longitude and latitude.
;		(See also definitions and descrition in reference
;		3D-Stereoscopic Analysis of Solar Active Region Loops:
;		I.SoHO/EIT Observations at Temperatures of 1.0-1.5 MK"
;		(Aschwanden et al. 1999, ApJ 515, 842)
;
; Syntax      : IDL> coord_cart_helio,index,r,ix,iy,x,y,r,l,b
;
; Examples    : IDL> !x.range=[0,naxis1]	;defines coordinates of x-axis
;		IDL> !y.range=[0,naxis2]	;defines coordinates of y-axis
;		IDL> tv,bytscl(data)		;displays image 
;		IDL> cursor,ix,iy		;reads out position (x,y)
;		IDL> radii=1.0			;photospheric radius
;      	        IDL> coord_car_helio,index,r,ix,iy,x,y,r,l,b
;		IDL> print,'heliographic coordinates  l,b = ',l,b 
;
; Inputs      : index 	-  structure containing descriptors of data1 
;
;		r	-  altitude in units of solar radii 
;			   (r=1.000 for photoshpere )
;			   (r=1.004 for chromospheric height of 2500 km)
;
;		ix 	-  x-pixel in image DATA (with structure INDEX)  
;
;		iy 	-  y-pixel in image DATA (with structure INDEX)  
;
;	        x,y,r   -  can be vector arrays
;
; Opt. Inputs : None 
;
; Outputs     : x	-  x coordinate in arcsecs relative to Sun center 
;
;               y	-  y coordinate in arcsecs relative to Sun center 
;
; 		l	-  heliographic longitude of position (X,Y)
;
;               b	-  heliographic latitude of position (X,Y)
;
;		x,y,l,b - are arrays if (r,ix,iy) are arrays
;
; Opt. Outputs: None
;
; Keywords    : None
;
; Common      : None
;
; Restrictions: structure INDEX is required to have some minimal solar
;		keywords, such as 
;			.solar_r	solar radius in arcseconds
;			.solar_l0	heliograph.longitude of disk center
;			.solar_b0	heliograph.latitude  of disk center
;			.solar_p	solar position angle
;			.crota2 	roll angle correction 
;
; Side effects: None
;
; History     : 1998-Oct-06, Version 1 written, Markus J. Aschwanden
;	        2000-Jan-17, replace ephemerides by GET_SUN	
;
; Contact     : aschwanden@lmsal.com
;-


;extracting ephemerides from structure INDEX
;p	=index.solar_p
;r0	=index.solar_r
;l0	=index.solar_l0
;b0	=index.solar_b0
;p0	=index.crota2

timeobs =index.date_obs
eph     =get_sun(timeobs)
r0      =eph(1)
l0      =eph(10)
b0      =eph(11)
p       =eph(12)
p0	=-p

crval1	=index.crval1
crval2	=index.crval2
crpix1	=index.crpix1
crpix2	=index.crpix2
cdelt1	=index.cdelt1
cdelt2	=index.cdelt2

;conversion deg-->rad and scaling 
q	=(!pi/180.)
p_	=p *q
p0_	=p0*q
l0_	=l0*q
b0_	=b0*q
x 	=(ix-(crpix1-1.))*cdelt1+crval1	;in arcseconds
y 	=(iy-(crpix2-1.))*cdelt2+crval2	;in arcseconds
x_	=x/(r0*r)		;in solar radii
y_	=y/(r0*r)		;in solar radii
;rotation by position angle 
x1 	= x_*cos(p_+p0_)+y_*sin(p_+p0_)
y1 	=-x_*sin(p_+p0_)+y_*cos(p_+p0_)
z1	= sqrt(r^2-x_^2-y_^2)
;rotation by latitude angle 
x2	= x1
y2	= y1*cos(b0_)+z1*sin(b0_)
z2	=-y1*sin(b0_)+z1*cos(b0_)
;rotation by longitude angle 
x3	= x2*cos(l0_)+z2*sin(l0_)
y3	= y2
z3	=-x2*sin(l0_)+z2*cos(l0_)
;latitude and latitude angle
b_	=asin(y3/r)
n	=n_elements(x3)
if (n eq 1) then arctan,z3,x3,l_,l_deg
if (n ge 2) then begin
 l_	=fltarr(n)
 for i=0,n-1 do begin 
  arctan,z3(i),x3(i),l_i,l_ideg 
  l_(i)=l_i 
 endfor
endif
;conversion from radian to degrees
l	=l_/q
b	=b_/q
end
