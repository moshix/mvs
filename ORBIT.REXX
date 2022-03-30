 /*   rexx for orbital mechanics */
 /* computing  period, eccentricity, and extremem speeds of
    space craft from the altitude of its perigee and apogee
 
    let ap and aq be the altitudes of the perigee and apoggee of the
    spacecraft orbiting around the earth. enter "ap" and "aq"
    in miles as data in Line 100. to output to the printer
    change print definition in TSO        */
 /* ATTENTION THIS IS IN MILES BASIS (IMPERIAL) NOT METERS/SEC,
    SO THIGNS LIKE GRAVITY ARE EXPRESSED IN MILES/SEC, NOT METERS!!!*/
 ap = 6400                                  /* apogee km         */
 aq = 6400                                  /* perigee km        */
 h =  6678                                  /* altitude km       */
 step = 100
 say 'Pls input periapse in km from earth center: '
 pull ap
 say 'Pls input apoiapse in km from earth center: '
 pull aq
 say 'Pls input increments for plit in kilomters: '
 pull step
 r = 6371
 rp = ap + r
 rq=  aq + r
 e =((rq - rp ) / (rq + rp))
 f = e * 10
 say 'Eccentricity ='f
 say 'ap ='ap'km'
 say 'aq ='aq'km'
 do i = ap to aq by step
 say 'at orbital altitude: ' i 'km, velocity is ' vel(ap,aq,i) 'm/sec'
 end
 exit
 
                                            /* parse values      */
 vel: procedure; parse arg vp,vq,vh
 vh =vh*1000                               /* hight in meters   */
                                            /*                   */
 vg = 6.67408E-11                            /* gravity constant  */
 vm = 5.972E24                               /* mass of earth     */
vmu = vg*vm                                 /* mu                */
va = ((vp+vq)/2)*1000
vvs =vmu*((2/vh)-(1/va))                    /* v sqrd for eliptic */
vv = sqr(vvs)                                /* square rt of v2   */
s = 'ms-1 '
return (vv)
end
 
 
sqr: procedure; parse arg x
/*insure that X is numeric, X>=0, and NUMERIC DIGITS is large enough.*/
/* [insert your code to do that (above) below.   (heh heh heh).*/
x=x/1; if x==0 then return x
_=format(x,,,,0) 'E0'; parse var _ x "E" p .
if abs(p)//2 then do; s=sign(p); x=x*10**s; p=p-s; end
g=.5*(x+1)
  do forever
  _=.5*(g+x/g); if g=_ then return (g*10**(p%2))/1
  g=_
end
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
