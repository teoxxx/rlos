




 
;Copyright notice:
;This is the RLOS (Relativistic Line-Of-Sight) code, version 1.15.1 This program is released under the LGPL-3.0-or-later licence, Copyright 2015-2018 Theodoros Smponias. 
;Please see the COPYING files in this distribution for copies of the licences.
;
;This program may call IDL routines provided by Andrea Mignone, with the PLUTO code, located in the same folder of this distribution.
;*************************************************************************************************************************************************************************************
;
;***************************************
;021218 B4 anything else, please execute the following six lines of code. Make sure datapath is set to the location of both data and pload 
;(for convenience RLOS currenly runs when RLOS, pload and hydro data are located in the same directory, pointed to by datapath)
;*************************************** 
DATAPATH='Q:\gitstuff\tempy210818SMALL'
cd, datapath
GDL_DIR='C:\Program Files (x86)\gnudatalanguage\gdlde'
PATH=!PATH+'C:\Program Files (x86)\gnudatalanguage\gdlde\'
!PATH=!PATH+datapath
pload, 1, dir=datapath
;***************************************

;201218 following file I/O loop adapted from Harris Geosp. website.
; Select a text file and open for reading
;file = DIALOG_PICKFILE(FILTER='*.txt')
file ='rlos_params.txt'
;********************************
CLOSE, 8
OPENR, 8, file
; Read one line at a time, saving the result into array
text_array = ''
line = ''
WHILE NOT EOF(8) DO BEGIN & $
  READF, 8, line & $
  text_array = [text_array, line] & $
  ;array_double = [array_double, line] & $
ENDWHILE
; Close the file and free the file unit
CLOSE, 8

;********************************

;NEXT IS ASSIGNEMENT LIST, OF PARAMETER VALUES, the one that counts! Later on it may be read from an external file or something. 
;Here we may collectively edit parameters, without searching for them in the code. 
;Then, the corresponding param, is assigned the 'external' value set up in this list. for e.g later in the code, we read: sfactor=sfactor_external
;************************************************
; 191218 conditional_stop is a switch that turns on (1) or off (0) stops along the code execution line, aimed to assist with debugging.
conditional_stop=text_array(4)/1.0D
;191218 debug_comments is a switch that turns on (1) or off (0) comments that assist with debugging.
debug_comments=text_array(6)/1.0D
;sfactr is shrink factor used in pload. Full res is 1, half res is 2, etc.
sfactor_external=text_array(8)/1.0D
;ts param next, tewaks matter speed everywhere, and anytime, in data
speedtweakfactor_external=text_array(10)/1.0D
;select wether manually verride clight, else it keeps its natural value calced by the algo
clight_override_external=text_array(12)/1.0D
;in case we select override=1, then it is gets the clight_preset clight value.else it keeps the natural value, whatever that will be
clight_preset_external=text_array(14)/1.0D
; 140418 HERE INPUT NOMINAL JET VELOCITY in units of c, as set manually in the I/O of PLUTO
jet_norm_velocity_external=text_array(16)/1.0D
;270815 shotmin was the beginning shot it was set to zero, so we always went
;till no shotmax, i.e. 10 in our case!!
; it all begins at shotmin and goes till shotmax 
; 270618 in order to run till a given snapshot, set shotmin to zero 
shotmin_external=text_array(18)/1.0D
; 040816 MUST HAVE enough temporal span in the data to cover the jet time cross interval
;i.e. shotmax-shotmin=more than jet cross time!!! 
;else in runs out of time instants and gives error message!!s
shotmax_external=text_array(20)/1.0D
;aiming angles 
phi2_external=text_array(22)/1.0D
phi1_external=text_array(24)/1.0D
; 170915 the following factor, freqshiftchoice, is 1 for taking into account
;doppler shift of the frequency and other, e.g. 0, for not taking it into account
;for freqshiftchoice=1, we get for each cell a different ncalc, ncalc=nobs/Dfactor
;freqshiftchoice=1.0
freqshiftchoice_external=text_array(26)/1.0D
; 040915 this following string, called dopplerchoice, should equal exactly 1.0
;anything else and there is no dopplerboosting whatsoever.
dopplerchoice_external=text_array(28)/1.0D
;if no dopplerboosting then uncomment the following line SOS
; 050815 we now set the spectral index, larger 
;than zero, i.e. meaning
alphaindex_external=text_array(30)/1.0D

; 060815 assign an onbserving frequency, 
nobs_external=text_array(32)/1.0D
;NLONG is maximum cell number along jet SOS
;SCALING FACTOR IT IS FOR CASE OF MORE CELLS AND HIGHER RESILUTION
;TO AVOID REWRITING THE INSIDES OF THIS CODE SOS
; 060815 in case of higher res available,
;maybe change param long upwards of its currently assigned 
; value of 150.
NLONG_external=text_array(34)/1.0D
;UNITS FROM INIT.c
;RELEVANT PLUTO's time unit is obtained by division of length to speed unit, i.e. 1/3 of sec.
; 090915 copy these from pluto's init.c file of i/b conditions sos
plutolength_external=text_array(36)/1.0D
plutospeed_external=text_array(38)/1.0D
plutodensity_external=text_array(40)/1.0D
plutocelllength_external=text_array(42)/1.0D

;PARAMS LIST ENDS HERE!
;************************************************
;
;
;151218 corrected recent artifact that prevented sfactor from working properly at more than 1 values. (must call pload with shrink=sfactor BEFORE 4D array definitions, in order for NX1,2,3 to get correct values)

; 140818 phi1 MUST range from -90 degrees to plus 90 degrees (-1.57 to plus 1.57 RADS   RADS ARE USED HERE  .
; NOT inclusive the limits. Else, singularity so ratio1f goes 
;neg then ratio1f goes zero so we get cris image from the side, fully doppler boosted, but WRONG GEOMETRICALLY  

;  140818 in order to image a jet at high angles, i.e. bossted, maybe we should position it so as to emerge from the imaging plane  .

;SOLVED 120818 when phi1 goes to 90 degrees, i.e. 1.5 rad or so, then theta in doppler factor calc (IMPLIED IN THIS CODE) goes to zero and 
;therefore D factor is maximized. When it is neg, then Dfactor is opposite effect.   tested now.
;for positive boosting else we get de-boosting PUT NOTE IN GEOMETRY IN PAPER FIG. 1, 2  Also FS off, db on, phi1 neg, et voila!
; 260718 keep freqfactor off, else it decreases, as is, the emission, cause it divides ncalc with ng and then squares that. hust a soothing effect for the tests. But it can be modded to inc. a real complex spectrum, better that the D^a that is now incl. in the D
;D boosting factor. mention in paper DB on, FS off for now works best. If complex spectrum in future, edit FS (into a true shifting of freq) and also DB (remove alpha index) and then use both. SOS

;
; 230718 THIS HAS dlu, dlc, dlr corrected the sfactor ERROR. NOW WORKS OK WITH SFACTOR=2 or 4 etc
; 230618 clight=0.5 works OK with current temporal resolution. jumps every 2 or so. could use abit denser though. SUPER always SET SHOTMAX BELOW MAX SHOT, ELSE ERROR T=0, dt=0
; THIS HAS THE LOS LENGTH ADAPTATION
;; THIS HAS freqfactor to the alphaindex, not just to the second power 270618
;maxphi1 less thaan 1 (0.7 works for sure)) if phi2 very small! else need moar data! 240816
;190816 recheck dboosting calc. right now, ze db array has max of just 1.56?? should be 
;much higher than that with speedtweak used, which boosts speed  near ot over c!
;Is it just angles, or maybe an error of sorts? A subtle one perhaps? ZE RELEVANT CALC  is the one involving ze aaaaa's, bbbbb's, ccccc's, etc
;i.e. re-check ze coslosu calculation vs its implementation in here!


;nx1 is also included in pload, but in 3d mode (non-R) pload was called before ze definition of nx1, therefore it was declared afterwards as nx1 and pload was not ever called again. but now, pload was
;called time and again, messing up with nx1 (it was set to pluto's x dimension, not minus 6.
;now we have nx_1 here and nx1 in pload. pload has nx1, nx2, nx3. We have now: nx_1, ny1, nz1


;040816 MUST HAVE enough temporal span in the data to cover the jet time cross interval
;i.e. shotmax-shotmin=more than jet cross time!!! 
;else in runs out of time instants and gives error message!!


;*********************************************************************
;241218 Following comments refer to a cone-like jet model, created dirctly in LOS code, based on past work during the PhD, without using a hydro code. This model is present heere, butcurrently de-activated.
;those ci's with units, change them to si those without units, leave 'em as is(probably:check'em also)
; CGS for PACHZ ci's, CHECK CHECK CHECK UNITS FOR PACHZ:
;if CGS, then all is in cgs:CHECK UNITS AND RUN TEST CASE FOR
;CONSTANT  DENSITY AND BFIELD TO SEE HOW IT WORKS FOR A VERY SIMPLE CASE
;p=1, ndens=1, cha=1, r1=r2,
; check how p interferes with the  definition of r2 SOS
;need a really narrow jet
;check hj88 geometry, p lateral exp, +other geometries, cad files, etc
;check quant results, also vs hj88 model same inputs
;check, check, check+optimize for galg
;older versions have error in nz1 lt nz10 control checks SOS
;check in dl etc calcs SOS
; in order to get janskys from si units, we need to
;multiply by a large factor, 10 ^18 probably
;********************************************************




;190617

;some special effects used to pronounce certain relativistic effects in code results

;clight simply 'enlarges' artificially the voxels, as far as the light ray is concerned. Therefore it sets the light speed to 4 voxels per sec, thus making relativistic effects more profound (AFFECTS LIGHT RAY SPEED). 

;tweakspeed, on the other hand, means we multiply existing JET MATTER velocities by a tweakspeed factor, in order to emulate a faster jet, without actually re-running the simulation. This also helps to more directly compare with the corresponding non-tweaked version, all other factors kept the same. 

;freqshift choice is a factor setting on off the frequency shift. This only matters when there is some dependence of the emitted intensity on the actual frequency used (ncalc, vs ng) in its calculation. In our case, we use freq squared, no gamma dependence. thus, a quadratic dependence on frequency, for the sake mainly of investigating its effects on the code results. 


;*********************************************************************
;241218 inactive portion of cone-like model here 
;pro peol

;CPU, TPOOL_NTHREADS=3
;CPU, /VECTOR_ENABLE
;CPU, TPOOL_MAX_ELTS = 0, TPOOL_MIN_ELTS = 1
;common su37, gammac
;gammac=1.8
;print, 'ok edo'
;end

;function cp1, x
;common su37
;a=(6.27E+18)
;return, a
;end

;function cp3, x
;common su37
;a=(1.87E-23)
;return, a
;end

;function cp5, x
;common su37
;gammac=1.8
;a=(0.25)*cp3(0.)
;b=gamma((3.*gammac-1.)/12.)
;c=gamma((3.*gammac+7.)/12.)
;d1=((gammac+(7./3.))/(gammac+1.))
;return, a*b*c*d1
;end

;function cp6, x
;common su37
;gammac=1.8
;a=double((1./32.)*(((2.998E+10)/(cp1(0.)))^(2.)))
;b=double(cp3(0.))
;c=double((gammac+(10./3.)))
;d1=double(gamma((3.*gammac+2.)/12.))
;e=double(gamma((3.*gammac+10.)/12.))
;return, double(a*b*c*d1*e)
;end

;******************************************************************

;241218 some optimization here
CPU, TPOOL_MAX_ELTS = 0, TPOOL_MIN_ELTS = 1
;common su37, gammac
; SPECIAL INDEX SET HERE gammac 070418
gammac=1.8
; THE pload's SHRINK FACTOR IS SET HERE
sfactor=2.0
sfactor=sfactor_external;d
;241218 CAREFUL HERE: CAREFUL!  IMPERATIVE to load some using the desired sfactor, in order to get the dims correct NX1, etc 
;for the array definitions shortly after
pload, 1, dir=datapath, shrink=sfactor

;************************************************************************
;241218 here we assign the parameter values to the ones read from the param txt file. This step provides the ability to manually override in case 
;reading from the param file fails for some reason.
;
;
; 090816 HERE WE SETUP A FACTOR IN ORDER TO HOMOGENEOUSLY TWEAK SPEEDS TEMPORARILLY, ALL OVER THE GRID!
;MUST AVOID THIS IN NORMAL RUNS SOS
speedtweakfactor=1.0
speedtweakfactor=speedtweakfactor_external;d

;141218 Next is clight override switch. IF yes, then set to 1 (one). Else, if NOT USED, then set to zero, or any other! 1=on, other =off.
;when ON, i.e. 1, then clight takes the value set below, clight preset, OVERRIDING the naturally calced value of clight.
;WHEN OFF, then no override occurs and NATURAL value is employed. SO set override to 1 and use preset, or set it otherwise and let clght natural value be used.
clight_override=0.0
clight_override=clight_override_external;d
clight_preset=1.0
clight_preset=clight_preset_external;d

; 140418 HERE INPUT NOMINAL JET VELOCITY in units of c, as set manually in the I/O of PLUTO
jet_norm_velocity=0.8
jet_norm_velocity=jet_norm_velocity_external;d


;SUPER 270815 shotmin was the beginning shot it was set to zero, so we always went
;till no shotmax, i.e. 10 in our case!!
; it all begins at shotmin and goes till shotmax SOS
; 270618 in order to run till a given snapshot, set shotmin to zero 
shotmin=2
shotmin=shotmin_external;d
 
;SUPER 040816 MUST HAVE enough temporal span in the data to cover the jet time cross interval
;i.e. shotmax-shotmin=more than jet cross time!!! 
;else in runs out of time instants and gives error message!!

; 270815 shotmin, shotmax must be within available range of data sets
;now we set a default value for shotmax, nothing special, just an initial value before reading it
shotmax=22
shotmax=shotmax_external;d

;***********************************************************************************

;241218 HERE begins the portion from original 3D LOS code, written by Theodoros Smponias in the summer of 2008. (2D LOS code in spring of 2008) 
;
;
;final aiming of LOS
;15508 edited loop to increment by two steps and decrease one step temporarily, since there are two steps of ratiof check. Then , we stored only second step. SOS. must
;store two steps for every loop circle, along the LOS.

;break it up, carefully, into subroutines. SUBROUTINES DO IT. + Test for more cases. REMAIN MTO DO INTEG ALONG LOS AND ALSO
;TO INPUT Cis into e and k and do units.

;This is a code of line-of-sight (hereafter LOS) calculations of radiative transfer.
;It presumes that there is only emission and absorbtion along the LOS, not scattering in different directions (CHECK AGAIN REFS)
;This version is three dimensional (hereafter 3D).
;It works as follows: We  start at a point to the 'left' of a rectilinear grid, i.e. x=x0, y=y0, z=z0.
;Then, we define 2 desired tan(phi(i)f), i=1,2 as a target for the LOS.
;Then the code starts advancing in the discrete grid, either in x (r) (right) or in y (u) (up), or in z (c) (climb).
;There are two  if then else criteria:
;For tan(phi1):
;If current tan(phi(i))<tan(phi(1)f) then (i=1) step up (u), else step right (r) (SOS) (i=2:c-r, i=3:c-u).
;the 2nd angle is tans phi 2 =z/sqrt(x^2+y^2) if lt ratio2f then icrease z, else increase x or y. Which one, use again the 1st criterion to decide.
;always update the los arrays after every grid step.
;2 angles phi1, phi 2 like in spherical coordinates. Not a 3rd variable!
;This way, we get closer to the desired LOS, kind of 'asymptotically'.
; Carry on till you reach the ends of the grid. Do not forget to save your results.
;angles of LOS, in radians
;tan(phi1)=y/x, tan(phi2)=z/sqrt(x^2+y^2)
;200815 phi1 is azimuth from x to y, on xy plane
;200815 phi2 is elevation, from xy plane towards z

;phi1=0.590D
;phi2=0.20D

; 060815 a few lines above it shows the definition of phi1, phi2 in terms of (x,y,z). phi1
;lies on xy plane, while phi2 is on a
;plane perpendicular to the xy but at an angle phi1 
;(different anyway than xz plane) to the xz plane.

; 03815 we shall try difernt angles. STUDY ANGLE SETUP IN 3D 
;phi1=-80.00D
;phi1=-4.60177602D

; 140818 phi1 MUST range from -90 degrees to plus 90 degrees (-1.57 to plus 1.57 RADS RADS ARE USED HERE SOS.
; NOT inclusive the limits. Else, singularity so ratio1f goes 
;neg then ratio1f goes zero so we get cris image from the side, fully doppler boosted, but WRONG GEOMETRICALLY SOS

;phi1=1.57D
;phi1=1.6D
;phi1=1.682D
phi2=0.005D
phi2=phi2_external;d

;phi1=1.57D
;SOS120818 above gives redshift, below gives blueshift. So, 91 degrees is BLUEshift, 89 degrees is RED shift. 
;phi1=1.6D

; 140818 this is it -1.6814D means 80D and 1.6814D means -80D, aall in radians SOS. 
phi1=1.6814D
phi1=phi1_external;d


; 090815 we temp set a small value to reinforce doppler effect 
;OR A LARGE ANGLE (in radians those are)
;phi1=0.050D
;phi2=0.70D


;*************ABABABABAB****************

; 170915 the following factor, freqshiftchoice, is 1 for taking into account
;doppler shift of the frequency and other, e.g. 0, for not taking it into account
;for freqshiftchoice=1, we get for each cell a different ncalc, ncalc=nobs/Dfactor
;freqshiftchoice=1.0
freqshiftchoice=0.0
freqshiftchoice=freqshiftchoice_external;d
; OS EDO 110418 3.38pm SOS


; 040915 this following string, called dopplerchoice, should equal exactly 1.0
;anything else and there is no dopplerboosting whatsoever.
dopplerchoice=1.0
dopplerchoice=dopplerchoice_external;d
;if no dopplerboosting then uncomment the following line SOS
;dopplerchoice=0.0
print, 'dopplerchoice=',dopplerchoice

; 050815 we now set the spectral index, larger 
;than zero, i.e. meaning

alphaindex=2.0
alphaindex=alphaindex_external;d

;nx1 is also included in pload, but in 3d mode (non-R) pload was called before ze definition of nx1, therefore it was declared afterwards as nx1 and pload was not ever called again. but now, pload was
;called time and again, messing up with nx1 (it was set to pluto's x dimension, not minus 6.
;now we have nx_1 here and nx1 in pload. pload has nx1, nx2, nx3. We have now: nx_1, ny1, nz1


; 060815 assign an onbserving frequency, 
;maybe move to beginning SOS
nobs=1000000000.*8.
nobs=nobs_external;d

;NLONG is maximum cell number along jet SOS
;SCALING FACTOR IT IS FOR CASE OF MORE CELLS AND HIGHER RESILUTION
;TO AVOID REWRITING THE INSIDES OF THIS CODE SOS

; 060815 in case of higher res available,
;maybe change param long upwards of its currently assigned 
; value of 150.

NLONG=150.0
NLONG=NLONG_external;d
;
;SOS230718here corrected sfactor bug SOS!
dlu=1.0*sfactor
;pluto grid not exactly isotropic
dlr=0.95*sfactor
dlc=1.0*sfactor
;020815 this 0.95 factor put in a file or at beginning and set as a param SOS
;some optimization, for the just-1-d loop though

; cell length is already included is kabs definition SOS
mikoscell=1

;020815 SUPER INCORPORATE FREQUENCY SHIFT, just like in original HJ88 model from JBank. DO IT IT

;UNITS FROM INIT.c
;RELEVANT PLUTO's time unit is obtained by division of length to speed unit, i.e. 1/3 of sec.
; 090915 copy these from pluto's init.c file of i/b conditions sos
plutolength=1.0*10.0^10.0
plutolength=plutolength_external;d
plutospeed=3.0*10.0^10.0
plutospeed=plutospeed_external;d
plutodensity=1.67*10.0^(-24.0)
plutodensity=plutodensity_external;d

plutotime=plutolength/plutospeed


;nx1 is also included in pload, but in 3d mode (non-R) pload was called before ze definition of nx1, therefore it was declared afterwards as nx1 and pload was not ever called again. but now, pload was
;called time and again, messing up with nx1 (it was set to pluto's x dimension, not minus 6.
;now we have nx_1 here and nx1 in pload. pload has nx1, nx2, nx3. We have now: nx_1, ny1, nz1
;nx_1=114
;ny_1=194
;nz_1=114

nx_1=NX1-6
ny_1=NX2-6
nz_1=NX3-6
; repeat this stuff 'cos we used both in different areas SOS
;if change one, then should change the other as well for sure.
;Else, recipe for disaster

ny1=NX2-6
nz1=NX3-6


;CGS value for cell length form PLUTO
; MUST multiply (NOT DIVIDE) BY pload's SHRINK SO, i.e. by sfactor

plutocelllength=1.0*10.0^10.0
plutocelllength=plutocelllength_external;d
lcell=plutocelllength*sfactor

;factorc converts from cgs to los cell length units

factorc=lcell
factort=1/plutotime
if (debug_comments eq 1) then print, factorc, factort, plutotime, lcell, ' .......... TSA11'
;CGS value for speed of light in medium, c, in cells per second 020815
; 110915 we corrected clight to use pluto time units instead of 
;seconds. time in pluto is measured in pluto time units
;and we use the pluto t array for calculations along the los etc.
;consequently, clight should be in cells per pluto time unit, 
;NOT in cells per sec, no.

; 301217 MUST CONSIDER SFACTOR IN CLIGHT SCALING 
clight=3.0*10000000000.0/(factorc*factort)
; next calculation assigns clight to its derived value, from PLUTO settings, plutotime, cell size, etc etc. Is 0.5 probably. 
print, 'original clight',clight

;temp stuff here 030816 cause we dont have that many snapshots also recheck dimensional stuff from
;PLUTO etc and also vs ours here!!! cm, sec, etc etc!
; 090816 clight only matters for time calculations. 
;NOT for dboosting calcs! there, all calcs take c=1 
;clight=400.0
; 141218 next we employ the override factor, in order 
if (clight_override eq 1) then (clight=clight_preset) 

if (conditional_stop eq 1) then stop


TPOOL_MAX_ELTS=1000
if (debug_comments eq 1) then print, nx_1, 'EDO ORE'
; array4d indexing begins at 0, whereas pload's snapshot indexing begins at 1. Therefore
;some RAM is wasted here (an extra snapshot is defined, just in case)
; shotmin is set at beginning of file. same as shotmax SUPER SOS

dimofvector=(nx_1+6.0)*(ny1+6.0)*(nz1+6.0)

arrayvector=dblarr(dimofvector,1)

;fre=dblarr(14,10368)
;freavg=dblarr(1,10368)
;fre=dindgen(14,10368)
;freavg=dindgen(1,10368)
;freallyears=dblarr(14,10368,21)

;ttt is just a convenience parameter for defining the 4d arrays the bigger it is the more RAM we
;consume, yet it leaves more headroom for loose operations
ttt=4

array_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
array=dblarr(nx_1+6,ny1+6,nz1+6)
emiss_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
kabs_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
kabs2_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
losdraw_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)

rho_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
prs_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
temp4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
bx1_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
bx2_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
bx3_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
vx1_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
vx2_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
vx3_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
v_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
; corrected sqrt in bfield, 020815 
bfield_4d=sqrt(bx1_4d*bx1_4d+bx2_4d*bx2_4d+bx3_4d*bx3_4d)
dopplerfactor_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
gammalorentz=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
ng=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
ncalc=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
coslosu_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
thetau_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)


; 060815 we do this assignement in order to keep ng an array sos
;220815 we keep it as is, it should work ok for the array calculation fo dopplerfactor
;
;CAREFUL 070418 ng is defined in PARAMETER REGION near the beginning of the code
ng=ng+nobs

;Now the following few comment lines are done globally! 
;
; {060815 later on we assign ng according to local doppler factor
;SUPER cannot do it globaly, as dopplerfactor is really assigned only 
;along the LOS
;therefore we can only assign ng along the LOS, not everywhere
;still we keep the assignement of the 4d ng array and doppler factor array,
;for convenience}


;array1 to b defined from string 0.
;write it down sos

;some reminder for concat here
;fluxidl=fluxdensitypachz(fre)
;14 columns, 0 to 13, 1st ans 2nd are coordinates, rest are data
;10368 rows, 0 to 10367, equal to the number  of grid points on the model global map.
; do this concat for a variable times of snapshots, i.e. for shotmax times. try 2 figure it out! e.g. write it so may times, up to a notional maximum, to be reminded above where shotmax is initialized,  then rest are zero!
;freavg=(fre[2,*]+fre[3,*]+fre[4,*]+fre[5,*]+fre[6,*]+fre[7,*]+fre[8,*]+fre[9,*]+fre[10,*]+fre[11,*]+fre[12,*]+fre[13,*])/12.;
;frepinakas=[fre[0,*],fre[1,*],freavg]

if (debug_comments eq 1) then print, nx_1,'No 1'
; this loop covers the whole lot SOS

;calculations based on pload's data 020815


v=sqrt(vx1*vx1+vx2*vx2+vx3*vx3)


array=rho*v

for shotindtemp=shotmin,(shotmax-1) do begin
;SUPER HERE WE SETUP TO BEGIN FROM A SHOTIND LARGER THAN ONE SOS
; 270815 we reset the following line, in order to start from shotmin
;not from 1 every time!!! SOS
;oldversion
shotind=shotindtemp+1.0-shotmin
; WE MUST PLOAD SHOTMIN+SHOTIND, BUTASSIGN JUST SHOTIND
;ASSIGNEMENT GOES FROM 0 TO SHOTMAX-SHOTMIN
;WHEREAS OPENING GOES FROM SHOTMIN TO SHOTMAX 
;SUPER HERE 270815 SOSARA
;...and new version
;shotind=shotindtemp+1.0
if (debug_comments eq 1) then print, 'lababula', shotind, shotindtemp, shotmin, shotmax
bd=string((shotind))
;ad='DLF.data'
if (debug_comments eq 1) then print, nx_1,'No 1.1'

bd=strcompress(bd,/remove_all)
;cd=bd+ad
if (debug_comments eq 1) then print, bd
; bd is string? we use shotind for ze shot number SOS

;HERE WAS THE ERROR ALL ALONG WE MUST PLOAD SHOTIND (INDEX OF 4D ARRAY)+SHOTMIN
;ELSE IT KEEPS LOADING THE FIRST SERIES OF DATA, E.G. FROM 1 TO 10 
;NEVER GOES TO HIGHER NUMBERS LATER ON,
;NO MATTER WHAT WE TELL IT TO DO 
; solved 270815 SOS
pload,shotmin+shotind, shrink=sfactor
if (debug_comments eq 1) then print,'t prin', t
if (debug_comments eq 1) then print, 'dt prin',dt
if (debug_comments eq 1) then print, 'shotind loaded', shotind
; CORRECTION 20-may-14 shotind may now begin at more than 1
; RETHINK THIS INDEX -1, or plus 1? check !!
;SOS

;sos010815 test high velocity remove after testing sos
;vx1=3*vx1
;vx2=3*vx2
;vx3=3*vx3
;v=3*v

;assign current 3d arrays to corresponding slices of 4d arrays 020815
array_4d[shotind,*,*,*]=array
rho_4d[shotind,*,*,*]=rho
print, shotind
if (debug_comments eq 1) then print, rho(1,1,1), 'tsa11'
if (debug_comments eq 1) then print, rho_4d(shotind,1,1,1),'tsa22'
prs_4d[shotind,*,*,*]=prs
;temp4d[shotind-1,*,*,*]=temp
bx1_4d[shotind,*,*,*]=bx1
bx2_4d[shotind,*,*,*]=bx2
bx3_4d[shotind,*,*,*]=bx3
vx1_4d[shotind,*,*,*]=vx1
vx2_4d[shotind,*,*,*]=vx2
vx3_4d[shotind,*,*,*]=vx3
v_4d[shotind,*,*,*]=v
; b4d=? 020815


; v is not the same? SOMETHING CHANGES IT? SUPER CHECK IT SOS!!!
; ABOVE v value not same as corresponding v4d

; new stuff added for rlos here
;2 B edited, they originated from clima thing i did 6 years ago.

;, shrink=2
;arrayshock=RHO*0.0
;arrayshock=shockfind(PR,V1,V2,eps_min=0.33,eps_max=5.0)
print, shotind
print,'t meta', t
print, 'dt meta',dt
endfor
if (debug_comments eq 1) then print, nx_1, 'No 2'
if (conditional_stop eq 1) then stop
if (debug_comments eq 1) then print, nx_1  , 'No 3'
;SUPERSOS
;next comes ALGO to find snapshot position, given current time curtime
;to be put suitably inside ze code, at algo location SOS

; 14-05-15 we do the following only every 2 los steps
;since the number of snapshots is much less than the
;number of los voxels. Therefore we do not repeat
;the curtime snapshot update check in the second0
;part of the counter los direction loop

;020815 the following has been added later on but there it interferes with the 
;algo assignement of time snapshot numbers
;consider moving to here? or if not possible, adjust the rest of relevant assignements

curtime=140
if (debug_comments eq 1) then print, where((curtime-t)<0,count)
if (debug_comments eq 1) then print, 't',t
if (debug_comments eq 1) then print, size(t, /n_elements)
;next is snapshot number, give or take 1, for a current time of curtime.
if (debug_comments eq 1) then print, size(t, /n_elements)-count
if (debug_comments eq 1) then print, nx_1, 'No 4'
if (conditional_stop eq 1) then stop




;200815 we define coordinates that point in the 
;direction of the two directional angles, as defined in the initialization sector, near the beginning of this code.
;241218 see paper for details on lx1, lx2, lx3 etc.
lx1=cos(phi2)*cos(phi1)
lx2=cos(phi2)*sin(phi1)
lx3=sin(phi2)
if (debug_comments eq 1) then print, 'phi1 phi2',phi1,phi2,'lx1 lx2 lx3',lx1,lx2,lx3
;ok compared lx1,lx2,lx3 vs sphericaltest file with same phi1, phi2
;and yields same coord results seems ok for phi's less than 90 degrees(verified)
;or angles less thaN 180 DEGREES (UNVERIFIED)

;;210815 we include here the previous formalism for comparison
;aaa=(xx-xx0)*ux+(yy-yy0)*uy+(zz-zz0)*uz
;bbb=sqrt((xx-xx0)*(xx-xx0) +(yy-yy0)*(yy-yy0) +(zz-zz0)*(zz-zz0) )
;ccc=sqrt(ux*ux+uy*uy+uz*uz)
;coslosu=aaa/(bbb*ccc)
;thetau=acos(coslosu)

; 060816 it seems as if at coslosu =cos (90 degrees)=0, dfactor is min (1-cccc*0)=1 =max denominator of dfactor 
;whereas at coslosu = cos(0)=1, denom is min: 1-cccc thus dfactor is maximum, as far as angle is cencerned. Thus, dfactor maximizes at angles =0, 
;for given speed at each cell!
;try smallish angles for best visible dfactor effect results  therefore!



;******************
; 090816 HERE WE HOMOGENEOUSLY TWEAK SPEEDS TEMPORARILLY, ALL OVER THE GRID!
;MUST AVOID THIS IN NORMAL RUNS SOS
; *** THIS IS TEMP FIX DO NOT KEEP IT SOS!!!!!!!!!
;241218 ended up being ts factor so we kept it!


vx1_4d=vx1_4d*speedtweakfactor
vx2_4d=vx2_4d*speedtweakfactor
vx3_4d=vx3_4d*speedtweakfactor
v_4d=v_4d*speedtweakfactor

;******************
; 110816 check this calc hereafter, why even at very high speed(tweaked) dopplerfactor never exceeds 1.15 max? 
;shouldnt it reach to infinity or something? check! testing extremes!
; it is a matter of angles also!!! check right above, lx1,lx2,lx33 SOS!
aaaaa=(lx1*vx1_4d+lx2*vx2_4d+lx3*vx3_4d)
bbbbb=sqrt(lx1*lx1+lx2*lx2+lx3*lx3)
ccccc=sqrt( (vx1_4d*vx1_4d) + (vx2_4d*vx2_4d) + (vx3_4d*vx3_4d) )
coslosu_4d = aaaaa/(bbbbb*ccccc+0.000001)
thetau_4d=acos(coslosu_4d)
; 291217 FOLLOWING IS FULL OF NANS, NONE OF THE ABOVE aaaaa, bbbbbb, ccccc, theta
;PROBLEM LOCATED ON (sqrt(1-ccccc*ccccc)), has NANS
gammalorentz_4d=1/(sqrt(1-ccccc*ccccc))

; screws the calculation DROP IT SOS!!! sets the minimum zero, while it must be one!!
;120816 this is meant to remove naNs and replace em with zeros SOS
;gammalorentz_4d=gammalorentz_4d*(finite(gammalorentz_4d))


;dopplerfactor_4d=(sqrt(1-ccccc*ccccc))/(1-ccccc*coslosu_4d)
;270718 changed the expression, for optimization reasons
dopplerfactor_4d=1/( gammalorentz_4d*(1-ccccc*coslosu_4d) )

;110816 this is meant to remove naNs and replace em with zeros SOS
;dopplerfactor_4d=dopplerfactor_4d*(finite(dopplerfactor_4d))

;define theta_4d=
; 170915 ncalc=ng/dopplerfactor_4d
;we use the 0.0000000001 factor in order to avoid division by zero
;if an element of the dfactor array turns out
;for some reason to be zero SOS
ncalc=ng*(1/(dopplerfactor_4d+0.000000001))


if (debug_comments eq 1) then print, 'max(vx1_4d)',max(vx1_4d)
if (debug_comments eq 1) then print, 'max(vx2_4d)',max(vx2_4d)
if (debug_comments eq 1) then print, 'max(vx3_4d)',max(vx3_4d)
if (debug_comments eq 1) then print, 'max(v_4d)',max(v_4d)

if (conditional_stop eq 1) then stop
;Final ratio of number of up steps to number of right steps. Final tan(phi), i.e. tan(phif). Our objective.
ratio1f=tan(phi1)
ratio2f=tan(phi2)

;initialize current (as opposed to final) 'up' step number, to one
uc=0
; current 'right' step counter begins at 1, i.e. one first step right, saves div by 0! We presume that it is intrinsically
;accounted for in the loops later on in the code.
rc=1
;similar for current 'climb' counter, z-dimension. (NOTE: This could be made to begin at zero, had we wished to do so.(SOS: AND HAD ACCOUNTED FOR IT IN THE LOOPS)
cc=0
;Initialize current ratio of up to right steps, at zero (i.e. (uc/rc)=0).
;This way, 2nd step is always up (first was right), as it has to increase ratioc up from zero, in the if construct.
;similarly for second tan(phi2) ratio2c
ratio1c=0.0
ratio2c=0.0

;model grid size:x,y,zprint, za1, za2, za3, za4, za5, za6, rz, ((xind-x1)^2+(yind-y1)^2)

;FROM NOW ON MATHEMATICA STUFF
;array = RANDOMU(seed, j, k,l)
;array=RHO
;  SO we assume only relativistic matter has
;shocks' emissions

;multiply by velocity just to see how things go





















r1=3
;print, 'tracker pos 0.1'
r2=14

;print, 'tracker pos 0.2'
x1=15
;CAREFUL WITH z1, z2
; it must be: (1<y1<yind<y2<ny1+1)
y1=1
y2=ny1-1

;print, 'tracker pos 0.4'
z1=1
z2=nz1+5
;print, 'tracker pos 0.6'


;we assume that initially, bfield array was set to zeros
;parameters to adjust bending of the axis of the cone of the jet:
;check by 2d plots of jet axis

a1m=0.0
b1m=0.0
a1exp=1.4
b1exp=1.3



;HERE HJ88 GEOMETRY DEFINED
;define in radians, angle between bfield and velocity of particles just a parameter really
theta=1.0


;distance in kpc
dkpc=3.0
;Some HJ88 model specifics
;p is parameter of HJ88 jet lateral expansion
p=0.5

; Z HAS NOW BECOME Y JET ALONG Y SOS
;length, in SI m, of grid cell along z
;length, along z, of computational domain, in mas
zmaxmas=20.
deltazmas=(zmaxmas/float(nz1))
zamas=1.0
dzbmas=0.5
zadeg=((zamas)/(3600.*1000.))
dzbdeg=((dzbmas)/(3600.*1000.))
zmaxdeg=((zmaxmas)/(3600.*1000.))
; CGS UNITS FOR LENGTH, cm i.e. cm in HJ88 model all the time (cos of the *100. factor)
za=(sin(2.*!pi*zadeg/360.))*dkpc*(3.1E+19)*100.
dzb=sin(2.*!pi*dzbdeg/360.)*dkpc*(3.1E+19)*100.
zmax=sin(2.*!pi*zmaxdeg/360.)*dkpc*(3.1E+19)*100.
;deltaz is voxel length along z axis
deltaz=zmax/(float(ny1))
;magnetic field at za, in tesla SI
cha=0.1
;particle density at ref. point, SI m^-3
cka=10.0
;initial mach number at ref point. HAs to do with initial opening angle, therefore
;defined in relation to the cone shape define before
;print, deltaz, zmax, nz1
;link those with above


;print, 'tracker pos 1'
;Mach number definition here.
;so far valid for a conical jet
abcde=double(r2-r1)
wwwww=double(y2-y1)
cma=1./(sin(abcde/wwwww))

;print, 'tracker pos 2'
;print, ((r2-r1)/(z2-z1)),sin((r2-r1)/(z2-z1)),cma,1./(sin((r2-r1)/(z2-z1)))
;print, 'tracker pos 3'


;print, 'tracker pos 4'
lamda1=(cha)*(za^p)
tau1=(cka)*(za^(2.*p*(gammac+2.)/3.))
ni1=(1.)/((za^(p-1.))*cma)
;print, 'tracker pos 4.1'
;define b, n arrays for HJ88 model
bfield=dblarr(nx_1+6,ny1+6, nz1+6)
;print, 'tracker pos 4.2'
ndens=dblarr(nx_1+6,ny1+6, nz1+6)
denshj88=fltarr(nx_1+6,ny1+6, nz1+6)
denshj880=10000000000000
;print, 'tracker pos 5'
;ndens=ndens+3.0
if (debug_comments eq 1) then print, nx_1

; hj88 has z axis along z. whereas PLUTO has jet along the X axis. we must therefore align one with the other. Easier seems to be to align the
;hj88 jet with the y axis. do it therefore.
for zind=z1,nz1+5 do begin
 for yind=1,ny1+1 do begin
  for xind=1,nx_1+1 do begin
  ; rz must be a function of p in HJ88 SOS
  ;only in HJ88 though
  ;r(z)=r1*((z/z1)^p)
;better to avoid too small r1 ruining the ratio
  ;r(z)=r2*((z/z2)^p)
x1m=x1+a1m*(yind^a1exp)
z1m=z1+b1m*(yind^b1exp)
;za1=(r2-r1)
;za2=(z2-z1)
;za3=(zind-z1)
;za4=r1
;za5=((za1*za3)/za2)+r1

;rz=za5
; 'factor' affects the scale of the jet width. Can be set to set up a narrower or wider jet.
;Just a scale of the jet narrowness. This should affect the jet opening angle and the jet mach number, normally.
factor=1.0
rz=factor*r2*((double(yind)/double(y2))^p)
;SOS
; this is for the p-HJ88 model


;MUST HAVE NON BEMT JET FOR THE FOLLOWING B and N TO BE VALID ELSE, USE BENT AXIS AS A JET CONE AXIS

;020815 group the following if's, optimize'em

if (((xind-x1m)^2+(zind-z1m)^2) lt rz*rz) then Bfield(xind,yind,zind)=lamda1*(1./((yind*deltaz)^p))
k123=2.*p*(gammac+2.)/3.
if (((xind-x1m)^2+(zind-z1m)^2) lt rz*rz) then ndens(xind,yind,zind)=tau1*(1./((yind*deltaz)^k123))
; HERE add a HJ88 density, compared to a relative value at z0.
if (((xind-x1m)^2+(zind-z1m)^2) lt rz*rz) then denshj88(xind,yind,zind)=denshj880*(1./((yind*deltaz)^p))
;JUST TO CHECK IF CONSTANT DENS GIVES CONSTANT GAMMA RAYS EMISSION CHECK
;if (((xind-x1m)^2+(zind-z1m)^2) lt rz*rz) then denshj88(xind,yind,zind)=denshj880*(1./((10.0*deltaz)^p))
 ;if (((xind-x1)^2+(yind-y1)^2) lt rz*rz) then ;print, za1, za2, za3, za4, za5, rz*rz, ((xind-x1)^2+(yind-y1)^2), xind,yind,zind,(xind-x1)^2

  endfor
 endfor

endfor
slicenumber=15
; HERE WE SELECT THE SLICE OF densHJ88 to plot
;qwerty=REFORM(denshj88[*,slicenumber,*],100,100)
;zita is the lenth along the jet axis. It is not valid foe a bent jet. It is an HJ88 model parameter.
;zita=z-z1
surface, denshj88[2,*,*], ax=64, az=102
;HERE ENDS THE DEFINITION OF MODEL GEOMETRY

 ;surface, REFORM(denshj88[15,*,*],100,100), ax=64, az=102
if (conditional_stop eq 1) then stop


;print, 'tracker pos 7'



;stop



if (debug_comments eq 1) then print, nx_1











;print, array
;for jj=0,((j*j)-1) do begin
;print, jj
;arrayvector(jj)=ARRAY_INDICES(arrayc,jj)
;if (debug_comments eq 1) then print, arrayvector(jj)
;end
; print, ind, array[ind[0],ind[1]], format = '(%"Value at [%d, %d] is %f")'
;if (debug_comments eq 1) then    print, ind
;if (debug_comments eq 1) then print, ARRAY_INDICES(array, 6)
;if (debug_comments eq 1) then print, array
;if (debug_comments eq 1) then print, ARRAY_INDICES(array, location)
; HERE WE PUT TEMPORARILY THE HJ88 DENSITY INSTEAD OF THE HYDROCODE ONE!! NEEDS
;NORMALIZATION, THOUGH, IT IS EASY TO DO IN A STEADY STATE MODEL!
;array=denshj88

;020815 consider 4D to mathematica and implications

; here we select the hydro thing


array=float(RHO)
;VELOCITY AND B FIELD SCALARS AS READ BY PLOAD
arraybscalar=((float(BX1))*(float(BX1))+(float(BX2))*(float(BX2))+(float(BX3))*(float(BX3)) )^(0.5)
arrayvscalar=((float(VX1))*(float(VX1))+(float(VX2))*(float(VX2))+(float(VX3))*(float(VX3)) )^(0.5)
; here we select the HJ88 thing
;array=float(denshj88)
;1D VECTORS TO TRANSFER DATA TO MATHEMATICA
arrayvector = REFORM(array,dimofvector, 1)
arraybscalarvector = REFORM(arraybscalar,dimofvector, 1)
arrayvscalarvector = REFORM(arrayvscalar,dimofvector, 1)
resultvector=arrayvector*0.0
;print, arrayvector,resultvector
; Create some data to store in a file:

; Open a new file for writing as IDL file unit number 1:
CD, datapath+'\databis'
OPENW, 1, 'newfilelong.datab'
POINT_LUN, 1, 0
; Write the data in D to the file:
WRITEU, 1, arrayvector
; Close file unit 1:
CLOSE, 1
;magnetic field scalar value
OPENW, 3, 'newfilelongb.datab'
POINT_LUN, 3, 0
; Write the data in D to the file:
WRITEU, 3, arraybscalarvector
; Close file unit 3:
CLOSE, 3

OPENW, 4, 'newfilelongv.datab'
POINT_LUN, 4, 0
; Write the data in D to the file:
WRITEU, 4, arrayvscalarvector
; Close file unit 4:
CLOSE, 4

OPENR,2,'newfilelong.datab'
POINT_LUN, 2, 0
; ABOVE we replaced resultvector with newfilevector. in reality,
;mathematica acts on arrayvector and produces resultvector. here, as a test, they are identical.
;in reality, they are simply of same dimensions.
READU,2,resultvector
CLOSE,2
;print,'tsa', resultvector
resultvectorfinal=REFORM(resultvector,nx_1+6,ny1+6,nz1+6)
; NEED TO BRING BACK BFIELD DATA FOR LOS RUNS WITH B FIELD STUFF. TO BE DONE BITCHEZ!
if (conditional_stop eq 1) then stop
;END OF MATHEMATICA STUFF

; 020815 move all such params such as sfactor to a file, or to the beginning SOS

; 050515 remember to return the CD from the databis to the main data folder if needded SOS.

;020815 here 3d(4D) verification checks
 

;Coordinates of entry point to the grid, of LOS
nx10=2
ny10=2
nz10=2
;Current coordinates, on our LObS (not neccessarily the ideal LOS, of course)
nx1current=nx10
ny1current=ny10
nz1current=nz10
;arrays of initial grid data: model geometry and the like. couls also be input from hd runs output.
;variable to hold total intensity, the sum or voxel intensities along the los.
in=0.0
;array of dIn, voxel intensities
incurrent=dblarr(nx_1+ny1+nz1)

din=dblarr(nx_1+ny1+nz1)
dtaun=dblarr(nx_1+ny1+nz1)
tauncurrent=dblarr(nx_1+ny1+nz1)
;describe the LOS:
;Define VECTOR (i.e. 1D) arrays, at a size that is larger than any possible value of the LOS length in the grid.
;A convenient choice for such a size is the grid diagonal.
;Optical depth, so far till current point, along (i.e. at every LOS point) our LOS
lostdepth=dblarr(nx_1+ny1+nz1)
;emission coefficients for each point along our LOS
lose=dblarr(nx_1+ny1+nz1)
;absorption coefficients for every point along our LOS
losk=dblarr(nx_1+ny1+nz1)
;an auxiliary array

;setup the cone axis x1,y1 and the small cone radius r1 and the large one, r2
;the cone is parallel to z.z1=bottom, z2=top of the cone
;r1,r2 must be less than half of the smallest of nx1,ny1
;z1 must be less than z2
;both z1 and z2 must be more than nzo but less than nz1



;Define whole grid now. Our arrays are now 3D, way much bigger. For every point in our grid, there are the
;emissivity and abrospivity for the radiation. These may originate from a steady state model, ala HJ88. The
;geometry of such a model may be directly imposed by us (a CAD feat in 3D!), in the definition of the following arrays.
;Or, it may come from the results of hydrodynamic (HD) simulations, using a relation than connects dynamic
;flow properties (pressure, density, velocity, etc) to radiative properties, like e and k.
;Regridding may be necessary, as the grid dimensions of the HD calcs
;are different, in general, from the radiative flow calcs
;(usually bigger, for such distant astronomical systems! what we see
;is fainter than what a decent HD run requires.
;The rest are just working assumptions, for the sake of not using a 40 by 40 HD grid on our nice PCs!)
;For now, we simply define the 3D arrays  at random values, and  in future versions this may be changed to a model 3D
;geometry. We could have bent jets, rings, clouds, whatever.


;emissivity
if (debug_comments eq 1) then print, nx_1+6,ny1+6, nz1+6
emiss=dblarr(nx_1+6,ny1+6, nz1+6)

emiss=rho
;emiss=emiss+2.0


;  180816 WTF is this freqfactor thing? isnt freq effect just an extra D^alphaindex effect? 
; 170915 we now assign the frequency shift to the emission SOS

; 180816 mallon edo exoume mia apopeira sysxetisis tis entasis me ti sixnotita, ti allo na paizei pia eleos! freqfactor kiar***ies. gm ti mnimi mou mesa!
;if (freqshiftchoice eq 1.0) then (  freqfactor=(ncalc/2.)^((1.-gammac)/2.))  else  (  freqfactor=(ng/2.)^((1.-gammac)/2.))
;190816 we set a square dependence, in order to better highlight results in the loglog plot
;270618 replaced below n/n exponent from 2 to alphaindex SOS
;290618 NOT NEEDE THE AOVE REPLACEMENT FREQFACTOR IS A SPECTRUM EFFECT< SOMETHING EXTRA REALLY JUST TURN IT OFF FOR THE APPLICATION AND ALSO DO JUSTIFY IT BY HAND CALCULTION  
; 270718 the ratio ncalc/ng is weakening the whole thing, better choice perhaps ng/ncalc%(ln(a)), or something. It suggests a spectrum countering the extra D^(a) below at dfactor 
;but it is soothing, so keeps the scale it check for the tests say it in paper SOS.
if (freqshiftchoice eq 1.0) then (  freqfactor=(ncalc/ng)^(2))  else  (  freqfactor=(ng/ng)^(2));i.e. ng/ng=1
;SO 290618 freqfactor is not FS on off, it is something else. alpha is already there in Db choice SOS

; 170816 to 1-gammac mas ta xalaei, ta loipa fainontai ok mexri stigmis. nailed it?


;120816 diereynisi
;print, max((ncalc/2.)^((1.-gammac)/2.), /NaN)  this run 0.00017258052
;print, max((ng/2.)^((1.-gammac)/2.), /NaN)  this run     0.00014427007

; 120816 look at this!! it is using gammac!!! but what exactly is this gammac! 
;preset to 1.8!!! should it be gammalorentz??? check!! 

;print, max(((1.-gammac)/2.), /NaN)  -0.400000
;print, min(((1.-gammac)/2.), /NaN)  -0.400000
 ;print,(((1.-gammac)/2.))   -0.40
 
 ;SOSO 120816 FROM HERE CONTINUE TO investigate SOS!!!
 

; 310815 we now assign dopplerboosting to the emission
;for now we keep density as a base for the emission coefficient
;ULTRA CLARIFY alphaindex PLUS OR MINUS? SUPER HERE!!!

;180816 looks like a plus to me now vs wiki pedia page for relbeaming
;180816 keep the old onef
;
;; here we get to perform the emission calculation. 
;if (dopplerchoice eq 1.0) then (  emiss_4d=rho_4d*((dopplerfactor_4d)^(2+alphaindex))*freqfactor  ) else (emiss_4d=rho_4d*freqfactor)
;180816 try out something else
;040718 here we set D^(2+a) for Cawthorn's case of opt. thin cont jet or opt. thick non-lapping blobs. Else, set D^(3+a) for normal case!
;if (dopplerchoice eq 1.0) then (  emiss_4d=rho_4d*((dopplerfactor_4d)^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=rho_4d*freqfactor);; THIS IS THE ONE USED so farSOS
;270718  we hereby check the values of dfactor  THE ABOVE IS THE CORRECT SOS
;; 270718 we may uncomment any of the following, in order to debug the dfactor, esp. when altering the phi1 ange above. phi1 MUST be NEG. and large, 
;if we are to achieve D boosting SOS
;if (dopplerchoice eq 1.0) then (  emiss_4d=((dopplerfactor_4d)^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)
;if (dopplerchoice eq 1.0) then (  emiss_4d=((   (gammalorentz_4d/dopplerfactor_4d)*coslosu_4d)^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)
;if (dopplerchoice eq 1.0) then (  emiss_4d=(   (1/(1-ccccc*coslosu_4d))^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)
;if (dopplerchoice eq 1.0) then (  emiss_4d=(   (gammalorentz_4d)^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)
;nxt is same as dfactor 
;if (dopplerchoice eq 1.0) then (  emiss_4d=(   (1/(gammalorentz_4d*(1-ccccc*coslosu_4d)))^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)
;if (dopplerchoice eq 1.0) then (  emiss_4d=(   (gammalorentz_4d)^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)

if (dopplerchoice eq 1.0) then (  emiss_4d=rho_4d*((dopplerfactor_4d)^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=rho_4d*freqfactor);; THIS IS THE ONE USED so farSOS


;(1-ccccc*coslosu_4d)
;120816 SUPER SOSARA EDO  this shows that something is wrong with the multipication factor really! 
;It should be much larger than one, not this smaller!!! WTF!

if (debug_comments eq 1) then  print, max( ((dopplerfactor_4d)^(2+alphaindex))*freqfactor,/NaN)
 if (debug_comments eq 1) then print, min( ((dopplerfactor_4d)^(2+alphaindex))*freqfactor,/NaN)
 if (debug_comments eq 1) then print, max(dopplerfactor_4d, /NaN)
if (debug_comments eq 1) then print, max(dopplerfactor_4d^(2+alphaindex), /NaN)
 if (debug_comments eq 1) then print, max( ((dopplerfactor_4d)^(2+alphaindex))*freqfactor,/NaN)
; 120816 ayto fenetai na exei to problima!!! ARA DES LIGO PIO PANO GIA DIEREYNISI
if (debug_comments eq 1) then print, max(freqfactor, /NaN)
if (debug_comments eq 1) then print, max( ((2+alphaindex))*freqfactor,/NaN)
; 4D-FY EVERYTHING, IN A NEAT MANNER SOS

;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.))
;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.))

;%OMIT GHZ-2 for the time being SOS

Temp_4d=(PRS_4d/RHO_4d)*10000000000000.0

; 050815 nobs=ndash*Dfactor therefore for each cell we have
;a different ndash=nobs/Dfactor (nobs is fixed for each run)
;therefore we calc ze radio emiss(e.g. thermal) at each cell
; this calc, each time it is done,
;is heavily dependent on the dfactor, i.e. on
;angle and velocity at that cell. IT IS 
;NOT A ONE OFF UNIVERSAL CALCULATION NOT AT ALL SOS


; 060815 interim variable for dopplerfactor = dopplerfactordivisor
;we set dfdivisor, for now, to a starter value 
;for the hj88 model, but assign it to 
;cell's dopplerfactor array value later on, before the main loop;
; assignements of emiss and kabs values are performed
;before the los loops. Everything is done 
;in array format before the loops.
;The loops merely draw a path through ready results.
dopplerfactordivisor=1.2
; 060815 nobs is set here once and for all, perhaps 
;move it to start of code as well 





;;ionization fraction is called xfrac
;xfrac=((T/1.0e+8)<1.0)
;more proof to the high densities of the disk:
;extended disk should give NO EMISSION IF COLD ENOUGH,
;NO MATTER HOW HIGH ITS DENSITY IS!!
xfrac=(((Temp_4d^3)/1.0e+14)<1.0)
gaunt=10.0*(1.0+0.1*alog((Temp_4d^(1.5))/ng))
gaunt=gaunt>0
;emiss is over volume to the third for volume NLONG etc
;-39 from formulae plus 42 from volume of cell makes plus 3 power of ten
;also over steradian
;e^(-hn/kt) is one for radio
;emiss times dV=dl*dl*dl
;times 0.01 for sr to the next cell SOS
;10^-39 from emissivity formula  times cell volume of 10^42 makes 10^3 thus the 10^3 factor here SOS
;last 10^-6 is the solid angle of a los this is to be altered for HIGHER RESOLUTION SOS
;0208156 move to file or to beginning this param
lossolidangle=0.000001
; now we put in ze gamma ray emission coefficient
;emiss=emiss*0.0
;emiss=5.44*(1.0e+3)*((100.0/NLONG)^3)*RHO*RHO*(T^(-0.5))*gaunt*(xfrac^2)*lossolidangle
;emiss=RHO
;emiss=emiss*0.0

;020815 SUPER consider moving here the D. boosting stuff? or check anyway SOS

;gammarayemissiongromresultvectorfinal,coming from the mathematica code
emiss=resultvectorfinal/(1000000000000000000000000000000.0)
;emiss=T/RHO
;emiss=0.0*emiss
;absorption coefficient


;kabs=cp6(0.)*ndens*(bfield*sin(theta))^((gammac+2.)/2.)*((ng/2.)/cp1(0.))^(-(gammac+4.)/2.)
; kabs is over length, not over volume, like emiss!!)^14 cm is pluto code unit for length 100/nlong times
;that is length of los thru this cell
;kabs times dl

; we set ll equal to 1.5 to account for angles diagonal through cell
;so here leave (100/nlong) OK for length
;and it works ok for cell volume too, which has nothing to do with the LOS angle!

; we set the gamma ray absorption coefficient to zero
; 060815 ng is also a 4d array now BUT ng is only assigned along the LOS the rest is wrong
;so we must move these assignements inside the LOS loop, but only as correctuons to
;their presently assigned values
;within the loop we select and correct
;only way to do it efficiently,
;if there must be a loop 
kabs_4d=0.018*gaunt*RHO_4d*RHO_4d*(1/ng)*(1/ng)*(Temp_4d^(-1.5))*(100.0/NLONG)*(1.0e+14)*(xfrac^2)
kabs2_4d=0.08235*RHO_4d*RHO_4d*(T^(-1.35))*0.01*(((Temp_4d/1.0e+8)>1.0)^2)*(100.0/NLONG)
kabs_4d=0.0*RHO_4d
kabs2_4d=0.0*RHO_4d
;kabs
; HERE PUT IN VALUE FOR KAPPA ABS COEFFICIENT
; DO IT!
; 190518 HERE ADD KAPPA ABS BASED ON REYNOSO ROMERO 2008, eqn 16, based on atoyan drmer 2003: also from our paper 2014 
;Note: no optical depth here: It is meaningless without a LOS, and a LOS is only for a small line throught the grid, not all over it!
;an array to enable picking out the los points within the 3D grid
losdraw=dblarr(nx_1+6,ny1+6, nz1+6)
;print, 'tracker pos 8'
;loop along the LOS, up to a maximum size of grid diagonal, or till you reach either side of grid(in the latter case, exit with break condition).
;1st calc of ratios before loop, for optimization!
;use real numbers for ratios.
ratio1c=(double(uc)/double(rc))

;print, 'tracker pos 9'
a1=double(cc)
b1=double(rc)
c1=double(uc)
d1=sqrt(b1*b1+c1*c1)
e1=a1/d1
ratio2c=e1
;print, 'tracker pos 10'
;this bit adjusts the corrsponding kength along the los.
;the formulae are:
;dll(counter)=dlup*sin(phi1)*cos(phi2) if up(y)
;=dlright*cos(phi1)*cos(phi2) if right(x)
;=dlclimb*sin(phi2) if climb(z)
;phi1=goes from x to y
;phi2=goes from xy plane to z
;define array of lrnghts along the los each element is the correspondig dl for that grid increment along the los.
ll=dblarr(nx_1+ny1+nz1+5)
;define 3 separate scales for 3 axes. They are the same for now, else we must change the angles sin and cod and tan definitios.
;For now, strictly same scales for x, y, z.


;NOT FOR PLUTO GRID SOS! 35/250*100/750*35/250
;i.e. smaller cell side along jet

;SUPER 301217 WE ADD SCALING FACTOR SFACTOR TO THE dlc, dlu, dlr cell lengths (in cell length units).




sf1=sin(phi1)
sf2=sin(phi2)
cf1=cos(phi1)
cf2=cos(phi2)
nx10=2

;161218 FROM HERE BEGINS DIFFERENTIATION BETWEEN xz AND yz VERSIONS.
;***********************************************************

eikona=dblarr(ny1+4,nz1+4)
eikonatau=dblarr(ny1+4,nz1+4)
;array to hold the number of voxels of each LOS. This is different foe LOS's near the edges of the grid, at low LOS angles.
; LLOS/counterlast=length per voxel=ll, where counterlast=counter at los exit
counterlast=dblarr(ny1+4,nz1+4)
;array to hold los length as calculated from difference in position from end and start of LOS, according to the formula:
; LLOS=sqrt(((nx-nxo)^2)+((y-y0)^2)+9z-z0)^2))

loslength=dblarr(ny1+4,nz1+4)

;print, 'xe xe '









; according to idl manual, inner part of the loop must be outer part of
;the array indices, in order to access the arrays as they are stored in
;memory. This goes for 2d arrays, we try it 4 3d here?
for nz10=1,nz1 do begin
for ny10=1,ny1 do begin
 
;print, 'ok os edo 1'
nx1current=nx10
ny1current=ny10
nz1current=nz10


uc=0
; current 'right' step counter begins at 1, i.e. one first step right, saves div by 0! We presume that it is intrinsically
;accounted for in the loops later on in the code.
rc=1
;similar for current 'climb' counter, z-dimension. (NOTE: This could be made to begin at zero, had we wished to do so.(SOS: AND HAD ACCOUNTED FOR IT IN THE LOOPS)
cc=0
;Initialize current ratio of up to right steps, at zero (i.e. (uc/rc)=0).
;This way, 2nd step is always up (first was right), as it has to increase ratioc up from zero, in the if construct.
;similarly for second tan(phi2) ratio2c
ratio1c=0.0
ratio2c=0.0

;print, 'ok os edo 2'
; before every LOS integration, reset los intensity, in, back to zero
in=0.0
;reset optical depth as well!
taun=0.0
; HERE SET t0LOS when the LOS begins (time of begining snapshot sos).  SHOULD DO IT SYSTEMTICALLY
t0los=0.0
;for counter=1,(nx_1+ny1+nz1),2  do begin
; we shrunk the loop length from the sum of the 3 sides
;of the parallelepiped to the max length of a line within the box.
; 050515 saves up to 40 percent!!!  should have done ages ago!! relic from
;quick calc survived so long unimproved!
for counter=1,(sqrt(nx_1*nx_1 + ny1*ny1 + nz1*nz1) + 1),2  do begin
if (debug_comments eq 1) then print, 'counter arxi inner loop ', counter,' tsa'
;sqrt(nx_1*nx_1 + ny1*ny1 + nz1*nz1) + 1

;print, 'ok os edo 3'
;model grid size:x,y,zprint, za1, za2, za3, za4, za5, za6, rz, ((xind-x1)^2+(yind-y1)^2)



;print, 'tsa1', counter
;temp decr counter to do 1st grid step
counter=counter-1
if (debug_comments eq 1) then print, 'counter after -1 ',counter
;print, 'tsa2', counter
ratio1c=(double(uc)/double(rc))
; HERE MUST RESET rc,uc,cc to initial values,
;else they retain old values, before breaking out of the inner loop,
;and they mess our loops


a1=double(cc)
b1=double(rc)
c1=double(uc)
d1=sqrt(b1*b1+c1*c1)
e1=a1/d1
ratio2c=e1

if (ratio1c lt ratio1f) then (uc=uc+1)  else (rc=rc+1)
;This was WRONG!! must divide and then limit it CUT it use approximate ll =1.5 times
;cell side length and assume equal cell sizes x, y, z SOS
;if (ratio1c lt ratio1f) then (ll(counter)=dlu*sf1*cf2)  else (ll(counter)=dlr*cf1*cf2)
ll(counter)=1.0

ratio1c=(double(uc)/double(rc))
if (debug_comments eq 1) then print, ' REENACTED THAT FOR LARGE ANGLES 140818', rc, uc, cc, ratio1c, ratio2c, counter, ll(counter)
if (debug_comments eq 1) then print, '140818,ratio1c,ratio1f,ratio2c,ratio2f', ratio1c, ratio1f, ratio2c, ratio2f
;update LOS arrays.
;Basically, every time we advance one grid point, we update the LOS arrays,
;in order to properly keep track of all LOS points.
;So we do this twice, as we have two ifs, each of which definately advances the LOS by exactly one grid point.

;find where we are in the 3D grid
;calculate current x-position in the grid
nx1current=nx10+rc
;same for current y-pos in the 3D grid
ny1current=ny10+uc
;same for z
nz1current=nz10+cc

; edo repeat the calc of angle and dboosting check

; 060515 we finally add aberration stuff here for rlos

;declare vars for calculating angle between LOS direction in 3D space and
;local velocity direction.

xx0=nx10
yy0=ny10
zz0=nz10


;we assign the following, in order to avoid confusions from using x,y,z
;which are very simple and might have been used elsewere in the code
;xx=x
;yy=y
;zz=z
;SUPER 25-05-15
;DIORTHOSI xx=xx0+rc, klp y, z. OSTE x-x0=rc=Dx. Allios bgazei o,ti na'nai arnitika
; SIMEIOSEIS LATHOS TO EIXAN SOS
xx=rc+xx0
yy=uc+yy0
zz=cc+zz0

; 040815 WE m
;just implement frequency shift, aka 1st model of hj88 from back in the day
;in order to achieve that, we first need to re-implement 
;the radio emission stuff from paper1, e.g. thermal 


; 040815 moved this bit a tad earlier in order to calculate shotnumber before 
;ze assignements of dopplerboosting related stuff 


;determine time shot
;dtllos=(dl/clight)
lloscurrent=sqrt( ((dlc*(nz1current-nz10))^2)+((dlu*(ny1current-ny10))^2)+((dlr*(nx1current-nx10))^2) )
if (debug_comments eq 1) then print, lloscurrent, 'proto deytero trito meros entos rizas llos', ((dlc*(nz1current-nz10))^2),((dlu*(ny1current-ny10))^2), ((dlr*(nx1current-nx10))^2) 
;SUPER SOS: curtime, begins when tlos=0, NOT when T=0 (in t array). MUST account for ze difference SOS!
; NEED TO UPDATE clight to cell code units, else it means cgs = a pluto cell or a LOS code cell! DO IT SOS! LLOSCURRENT IS NOT
;IN CGS IT IS IN LOS CODE CELLS SOS

; 260514 CHECK time units used here vs PLUTO time units used in the dbl.out file (and in the code of course)
;in current dbl.out, there are 21 snapshots and max time is less than 0.5
;SUPER 070915 we must initialize the los time 
;at when it begins, i.e. t(shotmin).time of the first snapshot else it resets to zero
;and we cannot catch the time delays along the los t0LOS
t0LOS=t(shotmin)
;ultra 090915 here is key determining calculation of current time
;along the los. so far, llos is in 10^10 cm, i.e. in pluto l units
;furthermore, clight is in adjusted for shrink sfactor cells per second 
;(not per time unit of pluto) ultra re-check this point sosara!!
;t0los should be in pluto time units.
;so check in what time units is actually the calculation of clight.
;pluto time units, or seconds? sos
 
curtime=lloscurrent/clight+t0LOS

; 050815 check if t0los should be somenthing else at 
;the beginning of the LOS. but perhaps not SOS

; 050815 the following curtimepluto thing is based on shotind
;but shotind is only used in the initial assignement loop
;for temporal data array 
;shotind should not be used any more in the code correct it here.
;shotnumber should be used in its place sos

; 050815 what is the meaning of curtimepluto? none! it is not used anywhere! 
;just a leftover it seems

;curtimepluto=t(shotind)

;280514
;tlos to tpluto, i.e. from sec to pluto time units, which are here 0.333 sec
; tlos in sec tpluto in 0.333 sec from units

; 070915 we deal in secs from pluto, not in 'pluto time units=0.333sec'. Therefore 
;we revert back to factort=1!!

; we uncomment this in order to avoid using factort=0.333 for time units, 
;stick with cgs units for now 

;080915  SUPER pluto mNuAL says time units are in L0/U0 units, therefore 
;we should use factort indeed! it is not in cgs seconds, it is in 
;pluto units, which in our case (see init.c for units) ends up 1/3 of a sec.


;080915b Above definition of curtime os based on clight and on pluto time index
;it is therefore in pluto time units already, as both its terms are
;in such units. No need for factort here it seems. need to recheck, though SOS
;curtime=curtime*factort


; 070915 NA ELEGXTHEI o xronos shotnumber vs count etc
;SOSARA! fix it


aaaa=where((curtime-t)<0,count)
;print, where((curtime-t)<0,count)
if (debug_comments eq 1) then print, size(t, /n_elements)
if (debug_comments eq 1) then print,'testing',count,curtime
;next is snapshot number, give or take 1, for a current time of curtime.
if (debug_comments eq 1) then print, size(t, /n_elements)-count
;ultra 080915 we add minus shotmin here, else it counts shotnumber
;over the whole time since the beginning of the sim,
;while we just want since the beginning of the LOS, i.e. -shotmin

; 110915 check shotnumber in time pluto units 
;is it advancing too slow perhap[s? check!!
;counter means how many loop steps, NOT count, count is ok!! see above!!
;but each loop step
;los advances by clight, which is not 1 in general sos!!
shotnumber=size(t, /n_elements)-count-shotmin
;080915 check length llos units, and time units vs sfactor etc
if (debug_comments eq 1) then print, clight, count, shotnumber, curtime,lloscurrent, 'tsikabum',counter, nx10,nz10,nx1current,ny1current,nz1current, plutocelllength, lcell
; factors, factort
if (debug_comments eq 1) then print, 'shotind in curtime definition,curtime,shotnumber',shotind,curtime,shotnumber
if (debug_comments eq 1) then print, 'los time  ', 't0los',lloscurrent/clight,t0LOS
if (debug_comments eq 1) then print, 'counter ',counter
if (debug_comments eq 1) then print, 'nx10 ny10 nz10 ',nx10, ny10,nz10
if (debug_comments eq 1) then print, 'nx1 nx_1 nx2 nx3 ',nx1,nx_1, nx2,nx3
if (debug_comments eq 1) then print, 'nx1current ny1current nz1current ', nx1current, ny1current, nz1current
if (debug_comments eq 1) then print, 'lloscurrent calced', sqrt( ((dlc*(nz1current-nz10))^2)+((dlu*(ny1current-ny10))^2)+((dlr*(nx1current-nx10))^2) )







;We now select and assign the suitable values of time tagged velocity
;(kind of 4D, probably not in the usual relativistic 4D sense)
;from the 4D array to the convenience vars ux,uy,uz
;in order to facilitate the (los,u) angle cos calculation
;We made sure this comes AFTER assignement of nx(i)current above.
;Else it doesn't work correctly!!

;ux=vx1_4d(shotnumber,nx1current,ny1current,nz1current)
;uy=vx2_4d(shotnumber,nx1current,ny1current,nz1current)
;uz=vx3_4d(shotnumber,nx1current,ny1current,nz1current)



;The following formula is used, from analytic geometry
;cos(los,u)=A/B, where
;A=[(xx-xx0)*ux+(yy-yy0)*uy+(zz-zz0)*uz]
;B=sqrt[(xx-xx0)^2 +(yy-yy0)^2 +(zz-zz0)^2 ]*sqrt[ux^2+uy^2+uz^2]

;aaa=(xx-xx0)*ux+(yy-yy0)*uy+(zz-zz0)*uz
;bbb=sqrt((xx-xx0)*(xx-xx0) +(yy-yy0)*(yy-yy0) +(zz-zz0)*(zz-zz0) )
;ccc=sqrt(ux*ux+uy*uy+uz*uz)
; 030815 this algo siimply calculates the coslosu angle according tocurent position
;along the los it does not use the original los aiming angle.
;it merly relies on the los aiming algo to do its job and then picks up on the latter's 
;result 
;coslosu=aaa/(bbb*ccc)
;310715 we define theta and also doppler boosting andf rel. beaming stuff
;also lorenrz factor
;bear in mind need to recheck left right oriented 3d xyz systems.
;thetau=acos(coslosu)
; DOUBLE CHECK IF ccc is indeed the relativistic velocity over c

; 220815 we commented out these two lines since we now use array operations
;gammalorentz(shotnumber,nx1current,ny1current,nz1current)=1/(sqrt(1-ccc*ccc))
;dopplerfactor(shotnumber,nx1current,ny1current,nz1current)=(sqrt(1-ccc*ccc))/(1-ccc*coslosu)
; check the above functions


;frequency

;SUPER 060815 here is the observing frequency
;consider moving it to a file ot to the beginning of the code,
;along with other params.
;in the relativistic version of the code
;where dopplerfactor is employed, ndash=nobs/dopplerfactor SOS
; set it up to change for each cell according to cell's 
;local dopplerfactor

;ng(shotnumber,nx1current,ny1current,nz1current)=nobs*(1/dopplerfactor(shotnumber,nx1current,ny1current,nz1current))
;ntesty=nobs*(1/dopplerfactor(shotnumber,nx1current,ny1current,nz1current))
;220815 we device a little test here in order to compare the array with the loop result for frequency SOS
;print, 'freq ntesty ',ng(shotnumber,nx1current,ny1current,nz1current),'dopplerfactor',dopplerfactor(shotnumber,nx1current,ny1current,nz1current), ntesty

;SUPER 070815 THE FOLLOWING DO WORK WE SET THEM OFF TEMPORARILY FOR EFFICIENCY
;WHILE DEVELOPING IT IS A SWITCH NOTHING MORE
;CASE emisselect OF 
;  1: PRINT, 'one' 
;  2: PRINT, 'two' 
;  3: PRINT, 'three' 
;  4: PRINT, 'four' 
;ENDCASE 
;kabs_4d(shotnumber,nx1current,ny1current,nz1current)=0.018*gaunt(shotnumber,nx1current,ny1current,nz1current)*RHO_4d(shotnumber,nx1current,ny1current,nz1current)*RHO_4d(shotnumber,nx1current,ny1current,nz1current)*(1/ng(shotnumber,nx1current,ny1current,nz1current))*(1/ng(shotnumber,nx1current,ny1current,nz1current))*(Temp_4d(shotnumber,nx1current,ny1current,nz1current)^(-1.5))*(100.0/NLONG)*(1.0e+14)*(xfrac(shotnumber,nx1current,ny1current,nz1current)^2)
;kabs2_4d=0.08235*RHO_4d*RHO_4d*(T^(-1.35))*0.01*(((Temp_4d(shotnumber,nx1current,ny1current,nz1current)/1.0e+8)>1.0)^2)*(100.0/NLONG)

; 070815 ALSO emiss have to be implemented 
;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.)
;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.)

;OR WAIT TILL WE DO  IT IN NEW GEOMETRIC FASHION USING
;RIGINAL DIRECTION ANGLES INSTEAD OF CURRENT LOS
;ANGLES 

;print, 'aaa,bbb,ccc',aaa,bbb,ccc
;print, 'edo ta ux,uy,uz,(xx-xx0),(yy-yy0),(zz-zz0),coslosu, gonia (los,u);
;print, ux,uy,uz,(xx-xx0),(yy-yy0),(zz-zz0), coslosu,acos(coslosu), thetau
;print,'xx,yy,zz,xx0,yy0,zz0', xx, yy, zz, xx0, yy0, zz0
; NA SYNEXISTEI!! 060515 OS EDO CODED
if (debug_comments eq 1) then print,'nx10,ny10,nz10', nx10,ny10,nz10
;print, 'ux^2,uy^2,uz^2',ux*ux,uy*uy,uz*uz
if (debug_comments eq 1) then print, ' mipos EINAI ARISTEROSTROFO to XYZ, px tis LOS? CHECK IN PRACTICE AGAIN SOS'
if (debug_comments eq 1) then print,'210515'
;print, 'gammalorentz',gammalorentz(shotnumber,nx1current,ny1current,nz1current)
;print, 'dopplerfactor',dopplerfactor(shotnumber,nx1current,ny1current,nz1current)
if (debug_comments eq 1) then print, 'el stoppo 190515'
if (debug_comments eq 1) then print, ' shotind vs shotnumber 020815 check before adding dopplerboosting factor SOS;
;SUPER 030815 fix shotnumber in lieu of shotind in recent dboosting and gamalorentz calculations 
;directly above DO NOT FORGET IT SOS
;test also by altering phi1, phi2 
; 030815try gathering althose params into one place with corresponding descriptions do it as welll.

;stop
; WE MAY REPEAT shotnumber check once more at second in-loop increment, for improved time -accuracy SOS
;140515 we shall not repeat shotnumber check since shots are
;much fewer than los voxels.

;IMPORTANT: FOLLOWING STUFF MUST GO AFTER INITIAL USE OF nx1current, ny1current, nz1current
;SUPERSOS
;next comes ALGO to find snapshot position, given current time curtime
;to be put suitably inside ze code, at algo location SOS



;SUPERSOS
;next comes ALGO to find snapshot position, given current time curtime
;to be put suitably inside ze code, at algo location SOS
;curtime=40
;print, where((curtime-t)<0,count)
;print, size(t, /n_elements)
;next is snapshot number, give or take 1, for a current time of curtime.
;print, size(t, /n_elements)-count




;now that we know where we are in the 3D grid, assign to the current los point, the e and k values
;from the corresponding point of the 3D grid
; 050815 we now introduce the Dfactor emission boost/de-boost SOS
lose(counter)=emiss_4d(shotnumber,nx1current,ny1current,nz1current)
; temporarily disable dfactor.also consider doing it array oriented
;using ratio1, ratio2 instead of coslosu
;lose(counter)=emiss_4d(shotnumber,nx1current,ny1current,nz1current)*((dopplerfactor(shotnumber,nx1current,ny1current,nz1current))^(2+alphaindex))
; k must be between 0 and 1, as it is an abs coefficient sosara!
losk(counter)=kabs_4d(shotnumber,nx1current,ny1current,nz1current)
losdraw(nx1current,ny1current,nz1current)=1.0
;dIn=-kn*dl+en
din(counter)=-losk(counter)*ll(counter)*in+lose(counter)*ll(counter)
;in cgs the length!!!
; 301217 we now multiply dlu, dlr, dlc by sfactor, in order to make up for less and larger cells when 
;shrinking the grid. But these do NOT affect the following mikoscell etc. mikoscell is set to 1 earlier, 
; no duplicate sfactor there.
dtaun(counter)=losk(counter)*ll(counter)*mikoscell
if (debug_comments eq 1) then print, 'EDOORETASO',lose(counter),losk(counter),ll(counter),mikoscell
in=in+din(counter)
incurrent(counter)=in
taun=taun+dtaun(counter)
tauncurrent(counter)=taun

;print, rc, uc, cc, ratio1c, ratio2c
;exit loop on reaching either end of 3D grid

;print, 'PTPTPTP', 'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc
if (rc gt (nx_1-nx10)) then break
;print, 'ssstdfklhv', 'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx_1-nx10)),'rc',rc,'uc',uc,'cc',cc
if (uc gt (ny1-ny10)) then break
;print, 'TTTTTT',  'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc
if (cc gt (nz1-nz10)) then break
;print, 'BINNN',  'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc
;restore counter to +2 value for second increment of grid. This way, we got all two grid
;increments recorded in counter.
;print, 'tsa3', counter
counter=counter +1
if (debug_comments eq 1) then print, 'counter meta to syn 1 ', counter 
;print, 'tsa4', counter

if (ratio2c lt ratio2f) then (cc=cc+1)  else if (ratio1c lt ratio1f) then (uc=uc+1)  else (rc=rc+1)

;likewise, this was wrong by a factor of two
;if (ratio2c lt ratio2f) then (ll(counter)=dlc*sf2)  else if (ratio1c lt ratio1f) then (ll(counter)=dlu*sf1*cf2)  else (ll(counter)=dlr*cf1*cf2)
ll(counter)=1.0

ratio1c=(double(uc)/double(rc))

a1=double(cc)
b1=double(rc)
c1=double(uc)
d1=sqrt(b1*b1+c1*c1)
e1=a1/d1
ratio2c=e1







;find where we are in the 3D grid
;calculate current x-position in the grid
nx1current=nx10+rc
;same for current y-pos in the 3D grid
ny1current=ny10+uc
;same for z
nz1current=nz10+cc


;FROM HERE D. BOOSTING
; 140515 we ALSO add SECOND aberration stuff here for rlos
;REASON is that each and every cell must have its very own Doppler boosting factor
;SO we need to calculate D. boosting for the second counter loop as well
;(2nd step of pathfinding in 3D)

;declare vars for calculating angle between LOS direction in 3D space and
;local velocity direction.

xx0=nx10
yy0=ny10
zz0=nz10


;we assign the following, in order to avoid confusions from using x,y,z
;which are very simple and might have been used elsewere in the code
;xx=x
;yy=y
;zz=z

xx=rc
yy=uc
zz=cc

;We now select and assign the suitable values of time tagged velocity
;(kind of 4D, probably not in the usual relativistic 4D sense)
;from the 4D array to the convenience vars ux,uy,uz
;in order to facilitate the (los,u) angle cos calculation
;We made sure this comes AFTER assignement of nx(i)current above.
;Else it doesn't work correctly!!


;020815 may be shotnumber, not shotind here check sosara
;ux=vx1_4d(shotnumber,nx1current,ny1current,nz1current)
;uy=vx2_4d(shotnumber,nx1current,ny1current,nz1current)
;uz=vx3_4d(shotnumber,nx1current,ny1current,nz1current)



;The following formula is used, from analytic geometry
;cos(los,u)=A/B, where
;A=[(xx-xx0)*ux+(yy-yy0)*uy+(zz-zz0)*uz]
;B=sqrt[(xx-xx0)^2 +(yy-yy0)^2 +(zz-zz0)^2 ]*sqrt[ux^2+uy^2+uz^2]


;aaa=(xx-xx0)*ux+(yy-yy0)*uy+(zz-zz0)*uz
;bbb=sqrt((xx-xx0)*(xx-xx0) +(yy-yy0)*(yy-yy0) +(zz-zz0)*(zz-zz0) )
;ccc=sqrt(ux*ux+uy*uy+uz*uz)
;coslosu=aaa/(bbb*ccc)
;310715 we define theta and also doppler boosting andf rel. beaming stuff
;also lorenrz factor
;bear in mind need to recheck left right oriented 3d xyz systems.
;thetau=acos(coslosu)
; DOUBLE CHECK IF ccc is indeed the relativistic velocity over c
;020815 may be shotnumber, not shotind here check sosara
;gammalorentz(shotnumber,nx1current,ny1current,nz1current)=1/(sqrt(1-ccc*ccc))
;dopplerfactor(shotnumber,nx1current,ny1current,nz1current)=(sqrt(1-ccc*ccc))/(1-ccc*coslosu)
; check the above functions


;ng(shotnumber,nx1current,ny1current,nz1current)=nobs*(1/dopplerfactor(shotnumber,nx1current,ny1current,nz1current))
; 2208145 this now comes from the array op before the loop, not from the in-loop calculation SOS
;print, 'freq',ng(shotnumber,nx1current,ny1current,nz1current),'dopplerfactor',dopplerfactor(shotnumber,nx1current,ny1current,nz1current)
; 070815 WE SET TEMPORARILY THOSE TO ZERO 
;FOR EFFICIENCY WHILE TESTING THE CODE GEOMETRICALLY
;MUST RESTORE LATER SOS
;EVEN BETTER, PUT IN A CASE SWITCH TO SELECT WETHER TO
;USE THESE OR NOT
;ALSO FINALLY DO THE ARRAY THING, DOPPLER =F(LOS ANGLE)
;NOT F(CURRENT ANGLE)
;kabs_4d(shotnumber,nx1current,ny1current,nz1current)=0.018*gaunt(shotnumber,nx1current,ny1current,nz1current)*RHO_4d(shotnumber,nx1current,ny1current,nz1current)*RHO_4d(shotnumber,nx1current,ny1current,nz1current)*(1/ng(shotnumber,nx1current,ny1current,nz1current))*(1/ng(shotnumber,nx1current,ny1current,nz1current))*(Temp_4d(shotnumber,nx1current,ny1current,nz1current)^(-1.5))*(100.0/NLONG)*(1.0e+14)*(xfrac(shotnumber,nx1current,ny1current,nz1current)^2)
;kabs2_4d=0.08235*RHO_4d*RHO_4d*(T^(-1.35))*0.01*(((Temp_4d(shotnumber,nx1current,ny1current,nz1current)/1.0e+8)>1.0)^2)*(100.0/NLONG)


; NA SYNEXISTEI!! 060515 OS EDO CODED

; WE MAY REPEAT shotnumber check once more at second in-loop increment, for improved time -accuracy SOS

; TILL HERE D. Boosting 2nd pathfinding loop part


;now that we know where we are in the 3D grid, assign to the current los point, the e and k values
;from the corresponding point of the 3D grid
; 050815 we now multiply by the dopplerfactor to account for D-boost/de-boost SOS
; temporarily disable dfactor.also consider doing it array oriented
;using ratio1, ratio2 instead of coslosu
;lose(counter)=emiss_4d(shotnumber,nx1current,ny1current,nz1current)*((dopplerfactor(shotnumber,nx1current,ny1current,nz1current))^(2+alphaindex))
lose(counter)=emiss_4d(shotnumber,nx1current,ny1current,nz1current)
;k must be between 0 and 1 sos
losk(counter)=kabs_4d(shotnumber,nx1current,ny1current,nz1current)
losdraw(nx1current,ny1current,nz1current)=1.0
;din=-k*dl*in +e*dl


din(counter)=-losk(counter)*ll(counter)*in+lose(counter)*ll(counter)
;in cgs the length!!!

dtaun(counter)=losk(counter)*ll(counter)*mikoscell

if (debug_comments eq 1) then print, 'EDOORETASO2',lose(counter),losk(counter),ll(counter),mikoscell

in=in+din(counter)
incurrent(counter)=in
taun=taun+dtaun(counter)
tauncurrent(counter)=taun
if (debug_comments eq 1) then print, 'EDO TO IN', in, din(counter), taun
;print, rc, uc, cc, ratio1c, ratio2c, counter, ll(counter)
;exit loop on reaching either end of 3D grid
k123=2.*p*(gammac+2.)/3.
;print, 'PTPTPTP', 'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc
;print, 'edo radstuff','��',in,'din',din(counter), $
;-losk(counter),ll(counter),lose(counter),'kabs',kabs(nx1current,ny1current,nz1current),'emiss',emiss(nx1current,ny1current,nz1current), $
;ndens(nx1current,ny1current,nz1current),(((nx1current-x1m)^2+(ny1current-y1m)^2) lt rz*rz)*tau1*(1./((nz1current*deltaz)^k123)), $
;'aloha',(((nx1current-x1m)^2+(ny1current-y1m)^2) lt rz*rz),rz,x1m,y1m,'dallas', $
;(((nx1current-x1m)^2+(ny1current-y1m)^2) lt rz*rz), 'bfield', $
;Bfield(nx1current,ny1current,nz1current),(((nx1current-x1m)^2+(ny1current-y1m)^2) lt rz*rz), $
;lamda1*(1./((nz1current*deltaz)^p)), $
;'emissbits',cp5(0.),ndens(nx1current,ny1current,nz1current),(bfield(nx1current,ny1current,nz1current)*$
;sin(theta))^((gammac+1.)/2.),((ng/2.)/cp1(0.))^((1.-gammac)/2.), $
;'absbits',cp6(0.)*ndens(nx1current,ny1current,nz1current),$
;(bfield(nx1current,ny1current,nz1current)*sin(theta))^((gammac+2.)/2.),$
;((ng/2.)/cp1(0.))^(-(gammac+4.)/2.)

; check emissbits, absbits vs model
;last abs bit is zero?? why?
lastcounter=counter
counterlast(ny10,nz10)=lastcounter
nx1currentlast=nx1current
ny1currentlast=ny1current
nz1currentlast=nz1current
if (rc gt (nx_1-nx10)) then break
;print, 'ssstdfklhv', 'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx_1-nx10)),'rc',rc,'uc',uc,'cc',cc
if (uc gt (ny1-ny10)) then break
;print, 'TTTTTT',  'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc
if (cc gt (nz1-nz10)) then break
if (debug_comments eq 1) then print, 'BINNN',  'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc

if (debug_comments eq 1) then print, 'counter prin telos inner loop los ', counter

endfor
llos=sqrt( ((dlc*(nz1currentlast-nz10))^2)+((dlu*(ny1currentlast-ny10))^2)+((dlr*(nx1currentlast-nx10))^2) )

if (debug_comments eq 1) then print, ny10, nz10,'tsa'
if (debug_comments eq 1) then print, ny1current, nz1current,'tsa'
if (debug_comments eq 1) then print, counterlast(ny10,nz10),'edo to counterlast'
if (debug_comments eq 1) then print, llos, 'edo to llos'
loslength(ny10,nz10)=(counterlast(ny10,nz10)/llos)
in=in/loslength(ny10,nz10)
if (debug_comments eq 1) then print, in, 'INEDOORESOS
if (debug_comments eq 1) then print, taun
eikona(ny10,nz10)=in
if (debug_comments eq 1) then print, in,eikona(ny10,nz10), 'teos'
eikonatau(ny10,nz10)=taun
counterlast(ny10,nz10)=lastcounter

openw, 16, datapath+'\databis\eikona1.eikona'
if (debug_comments eq 1) then printf, 16, ny10,nz10,eikona
close, 16

endfor

endfor
;print, ratio1f, ratio2f












;this plots slices of bfield along the jet axis, thus displaying cross
;sections of the cone. Helps see where things are in 3D. For e.g. the bent jet stuff, etc.
!P.MULTI=[0,2,3]
!P.NOERASE=1

;here plot either ndens or bfield
;it works ok for bfield only so far
;for tttt=10, 40, 10 do begin
;tvscl, emiss(*,*,tttt), (tttt/10-1)
;endfor

;tvscl, emiss(*,nx_1/2,*)
!P.MULTI = 0
!P.NOERASE=0
;print, ll
;print, din
; should en aborption be proportional to ecurrent intensity? no negs should be possible for intensity!! Check again
;eqn of rad transfer, quietly !
;tvscl wont work in 3D: try 2dslices, or 3d drawing instead
eikonaread=dblarr(ny1+4,nz1+4)

;161218TILL HERE DIFFERENTIATION BETWEEN xz AND yz VERSIONS.

jetprofile=dblarr(nz1+4)
;openr, 16, 'f:eikonafile.txt'
;readf, 16, ny10,nz10,eikonaread
;close, 16
;tvscl, eikonaread
jetprofile=TOTAL(eikonaread, 1)
indexed=dindgen(nz1+4)
indexedmas=double(nz1+4)
indexedmas=indexed*deltazmas

eikona2=eikona
plot, indexedmas, jetprofile*1e+18,xtitle='z(mas)', ytitle='Flux density per beam'
if (debug_comments eq 1) then print,'total flux in SI *10^(-18):', total(eikonaread), total(jetprofile)
;plot, jetprofile*1e+18



;surface, alog(eikona^0.333), ax=75
;surface, ((eikona)), ax=45, az=-190, MAX_VALUE=30, MIN_value=1
;surface, ((eikona)), ax=55, az=-190, /zlog, MAX_VALUE=10, MIN_value=0.0001, xstyle=2, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2

;shade_surf, ((eikona)),  ax=55, az=-190, /zlog, MAX_VALUE=10, MIN_value=0.0001, xstyle=4, $
;ystyle=4, zstyle=4, /NOERASE, background=255, color=0
;surface, ((eikonatau)), ax=55, az=-190, /zlog, MAX_VALUE=10, MIN_value=0.0001
;surface, ((eikonatau)), ax=20, az=-170, /zlog, MAX_VALUE=0.00001, MIN_value=0.00000000000000001, xstyle=4, $
;xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0

;openw, 26, 'C:\code\code\eikonafilehighres.txt'
;printf, 26, eikona
;close, 26
;openr, 26, 'C:\code\code\eikonafiletauhighres.txt'
;readf, 26, eikonatau
;close, 26
;surface, ((eikonatau)), ax=20, az=-170, /zlog, MAX_VALUE=0.00001, MIN_value=0.00000000000000001
;shade_surf, ((eikonatau)), ax=20, az=-170, /zlog, MAX_VALUE=0.00001, MIN_value=0.00000000000000001, xstyle=8, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2



;surface, ((eikonatau)), ax=20, az=-170, /zlog, MAX_VALUE=0.00001, MIN_value=0.00000000000000001, xstyle=8, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2
; edo to teleytaio sxima SOS
;surface, ((eikona)), ax=35, az=-150, /zlog, MAX_VALUE=100, MIN_value=0.01, xstyle=2, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2


;surface, ((eikonatau)), ax=20, az=-160, /zlog, MAX_VALUE=0.00001, MIN_value=0.000000000000001, xstyle=8, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2

;surface, ((eikonatau)), ax=20, az=-170, /zlog, MAX_VALUE=0.00001, MIN_value=0.00000000000000001, xstyle=8, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2
; edo to teleytaio sxima SOS
;surface, ((eikona)), ax=35, az=-150, /zlog, MAX_VALUE=100, MIN_VALUE=0.01, xstyle=2, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2

;
;********************************************************************************
;211218 In the following we annotate the plot with some of the parameters, 
;in a formatted output mode.
shade_surf, ((eikona)), charsize=1.5, ax=35, az=-150, PIXELS=1024, /zlog, MAX_VALUE=100000000000000, MIN_VALUE=100000, xstyle=2, $
ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
ythick=2, zthick=2, xcharsize=3, ycharsize=3, zcharsize=3.5
 if (debug_comments eq 1) then print, 'total jet gamma ray intensity, yet unnormalized,',total(eikona, /NaN)
  if (debug_comments eq 1) then print, 'max element excluding NaNs', max(eikona, /NaN)

;here some interim vars to trim up things
clight_temp=string(clight, format='(E19.3)')
clight_temp_trim=strtrim(clight_temp,2)

sfactor_temp=string(sfactor, format='(E19.3)')
sfactor_temp_trim=strtrim(sfactor_temp,2)

shotmin_temp=string(shotmin, format='(E19.3)')
shotmin_temp_trim=strtrim(shotmin_temp,2)

shotmax_temp=string(shotmax, format='(E19.3)')
shotmax_temp_trim=strtrim(shotmax_temp,2)

phi1_temp=string(phi1, format='(E19.3)')
phi1_temp_trim=strtrim(phi1_temp,2)

phi2_temp=string(phi2, format='(E19.3)')
phi2_temp_trim=strtrim(phi2_temp,2)

speedtweakfactor_temp=string(speedtweakfactor, format='(E19.3)')
speedtweakfactor_temp_trim=strtrim(speedtweakfactor_temp,2)

dopplerchoice_temp=string(dopplerchoice, format='(E19.3)')
dopplerchoice_temp_trim=strtrim(dopplerchoice_temp,2)

freqshiftchoice_temp=string(freqshiftchoice, format='(E19.3)')
freqshiftchoice_temp_trim=strtrim(freqshiftchoice_temp,2)

plutolength_temp=string(plutolength, format='(E19.3)')
plutolength_temp_trim=strtrim(plutolength_temp,2)

clight_override_temp=string(clight_override, format='(E19.3)')
clight_override_temp_trim=strtrim(clight_override_temp,2)

XYOUTS, charsize=1.8, color=0, 1, 520 ,'clight_override='+clight_override_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 500 ,'length_unit='+plutolength_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 480 ,'FS_switch='+freqshiftchoice_temp_trim, /DEVICE
XYOUTS, charsize=1.8, color=0, 1, 460 ,'clight='+clight_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 440 ,'DB_switch='+dopplerchoice_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 420 ,'ts='+speedtweakfactor_temp_trim, /DEVICE   
XYOUTS, charsize=1.8, color=0, 1, 400, 'sfactor='+sfactor_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 80, 'shotmin='+shotmin_temp_trim, /DEVICE 
XYOUTS, charsize=1.8, color=0, 1, 60, 'shotmax='+shotmax_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 40, 'phi1='+phi1_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 20, 'phi2='+phi2_temp_trim, /DEVICE 


;clight_temp=string(clight, format='(E19.3)')
;print, strtrim(clight_temp,2)
;******************************************************************
stop

shade_surf, ((eikona)), ax=35, az=-150, /zlog, MAX_VALUE=100000000000000, MIN_VALUE=100000, xstyle=2, $
ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
ythick=2, zthick=2, xcharsize=3, ycharsize=3, zcharsize=3
 if (debug_comments eq 1) then print, 'total jet gamma ray intensity, yet unnormalized,',total(eikona, /NaN)
  if (debug_comments eq 1) then print, 'max element excluding NaNs', max(eikona, /NaN)
stop
; 130418 IT WORKS, opens with GIMP2 SUPER  
shade_surf, ((eikona)), ax=35, az=-150, MAX_VALUE=100000000000000, MIN_VALUE=100000, xstyle=2, $
ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
ythick=2, zthick=2, xcharsize=3, ycharsize=3, zcharsize=3
 if (debug_comments eq 1) then print, 'total jet gamma ray intensity, yet unnormalized,',total(eikona, /NaN)
  if (debug_comments eq 1) then print, 'max element excluding NaNs', max(eikona, /NaN)
  
  stop
  
shade_surf, ((eikona^0.6)), ax=35, az=-150,  MAX_VALUE=1500000, MIN_VALUE=1000, xstyle=2, $
ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
ythick=2, zthick=2, xcharsize=4, ycharsize=4, zcharsize=4
 if (debug_comments eq 1) then print, 'total jet gamma ray intensity, yet unnormalized,',total(eikona, /NaN)
  if (debug_comments eq 1) then print, 'max element excluding NaNs', max(eikona, /NaN)
stop
; LOS LENGTH IS LLOS=sqrt(((nx-nxo)^2)+((y-y0)^2)+9z-z0)^2))
;SO DO LLOS/counterlast=length per voxel=ll, where counterlast=counter at los exit do it













end








































