
(defun c:RB ( / ss ang ) (vl-load-com)

(if (and (setq ss (ssget "_:L" '((0 . "INSERT"))))
(setq ang (getangle "\nSpecify Rotation Angle: "))
)
(
(lambda ( i / e o )
(while (setq e (ssname ss (setq i (1+ i))))
(vla-put-rotation (setq o (vlax-ename->vla-object e))
(+ (vla-get-rotation o) ang)
)
)
)
-1
)
)

(princ)
)

(defun C:r180 ()
(command "rotate" PAUSE "" (GETPOINT) "" 180 "")
) 





(defun C:gS()

(defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
	
(setq oldsnap (getvar "osmode"))
(if (= initialstation nil)
(setq initialStation 0)
)
(setvar "ucsfollow" 0)
(command "ucs" "world") 

(WHILE 1
(setq pt-1 (getpoint "Specify insertion point:"))                                              

(setq pt-2 (vlax-curve-getClosestPointTo *baseline pt-1))
(setq param (vlax-curve-getparamatpoint *baseline pt-2))
(setq sta-total (vlax-curve-getDistAtParam *baseline param))

(setq sta-100 (itoa (+ (fix (/ sta-total 100))  initialStation) ))
(setq sta-1 (rtos (rem sta-total 100) 2 0))



(if (< (rem sta-total 100) 10)
	(setq sta-1 (strcat "0" sta-1))
)
(setq objectStation (strcat sta-100 "+" sta-1))

(setvar "CLAYER" "GUIDELINES")
(setvar "textsize" 2)
(command "MTEXT" pt-2 "w" "200"  objectStation "")
)
(princ)
(end)
)



(defun C:sg()

(defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
	
(setq oldsnap (getvar "osmode"))
(if (= initialstation nil)
(setq initialStation 0)
)
(setvar "ucsfollow" 0)
(command "ucs" "world") 

(WHILE 1
(setq pt-1 (getpoint "Specify insertion point:"))                                              

(setq pt-2 (vlax-curve-getClosestPointTo *baseline pt-1))
(setq param (vlax-curve-getparamatpoint *baseline pt-2))
(setq sta-total (vlax-curve-getDistAtParam *baseline param))

(setq sta-100 (itoa (+ (fix (/ sta-total 100))  initialStation) ))
(setq sta-1 (rtos (rem sta-total 100) 2 0))



(if (< (rem sta-total 100) 10)
	(setq sta-1 (strcat "0" sta-1))
)
(setq objectStation (strcat sta-100 "+" sta-1))

(setvar "CLAYER" "GUIDELINES")
(setvar "textsize" 2)
(command "MTEXT" pt-1 "w" "200"  objectStation "")
)
(princ)
(end)
)







(defun C:getStation()
(if (= initialstation nil)
(setq initialStation 0)
)
(setvar "ucsfollow" 0)
(command "ucs" "world") 

(WHILE 1
(setq pt-1 (getpoint "Specify insertion point:"))                                              

(setq pt-2 (vlax-curve-getClosestPointTo *baseline pt-1))
(setq param (vlax-curve-getparamatpoint *baseline pt-2))
(setq sta-total (vlax-curve-getDistAtParam *baseline param))

(setq sta-100 (itoa (+ (fix (/ sta-total 100))  initialStation) ))
(setq sta-1 (rtos (rem sta-total 100) 2 0))



(if (< (rem sta-total 100) 10)
	(setq sta-1 (strcat "0" sta-1))
)
(setq objectStation (strcat sta-100 "+" sta-1))

(setvar "CLAYER" "GUIDELINES")
(setvar "textsize" 2)
(command "MTEXT" pt-2 "w" "200"  objectStation "")
)
(princ)
(end)
)




(defun C:SortLayout (/ cnt)
  (vl-load-com)
  (setq cnt 1)
  (foreach lay (acad_strlsort (layoutlist))
    (vla-put-taborder
      (vla-item (vla-get-layouts
    (vla-get-activedocument (vlax-get-acad-object))
  )
  lay
      )
      cnt
    )
   (setq cnt (1+ cnt))
  )
  (princ)
)



(DEFUN C:Y()
(setq any (getstring "Specify object to insert"))
(command "-insert" any PAUSE "" "" PAUSE)
(command "explode" "l")
)


(DEFUN C:ATT()
(command "-insert" "ATT" PAUSE "" "" "")
(command "explode" "l")
)


(defun C:is()
(setq initialStation (getint "specify initial station:"))
)


(defun C:initialStation()
(setq initialStation (getint "specify initial station:"))
)

(defun C:index()
(setq road (ssget "_X" '((8 . "road"))))
(setq viewport (ssget "_X" '((8 . "viewport"))))
(setq power (ssget "_X" '((8 . "power"))))
(setq route (ssget "_X" '((8 . "route"))))
(setq bore (ssget "_X" '((8 . "bore"))))
(setq trench (ssget "_X" '((8 . "trench"))))
(setq zayo (ssget "_X" '((8 . "zayo"))))
(setq baseline (ssget "_X" '((8 . "baseline"))))
(setq exist-util-pole-with-riser (ssget "_X" '((8 . "exist-util-pole-with-riser"))))
(setq prop-guy-and-anchor (ssget "_X" '((8 . "prop-guy-and-anchor"))))
(setq t-mh (ssget "_X" '((8 . "t-mh"))))
(setq cell-tower (ssget "_X" '((8 . "cell-tower"))))
(setq bldg (ssget "_X" '((8 . "bldg"))))
(setq indexOobjects (acet-ss-union (list ssadd road viewport power route bore trench zayo baseline exist-util-pole-with-riser prop-guy-and-anchor t-mh cell-tower bldg)))

(command "copy" indexOobjects "" pause pause "")
(princ)
)

(defun C:gs2()
(if (= initialstation nil)
(setq initialStation (getint "specify initial station:"))
(setq initialStation initialStation)
)
(setvar "ucsfollow" 0)
(command "ucs" "world") 
(setq pt-1 (getpoint "Specify insertion point:"))                                              
(setq *baseline (car (entsel "\nselect baseline entity\n\n")))
(setq pt-2 (vlax-curve-getClosestPointTo *baseline pt-1))
(setq param (vlax-curve-getparamatpoint *baseline pt-2))
(setq sta-total (vlax-curve-getDistAtParam *baseline param))
(setq sta-100 (itoa (+ (fix (/ sta-total 100))  initialStation) ))
(setq sta-1 (rtos (rem sta-total 100) 2 0))
(setq objectStation (strcat sta-100 "+" sta-1))
(setq derivative (vlax-curve-getfirstderiv *baseline param))
(setq angleInRad (angle '(0 0 0) derivative))
(setq angleOffset (angle pt-1 pt-2))
(setq offset (rtos (distance pt-1 pt-2) 2 0))
(if 
   (or 
     (and 
       (>= angleInRad 0)
       (< angleInRad (/ pi 2)) 
       (>= angleOffset (* 3 (/ pi 2))) 
       (< angleOffset (* 2 pi))
     )
     (and 
       (>= angleInRad (/ pi 2)) 
       (< angleInRad pi) 
       (>= angleOffset 0) 
       (< angleOffset (/ pi 2))
     )
     (and 
       (>= angleInRad pi) 
       (< angleInRad (* 3 (/ pi 2))) 
       (>= angleOffset (/ pi 2)) 
       (< angleOffset pi)
     )
     (and 
       (>= angleInRad (* 3 (/ pi 2))) 
       (< angleInRad (* 2 pi)) 
       (>= angleOffset pi) 
      (< angleOffset (* 3 (/ pi 2)))
     )
   )
(setq direction "RT") 
(setq direction "LT")
)
(alert (strcat "Station = " objectStation "\n\n\t" offset "' O/S " direction  ))
(princ)
)






(defun C:annotate ()

(setq qt (getpoint))

(setq *baseline (entsel "Specify baseline"))

(setq ENT (car *baseline))

(setq pt (vlax-curve-getClosestPointTo
ENT qt))
(setq rt (list (car pt)  (+ (cadr pt) 22) 0))
(setq st (list (+ (car pt) 4) (+ (cadr pt) 22) 0))

(setq os (rtos (distance qt pt) 2 0))



(setq par (vlax-curve-getparamatpoint ENT pt))
(setq sta (vlax-curve-getDistAtParam ENT par))
(setq deriv (vlax-curve-getfirstderiv ENT par))
(setq angrad (angle '(0 0 0) deriv))
(setq angleOS (angle pt qt))

(setq sta100  (rtos (/ sta 100) 2 0))
(setq sta1 (rtos (rem sta 100) 2 0)) 
                                              

(if 
   (or 
     (and 
       (>= angrad 0)
       (< angrad (/ pi 2)) 
       (>= angleOS (* 3 (/ pi 2))) 
       (< angleOS (* 2 pi))
     )
     (and 
       (>= angrad (/ pi 2)) 
       (< angrad pi) 
       (>= angleOS 0) 
       (< angleOS (/ pi 2))
     )
     (and 
       (>= angrad pi) 
       (< angrad (* 3 (/ pi 2))) 
       (>= angleos (/ pi 2)) 
       (< angleos pi)
     )
     (and 
       (>= angrad (* 3 (/ pi 2))) 
       (< angrad (* 2 pi)) 
       (>= angleos pi) 
      (< angleos (* 3 (/ pi 2)))
     )
   )
(setq dir "RT") 
(setq dir "LT")
)
(setvar "TEXTSTYLE" "TEXT-BOLD")

(setvar 'CECOLOR "240")

(setq string1 (strcat "STA " sta100 "+" sta1 ))
(setq string2 "ONCOR POLE # UNK. (00)")
(setq string3 (strcat os " ' O/S " dir " BOC"))

(command "TEXT" pt  "90"  string1 "")

(command "TEXT" rt  "90"  string2 "")

(command "TEXT" st  "90"  string3 "")
)


(defun c:CHECK ()
(command "_start" "G:\PROJECT FILES\ZAYO\MISC\ZAYO STANDARD\ZAYO LISP\Field Note Processor\Drawing check-points.docx")
(princ)
)


(defun c:CONTACT ()
(command "-insert" "G:\\PROJECT FILES\\ZAYO\\MISC\ZAYO STANDARD\\ZAYO BLOCKS\\contacts" PAUSE "" "" "")
(command "explode" "l")
(princ)
)

(defun C:cl (/ s p)

(setq p (getpoint))

(setq q (getpoint))

(setq s (entsel))

(COMMAND "offset" "t" s "m2p" p q )
)

(defun C:DWT ()

(command "_.-insert" "DW" pause "" "" "")

(command "ROTATE" "L"  "" "@" PAUSE)

(command "EXPLODE" "l" "" )

)


(defun C:city ()

(command "_.-insert" "c" pause "" "" "")

(command "ROTATE" "L"  "" "@" PAUSE)

(command "EXPLODE" "l" "" )

)

  
(defun C:DM ()
(setq dm (rtos (* (GETDIST) 40)))
(alert (strcat "Dimension in model space = " dm))
)  


(defun C:CON ()

(command "_.-insert" "CONTRACTOR" pause "" "" "")
(command "EXPLODE" "l" "" )

)  

(DEFUN C:LG()

(command "-insert" "G:\\PROJECT FILES\\ZAYO\\MISC\\ZAYO STANDARD\\ZAYO BLOCKS\\Legends and symbols.DWG" PAUSE "" "" "")
(command "explode" "l")
   
)

(DEFUN C:LGC()

(command "-insert" "G:\\PROJECT FILES\\ZAYO\\MISC\\ZAYO STANDARD\\ZAYO BLOCKS\\CONSTRUCTION NOTES\\CONSTRUCTION NOTES LEGEND" PAUSE "" "" "")
(command "explode" "l")
   
)

(DEFUN C:LGP()

(command "-insert" "G:\\PROJECT FILES\\ZAYO\\MISC\\ZAYO STANDARD\\ZAYO BLOCKS\\PROFILE\\PROFILE-OBJECTS.DWG" PAUSE "" "" "")
(command "explode" "l")
   
)


(DEFUN C:LGT()

(command "-insert" "G:\\PROJECT FILES\\ZAYO\\MISC\\ZAYO STANDARD\\ZAYO BLOCKS\\Legends and symbols for typicals" PAUSE "" "" "")
(command "explode" "l")
   
)


(DEFUN C:RI()

(command "-insert" "G:\\PROJECT FILES\\ZAYO\\MISC\\ZAYO STANDARD\\ZAYO BLOCKS\\riser.DWG" PAUSE "" "" "")
(command "ROTATE" "L" "" "@"  PAUSE)

   
)



(DEFUN C:DGT()
(command "-insert" "G:\\PROJECT FILES\\ZAYO\\MISC\\ZAYO STANDARD\\ZAYO BLOCKS\\DGT.DWG" PAUSE "" "" "")
(command "explode" "l")   
)

(DEFUN C:pi()
(command "-insert" "ppinformation" PAUSE "" "" "")
(command "explode" "l")   
)



(DEFUN C:LOOPT()
(command "-insert" "G:\\PROJECT FILES\\ZAYO\\MISC\\ZAYO STANDARD\\ZAYO BLOCKS\\LOOPT.DWG" PAUSE "" "" "")
(command "explode" "l")   
)

(DEFUN C:RIT()
(command "-insert" "G:\\PROJECT FILES\\ZAYO\\MISC\\ZAYO STANDARD\\ZAYO BLOCKS\\RIT.DWG" PAUSE "" "" "")
(command "explode" "l")
)

(defun C:SP ()

(command "_.-insert" "SEE-PROFILE" pause "" "" "")
(command "EXPLODE" "l" "" )

)  


(defun C:ic ()
(command "IMAGECLIP" PAUSE "N" "P" PAUSE)
) 



(defun C:rw ( / s)
(setq s (getdist "Right of way"))
(setq r (getdist  "Road"))

(setq offset (atoi (rtos (/ (- s r) 2) 2 0)))
(COMMAND "offset" offset pause )
)


(defun C:li ()

(COMMAND "layiso" "s" "o" "o" pause )
)

(defun C:ll ()
(COMMAND "layiso" "s" "l" "70" pause )
)


 
(defun C:CH0 ()
(COMMAND "CHAMFER" "d" "0" "" PAUSE)
)

(defun C:lu ()
(COMMAND "layUNiso")
)

(defun C:BRE ()
(COMMAND "BREAK" PAUSE "F" "@" "@")
(COMMAND "ERASE" PAUSE "" )
)


(defun C:GH ()
(COMMAND "GEOMAP"  "HYBRID"  )
(PRINC)
) 

(defun C:GA ()
(COMMAND "GEOMAP"  "AERIAL"  )
(PRINC)
) 

(defun C:GO ()
(COMMAND "GEOMAP"  "OFF"  )
(PRINC)
)

(defun C:GR ()
(COMMAND "GEOMAP"  "ROAD"  )
(PRINC)
)

(defun C:f0 ()
(COMMAND "fillet"  "R" 0 pause PAUSE)
(PRINC)
)


(defun C:RR ()
(COMMAND "ROTATE" PAUSE "R" "@" PAUSE )
(PRINC)
)

(defun C:SCALE-PLATS ()

(COMMAND "SCALE" PAUSE "R" "@" pause 100)
(PRINC)
)

(defun changeAttribValue (ent atttag oldval newval / entl)
  (while (and ent (/= "SEQEND" (cdr (assoc 0 (setq entl (entget ent))))))
    (and (= atttag (cdr (assoc 2 entl)))
         (= oldval (cdr (assoc 1 entl))) ;<- could use WCMATCH instead
         (entmod (subst (cons 1 newval) (assoc 1 entl) entl))
         (entupd ent)
         (mapcar 'princ (list "\n" oldval " -> " newval))
    )
    (setq ent (entnext ent))
  )
)

(defun C:AS ()
(setvar "attreq" 0)
(setvar "attdia" 1)


         (setq oldlayer (getvar "CLAYER"))
         (setvar "CLAYER" "aerial-span-footage")

(while (/= pt1 "")
         (setq pt1 (getpoint "Specify first point"))
         (setq pt2 (getpoint "Specify second point"))
         (setq s (distance pt1 pt2))
         (setq ang (/ (* 180 (angle pt1 pt2)) PI))
         (setq str (strcat (rtos s 2 0) "'"))
         


         (command "_.-insert"  "A-SPAN-FT" pause "40" "" "")


         (changeAttribValue (entlast) "125" "000" str)

         
         
         (command "rotate"  "l" "" "@" ang )

)
         (setvar "CLAYER" oldlayer)

)
(defun C:DB()
(setq oldsnap (getvar "osmode"))
(setq oldlayer (getvar "CLAYER"))
(setvar "CLAYER" "annotation")
(setq oldcolor (getvar 'cecolor))
(setvar 'CECOLOR "240")
(setq ss (ssget "X"  (list (cons 410 (getvar 'CTAB)) (cons 0  "text") (cons 1  "MATCHLINE*") )))
(setq m (ssname ss 0))
(setq n (ssname ss 1))
(setq elem (entget m))
(setq elen (entget n))
(setq stam (cdr (assoc 1 elem)))
(setq stan (cdr (assoc 1 elen)))
(setq stam (substr stam 10 10))
(setq stan (substr stan 11 10))
(setq text (strcat stan " TO" stam "\nDIR. BORE ("  "') 2-1.25\" HDPE \nE/W 144 ct FIBER OPTIC CABLE &\n10GA TRACER WIRE" ))
(setq p (getpoint))
(setvar "TEXTSTYLE" "TITLE")
(setq o (list (- (car p) 0.05) (+ (cadr p)  0.05) 0))
(setvar "osmode" 0)
(setq q (list (+ (car o) 1.75) (cadr o) 0))
(setq r (list (+ (car o) 1.75) (- (cadr o) 0.5) 0))
(setq s (list (car o) (- (cadr o) 0.5) 0))
(command "wipeout" o q r s o "")
(command "pline" o q r s o "")
(command "MTEXT" p "R" "0"   "W" 20  text "")
(command "explode" "l" "")
(setvar "osmode" oldsnap)
(setvar 'CECOLOR oldcolor)
(setvar "CLAYER" oldlayer)
)

(defun C:LRBY()
(setq y (getstring "LOWER by:"))
(setq m (CAR (ENTSEL )))
(setq elem (entget m))
(setq element (assoc 1 elem))
(setq enti (cdr element))
(setq lst (vl-string->list enti))
(setq st1 " (LOWER ")
(setq st2 "\")")
(setq z (strcat ENTI ST1 y st2 ))
(setq lst2 (cons '1 z)) 
(setq lst3 (subst lst2 element elem))
(setq lst4 (entmod lst3))
(ssdel m s)
(setq x (+ x 1))
(setq y (+ y 1))
)

(defun C:LR()
(setq y (getstring "LOWER TO FEET"))
(setq yy (getstring "LOWER TO___INCH:"))
(setq m (CAR (ENTSEL )))
(setq elem (entget m))
(setq element (assoc 1 elem))
(setq enti (cdr element))
(setq lst (vl-string->list enti))
(setq st1 " (LOWER TO ")
(setq st2 "'-")
(setq st3 "\")")
(setq z (strcat ENTI ST1 y st2 yy st3 ))
(setq lst2 (cons '1 z)) 
(setq lst3 (subst lst2 element elem))
(setq lst4 (entmod lst3))
(ssdel m s)
(setq x (+ x 1))
(setq y (+ y 1))
)


(defun C:CH ()
(initget 
"ONCOR-SECONDARY-DRIP-LOOP ONCOR-BOTTOM-OM-TRANS AT&T ONCOR-NEUTRAL ONCOR-RISER-TOC-SECONDARY ONCOR-OPEN-WIRE-SECONDARY CHARTER Arlington-city-Weatherhead-drip-loop Arlington-city-Fiber ARLINGTON-CITY-ST ARLINGTON-CITY-DUPLEX ARLINGTON-CITY-SECONDARY TCI-
TKR MCI-FIBER ZAYO SWB-MULTIPAIR-CABLE")

(setq y (getKWORD "[ONCOR-SECONDARY-DRIP-LOOP/
ONCOR-BOTTOM-OM-TRANS/
ONCOR-NEUTRAL/
ONCOR-RISER-TOC-SECONDARY/
ONCOR-OPEN-WIRE-SECONDARY/
CHARTER FIBER & COAX/
AT&T/
Arlington-city-Fiber/
ARLINGTON-CITY-ST/
ARLINGTON-CITY-DUPLEX/
ARLINGTON-CITY-SECONDARY/
TCI-TKR/
MCI-FIBER/
SWB-MULTIPAIR-CABLE/
ZAYO]"))



(if (= y "ONCOR-SECONDARY-DRIP-LOOP")
    (setq Z "ONCOR SECONDARY DRIP LOOP = 0'-0\"")
    (if (= y "AT&T")
        (setq Z "AT&T = 0'-0\"")
        (if (= y "Arlington-city-Fiber")
            (setq z "ARLINGTON CITY FIBER = 0'-0\"")
            (if (= y "TCI-TKR")
                (setq z "TCI-TKR = 0'-0\"")
                (if (= y "MCI-FIBER")
                    (setq z "MCI FIBER = 0'-0\"") 
                    (if (= y "ONCOR-OPEN-WIRE-SECONDARY")
                        (setq z "ONCOR OPEN WIRE SECONDARY = 0'-0\"") 
                        (if (= y "ZAYO")
                            (setq z "PROP. ZAYO = 0'-0\"") 
                            (if (= y "ONCOR-NEUTRAL")
                                (setq z "ONCOR NEUTRAL = 0'-0\"")
                                (if (= y "SWB-MULTIPAIR-CABLE")
                                    (setq z "SWB MULTIPAIR CABLE = 0'-0\"")
                                    (if (= y "ARLINGTON-CITY-ST")
                                        (setq z "ARLINGTON CITY ST. LIGHT DRIP LOOP = 0'-0\"")
                                        (if (= y "ARLINGTON-CITY-DUPLEX")
                                            (setq z "ARLINGTON CITY DUPLEX SECONDARY = 0'-0\"") 
                                            (if (= y "ONCOR-BOTTOM-OM-TRANS")
                                                (setq z "ONCOR BOTTOM OM TRANS = 0'-0\"")
                                                (if (= y "ONCOR-RISER-TOC-SECONDARY")
                                                    (setq z "ONCOR RISER TOC-SECONDARY = 0'-0\"")
                                                    (if (= y "OR")
                                                        (setq z "ONCOR RISER TOC-SECONDARY = 0'-0\"")
                                                        (if (= y "ARLINGTON-CITY-SECONDARY")
                                                            (setq z "ARLINGTON CITY SECONDARY DRIP LOOP = 0'-0\"")
                                                            (if (= y "CHARTER")
                                                                (setq z "CHARTER FIBER & COAX = 0'-0\"")
                                                
                                                            )  
                                                        )  
                                                    )  
                                                )
                                            )
                                        )
                                    )
                                )
                            ) 
                        )   
                    )   
                )   
            )
        )
    )
)


(setq m (CAR (ENTSEL )))
(setq elem (entget m))
(setq element (assoc 1 elem))
(setq enti (cdr element))
(setq lst (vl-string->list enti))


(setq lst2 (cons '1 z)) 
(setq lst3 (subst lst2 element elem))
(setq lst4 (entmod lst3))
(ssdel m s)

)


(defun C:RS()
(setq y (getstring "RAISE TO___FEET:"))
(setq yy (getstring "RAISE TO___INCH:"))
(setq m (CAR (ENTSEL )))
(setq elem (entget m))
(setq element (assoc 1 elem))
(setq enti (cdr element))
(setq lst (vl-string->list enti))
(setq st1 " (RAISE TO ")
(setq st2 "'-")
(setq st3 "\")")
(setq z (strcat ENTI ST1 y st2 yy st3 ))
(setq lst2 (cons '1 z)) 
(setq lst3 (subst lst2 element elem))
(setq lst4 (entmod lst3))
(ssdel m s)
(setq x (+ x 1))
(setq y (+ y 1))
)


(defun C:SQ()
(setq y (getstring "POLE SEQ # PS ?:"))
(setq m (CAR (ENTSEL )))
(setq elem (entget m))
(setq element (assoc 1 elem))
(setq enti (cdr element))
(setq lst (vl-string->list enti))
(setq st1 " (PS ")
(setq st2 ")")
(setq z (strcat ENTI st1 y st2 ))
(setq lst2 (cons '1 z)) 
(setq lst3 (subst lst2 element elem))
(setq lst4 (entmod lst3))
(ssdel m s)
(setq x (+ x 1))
(setq y (+ y 1))
)


 (defun C:number()


(setvar "TEXTSTYLE" "TEXT-BOLD")

(setq oldlayer (getvar "CLAYER"))
(setvar "CLAYER" "VIEWPORT")
(setvar 'CECOLOR "ByLayer")

(setq x (getint"specify initial number or 1:"))
(if (= x nil)
(setq x 1)
(setq x x))
(princ)



(while

(if (< x 10)
(setq xstring (strcat "00" (itoa x))) 
(if (> x 99)
(setq xstring (itoa x))
(setq xstring (strcat "0" (itoa x)))
)
)




(setq p (getpoint))

(setq q (list (+ (car p) 10) (cadr p) 0))



(command "MTEXT" p "R" "0"  q xstring "")
(command "explode" "l" "")
(command "scale" "l" "" p  20 )




(setq x (+ x 1))





)
(setvar "CLAYER" oldlayer)

)

 (defun C:nu()


(setvar "TEXTSTYLE" "TEXT-BOLD")

(setq oldlayer (getvar "CLAYER"))
(setvar "CLAYER" "VIEWPORT")
(setvar 'CECOLOR "ByLayer")

(setq x (getint"specify initial number or 1:"))
(if (= x nil)
(setq x 1)
(setq x x))
(princ)



(while

(if (< x 10)
(setq xstring (strcat "00" (itoa x))) 
(if (> x 99)
(setq xstring (itoa x))
(setq xstring (strcat "0" (itoa x)))
)
)




(setq p (getpoint))

(setq q (list (+ (car p) 10) (cadr p) 0))



(command "MTEXT" p "R" "0"  q xstring "")
(command "explode" "l" "")
(command "scale" "l" "" p  20 )




(setq x (+ x 1))





)
(setvar "CLAYER" oldlayer)

)


 (defun C:nup()


(setvar "TEXTSTYLE" "TEXT-BOLD")

(setq oldlayer (getvar "CLAYER"))
(setvar "CLAYER" "VIEWPORT")
(setvar 'CECOLOR "ByLayer")

(setq x (getint"specify initial number or 1:"))
(if (= x nil)
(setq x 1)
(setq x x))
(princ)



(while

(if (< x 10)
	(setq xstring (strcat "0" (itoa x) "A")) 
	(setq xstring (strcat  (itoa x) "A"))

)




(setq p (getpoint))

(setq q (list (+ (car p) 10) (cadr p) 0))



(command "MTEXT" p "R" "0"  q xstring "")
(command "explode" "l" "")
(command "scale" "l" "" p  20 )




(setq x (+ x 1))





)
(setvar "CLAYER" oldlayer)

)

(defun C:mlw ( / dist str1 text  m  elem sta sheet sheetint sheet-1 ml sheet2 stat p q r st a ang )
(setq oldlayer (getvar "CLAYER"))
(setvar "CLAYER" "matchline")
(setvar 'CECOLOR "ByLayer")

(command "mspace")


(setq stat (car (entsel "select station text")))
(command "pspace")
     (setq stat (entget stat))
     (setq stat (cdr (assoc 1 stat)))
(if (= (strlen stat) 4)
	(setq stat (substr stat 1 1))
		(if (= (strlen stat) 5)
			(setq stat (substr stat 1 2))
				(if (= (strlen stat) 6)
					(setq stat (substr stat 1 3))
				)
		)
)
(setq stat (atoi stat))



(setq dist 0)



(setq p (getpoint))



(command "pline" p "w" 0.02 0.02 pause "" )
(setq q (getvar "lastpoint"))
(setq a (angle p  q))
(setq ang (- a (/ 3.14159265359 2)))
(setq r (list (/ (+ (car p) (car q)) 2) (/ (+ (cadr p) (cadr q)) 2) 0.0))
(setq s (polar r ang -0.15))
(setq t (polar r ang 0.23))

(setq sta (getvar "ctab"))
(setq sheet (substr sta 1 3))
(setq sheetint (atoi sheet))

(if (< (car p) 8)
(setq sheet-1 (- sheetint 1))
(setq sheet-1 (+ sheetint 1)))




(setq stat (strcat (itoa stat) "+"))

(if (< dist 10) 
(setq dist (strcat "0" (rtos dist 2 0)))
(setq dist (rtos dist 2 0)))


(setq sheetstr (itoa sheet-1 ))

(setq ml "MATCHLINE STA ")


(if (< sheet-1 10)

(setq ml2 " - SEE SHEET 00")  

(if (> sheet-1 99)
(setq ml2 " - SEE SHEET ") 
(setq ml2 " - SEE SHEET 0")))

(setq sheet2 (strcat ml stat dist ml2  sheetstr))

(command "MTEXT" s "S" "MATCHLINE" "j" "mc"  "R" (* a ( / 180 3.14)) "w" 5  sheet2 "")


(command "explode" "l" "")

(command "SCALE" "l" "" r 0.77 )

(setvar "CLAYER" oldlayer)
(setq str1 (getstring "Reverse No: "))

(if (or ( = str1 "y") ( = str1 "Y"))

(command "move" "l" "" r t )
(princ))
(princ)


)


(defun C:ml ( / dist str1 text  m  elem sta sheet sheetint sheet-1 ml sheet2 stat p q r st a ang )

(defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

(setq oldlayer (getvar "CLAYER"))
(setvar "CLAYER" "matchline")
(setvar 'CECOLOR "ByLayer")

(command "mspace")


(setq m (car (entsel "select station text")))
(command "pspace")


(setq elem (entget m))
(setq stat (cdr (assoc 1 elem)))








(setq p (getpoint))



(command "pline" p "w" 0.02 0.02 pause "" )
(setq q (getvar "lastpoint"))
(setq a (angle p  q))
(setq ang (- a (/ 3.14159265359 2)))
(setq r (list (/ (+ (car p) (car q)) 2) (/ (+ (cadr p) (cadr q)) 2) 0.0))
(setq s (polar r ang -0.15))
(setq t (polar r ang 0.23))

(setq sta (getvar "ctab"))
(setq sheet (substr sta 1 3))
(setq sheetint (atoi sheet))

(if (< (car p) 8)
(setq sheet-1 (- sheetint 1))
(setq sheet-1 (+ sheetint 1)))


(setq sheetstr (itoa sheet-1 ))

(setq ml "MATCHLINE STA ")


(if (< sheet-1 10)

(setq ml2 " - SEE SHEET 00")  

(if (> sheet-1 99)
(setq ml2 " - SEE SHEET ") 
(setq ml2 " - SEE SHEET 0")))

(setq sheet2 (strcat ml stat ml2  sheetstr))

(command "MTEXT" s "S" "MATCHLINE" "j" "mc"  "R" (* a ( / 180 3.14)) "w" 5  sheet2 "")


(command "explode" "l" "")

(command "SCALE" "l" "" r 0.77 )

(setvar "CLAYER" oldlayer)
(setq str1 (getstring "Reverse No: "))

(if (or ( = str1 "y") ( = str1 "Y"))

(command "move" "l" "" r t )
(princ))
(princ)






)



(defun C:TM()
(TM-BEGIN)
(initget "UP DOWN GUIDELINES PROFILE ELEVATION")
(setq TIC (getkword "[UP/DOWN/GUIDELINES/PROFILE/ELEVATION]"))
(cond	((= TIC "UP")
	(setq TIC "TIC-UP")
	(TM-BODY))
	
	((= TIC "DOWN")
	(setq TIC "TIC-DN")
	(TM-BODY))

	((= TIC "GUIDELINES")
	(setq TIC "GUIDE-TIC")
	(TM-BODY))

	((= TIC "PROFILE")
	(setq TIC "PROFILE-TIC")
	(TM-BODY))
	
	((= TIC "ELEVATION")
	(TM-ELEVATION))
)
)


(defun TM-BEGIN ()
(setq flag (getvar "qaflags"))
(setvar "qaflags" 5)
)


(defun TM-BODY ()
(setq y (getint "Specify initial number or 1:"))
(if (= y nil)
(setq y 1)
(setq y y))
(command "_.MEASURE" pause "B" TIC "Y" "100" "")
(setq sel1 (ssget "P"))
(command "explode" sel1 "")
(setq s (ssget "P" '((0 . "TEXT"))))
(while
(setq x 0)
(setq m (ssname s x))
(setq elem (entget m))
(setq element (assoc 1 elem))
(setq enti (cdr element))
(setq lst (vl-string->list enti))
(setq st1 "+00")
(setq z (strcat (itoa y) st1))
(setq lst2 (cons '1 z))
(setq lst3 (subst lst2 element elem))
(setq lst4 (entmod lst3))
(ssdel m s)
(setq x (+ x 1))
(setq y (+ y 1))
(setvar "qaflags" 0)
)
)


(defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )




(defun TM-ELEVATION()


(defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

(setq oldlayer (getvar "CLAYER"))
(setvar 'CECOLOR "ByLayer")
(setq x (getint "Enter (type) lowest elevation:"))

(WHILE 1
(setq p (getpoint "specify lowest elevation:"))


(cond	((= (getvar "tilemode") 1)
	
	(setvar "CLAYER" "guidelines")
	(setvar "TEXTSTYLE" "TEXT")
	(setq y 40))

	((= (getvar "tilemode") 0)
	
	(setvar "CLAYER" "annotation")
	(setvar "TEXTSTYLE" "CONST-TXT")
	(setq y 1))

)

(if (< x 10)
(setq xstring (strcat "00" (itoa x))) 
(if (> x 99)
(setq xstring (itoa x))
(setq xstring (strcat "0" (itoa x)))
)
)


(command "MTEXT" p "R" "0"  "w" "5" xstring "")
(command "explode" "l" "")





(setq x (+ x 10))
(if (< x 10)
(setq xstring (strcat "00" (itoa x))) 
(if (> x 99)
(setq xstring (itoa x))
(setq xstring (strcat "0" (itoa x)))
)
)
(setq p (list (car p) (+ y (cadr p)) 0))
(command "MTEXT" p "R" "0"  "w" "5" xstring "")
(command "explode" "l" "")


(setq x (+ x 10))
(if (< x 10)
(setq xstring (strcat "00" (itoa x))) 
(if (> x 99)
(setq xstring (itoa x))
(setq xstring (strcat "0" (itoa x)))
)
)
(setq p (list (car p) (+ y (cadr p)) 0))
(command "MTEXT" p "R" "0"  "w" "5" xstring "")
(command "explode" "l" "")


(setq x (+ x 10))
(if (< x 10)
(setq xstring (strcat "00" (itoa x))) 
(if (> x 99)
(setq xstring (itoa x))
(setq xstring (strcat "0" (itoa x)))
)
)
(setq p (list (car p) (+ y (cadr p)) 0))
(command "MTEXT" p "R" "0"  "w" "5" xstring "")
(command "explode" "l" "")

(setq x (+ x 10))
(if (< x 10)
(setq xstring (strcat "00" (itoa x))) 
(if (> x 99)
(setq xstring (itoa x))
(setq xstring (strcat "0" (itoa x)))
)
)

(setq p (list (car p) (+ y (cadr p)) 0))
(command "MTEXT" p "R" "0"  "w" "5" xstring "")
(command "explode" "l" "")
(setq x (- x 40))

)



(setvar "CLAYER" oldlayer)



)


(defun c:ucc()

(command "ucsfollow" "1" )
(command "ucs" pause pause pause )
(setq pt (getvar "lastpoint"))
(command "ucsfollow" "0" )
(command "zoom" "c" pt "800")
)

  
(defun c:ucv()

(command "MSPACE")
(setq pt1 (list 20.0 20.0 0.0))
(setq pt2 (list 0.0 0.0 0.0))
(setq sel1 (ssget "w" pt1 pt2))
(command "._mview" "lock" "off" (ssget "x")"")
 
(command "ucsfollow" "1" )

(command "ucs" pause pause pause)
(setq pt (getvar "lastpoint"))

(command "ucsfollow" "0" )

(command "zoom" "c" pt "800")
(command "zoom" "end" pause "end" pause)


(command "._mview" "lock" "on" (ssget "x")"")
(command "ZOOM" "E")


)

(defun c:zv()

(command "MSPACE")
(command "._mview" "lock" "off" (ssget "x")"")

(command "zoom" "end" pause "end" pause)

(command "._mview" "lock" "on" (ssget "x")"")
(command "ZOOM" "E")

)



(defun c:ucW()
(command "ucsfollow" "1" )
(command "ucs" "W")
(command "ucsfollow" "0" )
)




(defun C:mlP ( / dist str1 text  m  elem sta sheet sheetint sheet-1 ml sheet2 stat p q r st a ang )

(defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

(setq oldlayer (getvar "CLAYER"))
(setvar "CLAYER" "matchline")
(setvar 'CECOLOR "ByLayer")

(command "mspace")


(setq m (car (entsel "select station text")))
(command "pspace")


(setq elem (entget m))
(setq stat (cdr (assoc 1 elem)))


(setq p (getpoint))


(setq pt1 (list (car p) 5.86108 0.0)) 
(setq pt2 (list (car p) 9.53108 0.0)) 

(setq ptR (list (+ (car p) 0.11) 7.90379 0.0))
(setq ptL (list (- (car p) 0.11) 7.90379 0.0))
(command "pline" pt1 "w" 0.02 0.02  pt2 "" )





(setq q (getvar "lastpoint"))
(setq a (angle p  q))
(setq ang (- a (/ 3.14159265359 2)))
(setq r (list (/ (+ (car p) (car q)) 2) (/ (+ (cadr p) (cadr q)) 2) 0.0))
(setq s (polar r ang -0.15))
(setq t (polar r ang 0.23))

(setq sta (getvar "ctab"))
(setq sheet (substr sta 1 3))
(setq sheetint (atoi sheet))

(if (< (car p) 8)
(setq sheet-1 (- sheetint 1))
(setq sheet-1 (+ sheetint 1)))


(setq sheetstr (strcat (itoa sheet-1 ) "A"))

(setq ml "MATCHLINE STA ")


(if (< sheet-1 10)

(setq ml2 " - SEE SHEET 0")  

(setq ml2 " - SEE SHEET ") 
)


(setq sheet2 (strcat ml stat ml2  sheetstr))
(setvar "osmode" 0)
(if (< (car p) 8)
(command "MTEXT" ptl "S" "MATCHLINE" "j" "mc" "R" "90"  "w" 5  sheet2 "")
(command "MTEXT" ptr "S" "MATCHLINE" "j" "mc" "R" "90"  "w" 5  sheet2 "")

)
(setq pointSCALE (getvar "lastpoint"))
(command "explode" "l" "")

(command "SCALE" "l" "" pointSCALE 0.77 )

(setvar "CLAYER" oldlayer)
(setvar "osmode" oldsnap)

(princ)


)

(defun C:RD()
(setq oldlayer (getvar "CLAYER"))
(setvar "CLAYER" "ROAD")
(setvar "qaflags" 5)

(setq oldmline (getvar "CMLSTYLE"))
(setvar "CMLSTYLE" "STANDARD")
(setq width (fix (getdist"Specify width of road")))ap 

(command "mline" "s" width pause )
(while (> (getvar 'CmdActive) 0) (command pause))

(command "explode" "L" "")

(command "join" "P" "")


(setvar "CMLSTYLE" oldmline)
(setvar "qaflags" 0)
(setvar "CLAYER" oldlayer)
(princ)

)





;;This code is from online
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:PS0()
  (foreach lay (layoutlist)
    (command "_LAYOUT" "_Set" lay "PSLTSCALE" 0)
    (command "_pspace")
    (command "_zoom" "extents")
  )
)


(defun c:UCSF_OFF_ALL()
  (foreach lay (layoutlist)
    (command "_LAYOUT" "_Set" lay "PSLTSCALE" 0)
    (command "_MSPACE")
	(COMMAND "UCSFOLLOW" "0" )
	(command "_PSPACE")
    (command "_zoom" "extents")
  )
)





(defun c:PS1()
  (foreach lay (layoutlist)
    (command "_LAYOUT" "_Set" lay "PSLTSCALE" 1)
    (command "_pspace")
    (command "_zoom" "extents")
  )
)

(defun c:UCSF0()
(foreach lay (layoutlist)
	(command "_Mspace")
	(command "_UCSFOLLOW" "0")
   
  )
)


(defun c:INSERT-IN-ALL-LAYOUTS() 
  (foreach lay (layoutlist)
	(command "_LAYOUT" "_Set" lay)
	(command "-insert" "SSSS" "0,0" "" "" "")
	(command "explode" "l")
    
  )
)


(defun c:ERASE-and-paste-in-all-layouts()
(setq a (getpoint "specify first corner"))
(setq b (getpoint "specify first corner"))
 
  (foreach lay (layoutlist)
	(command "_LAYOUT" "_Set" lay)
	(command "ERASE" a b "")
	(command "pasteclip" "0,0")
	
    
  )
)


(defun c:DELETE-MATCHLINES-IN-ALL-LAYOUTS () 
  (foreach lay (layoutlist)
	(command "_LAYOUT" "_Set" lay)
	(command "erase" (ssget "x" '((8 . "matchline"))) "") 
  )
)


(defun c:renamelayouts (/ laylist c)
(setq laylist (layoutlist) c 1)
(setq a (getint "Specify initial layout" ))
(foreach x laylist
(if (< a 10)
	(command "_LAYOUT" "R" x (strcat "00" (itoa a)))
	(if (< a 99)
		(command "_LAYOUT" "R" x (strcat "0" (itoa a)))
		(command "_LAYOUT" "R" x (itoa a))
	)
	

)
(setq a (+ a 1))
(setq c (+ 1 c))
)
(princ)
)


;;;Changes station numbers by specified amount.  Station format, "+" in
;;;in the number, is read and edited.  The new number is conformed to the
;;;station format to the user specified linear units of precision.
;;;
;;;Metric stations are sensed by the presence of three digits after the
;;;'+' and before the decimal.
;;;
;;;If precision is reduced by the user it cannot be regained by this routine.
;;;The prior digits of higher precision are discarded and cannot be recovered
;;;except by the method they were originally obtained or other equal method.
;;;
;;;	AUTHOR: HENRY C. FRANCIS
;;;		425 N. ASHE ST.
;;;		SOUTHERN PINES, NC 28387
;;;
;;;		All rights reserved without prejudice.
;;;
;;;	Copyright:	9-02-94
;;;	Edited:		9-27-2005
;;;
(DEFUN C:ADJSTA (/ m_units)			;adjv tele chrs nsta
  (if stait nil (load "stait" "\nFile STAIT.LSP not loaded!"))
  (if ustr nil (load "ustr" "\nFile USTR.LSP not loaded!"))
  (if uint nil (load "uint" "\nFile UINT.LSP not loaded!"))
  (setq nn_old_err *error*)
  (setq	adjv (distof (ustr 1
			   "\nValue to change Sta. by "
			   (if adjv
			     (rtos adjv 2 2)
			     ""
			   ) ;_ end of if
			   nil
		     ) ;_ end of ustr
	     ) ;_ end of distof
  ) ;_ end of setq
  (setq c_lupr (getvar "luprec"))
  (setq	t_lupr (uint 1
		     ""
		     "Units of precision? "
		     (if t_lupr
		       t_lupr
		       2
		     ) ;_ end of if
	       ) ;_ end of uint
  ) ;_ end of setq
  (setvar "luprec" t_lupr)
  (princ "\nSelect a number string to change: ")
  (while
    (setq tele (nentsel "\nSelect station to change "))
     (setq elem (entget (car tele)))
     (setq tstr (cdr (assoc 1 elem)))
     (setq cpos 1)
     (setq strl (strlen tstr))
     (while
       (and
	 (< cpos strl)
	 (setq chrs (substr tstr cpos 1))
	 (or
	   (eq "." chrs)
	   (not(eq (type (read "1")) (type (read chrs))))
	   (eq "+" chrs)
	   (eq "-" chrs)
	   (eq "," chrs)
	 ) ;_ end of or
       ) ;_ end of and
	(setq cpos (1+ cpos))
     ) ;_ end of while
     (setq cpoe cpos)
     (setq cpop cpos)
     (while
       (and
	 (setq chrs (substr tstr cpoe 1))
	 (not (> cpoe strl))
	 (or
	   (eq "." chrs)
	   (eq (type (read "1")) (type (read chrs)))
	   (eq "+" chrs)
	   (eq "-" chrs)
	   (eq "," chrs)
	 ) ;_ end of or
       ) ;_ end of and
	(setq cpoe (1+ cpoe))
     ) ;_ end of while
     (cond ((wcmatch tstr "*+###*") (setq m_units T))
	   ((wcmatch tstr "*+##*") (setq m_units nil))
     ) ;_ end of cond
     (if
       (eq (read "real") (type (read (substr tstr cpos cpoe))))
	(progn
	  (setq	nnum
		 (+ adjv
		    (distof
		      (substr tstr cpos (- cpoe cpos))
		    ) ;_ end of distof
		 ) ;_ end of +
	  ) ;_ end of setq
	  (setq	nstr
		 (strcat
		   (substr tstr 1 (1- cpos))
		   (rtos nnum)
		   (substr tstr cpoe)
		 ) ;_ end of strcat
	  ) ;_ end of setq
	) ;_ end of progn
	(progn
	  (while
	    (and
	      (not (> cpop strl))
	      (setq chrs (substr tstr cpop 1))
	      (not (eq "+" chrs))
	      (not (eq "-" chrs))
	      (not (eq "," chrs))
	    ) ;_ end of and
	     (setq cpop (1+ cpop))
	  ) ;_ end of while
	  (cond
	    ((eq "-" chrs)
	     (setq nsta "T")
	    )
	    ((or(eq "+" chrs)(eq "," chrs))
	     (setq nsta nil)
	    )
	  ) ;_ end of cond
	  (if nsta
	    (setq nnum
		   (+ adjv
		      (- 0
			 (distof
			   (if (not (>= cpop (1- strl)))
			     (strcat
			       (substr tstr cpos (- cpop cpos))
			       (substr tstr (1+ cpop) (- cpoe cpop 1))
			     ) ;_ end of strcat
			     (strcat
			       (substr tstr cpos (- cpoe cpos))
			       (substr tstr cpoe)
			     ) ;_ end of strcat
			   ) ;_ end of if
			 ) ;_ end of distof
		      ) ;_ end of -
		   ) ;_ end of +
	    ) ;_ end of setq
	    (setq nnum
		   (+ adjv
		      (distof
			(if (not (>= cpop (1- strl)))
			  (strcat
			    (substr tstr cpos (- cpop cpos))
			    (substr tstr (1+ cpop) (- cpoe cpop 1))
			  ) ;_ end of strcat
			  (strcat
			    (substr tstr cpos (- cpoe cpos))
			    (substr tstr cpoe)
			  ) ;_ end of strcat
			) ;_ end of if
		      ) ;_ end of distof
		   ) ;_ end of +
	    ) ;_ end of setq
	  ) ;_ end of if
	  (setq	nstr
		 (strcat
		   (substr tstr 1 (1- cpos))
		   (stait nnum t_lupr)
		   (substr tstr cpoe)
		 ) ;_ end of strcat
	  ) ;_ end of setq
	) ;_ end of progn
     ) ;_ end of if
     (setq elem
	    (subst (cons 1 nstr)
		   (assoc 1 elem)
		   elem
	    ) ;_ end of subst
     ) ;_ end of setq
     (entmod elem)
;     (COMMAND)
  ) ;_ end of while
  (setvar "luprec" c_lupr)
  (princ)
) ;_ end of DEFUN

;|«Visual LISP© Format Options» (72 2 40 2 T "end of " 60 9 0 0 0 T T nil T)
 ***Don't add text below the comment!***|;



(defun C:fi()

(command "-imageadjust" pause "" "f" "70" )

)

(defun C:dimr( /
;;local symbols
                       ename
                       ent
                       gripset
                       i
                       flag
                       j
                       oldeko
                       olderr
                       ss
;;local functions
++ *lerror*
                  )
;;local error handler
(defun *lerror* (msg)
  (if (member msg '("console break" "Function cancelled"))
      (princ)
      (princ (strcat "\nError " (itoa (getvar "ERRNO")) ": " msg))
  );if
;;restore old error handler
  (setq *error* olderr olderr nil)
  (princ)
);*lerror*
;; ++ Increment counter &a .
;;;call: (++ 'a) <<--note the quote!
(defun ++ (&a)
        (set &a (1+ (eval &a)))
);end of ++
;;support for noun-verb selection
;;save a selection set of any gripped objects
(setq gripset (cadr (ssgetfirst)))
;;start an UNDO group, which cancels grips
(command "._UNDO" "_GROUP")
;;install local *error* function
(setq olderr *error*
      *error* *lerror*)
;;if gripset exists
(if gripset
;;step through gripset and modify any DIMENSION entities found
  (progn
    (setq i 0 j 0 ss (ssadd))
    (while (< i (sslength gripset))
      (setq ename (ssname gripset i))
      (if (= "DIMENSION" (cdr (assoc 0 (setq ent(entget ename)))))
        (progn
        (entmod(subst (cons 51 (- (cdr (assoc 51 ent)) PI)) (assoc 51 ent) ent))
        (++ 'j)
        );progn
      );if
      (++ 'i)
    );while
    (princ (strcat "\nText Rotated on " (itoa j) " DIMENSIONS."))
;;nil ss & set flag to set up for the case of null gripset
    (setq ss nil flag T)
  );progn
;;otherwise prompt user to select DIMENSIONS to change
  (progn
   (prompt "\nSelect Which DIMENSIONS to Rotate Dimension Text 180°: ")
      (setq ss (ssget '((0 . "DIMENSION"))))
  );
);if
;;process all entities in the resulting selection set
(if ss
  (progn
    (setq i 0)
    (while (< i (sslength ss))
      (setq ent (entget (ssname ss i)))
      (entmod(subst (cons 51 (- (cdr (assoc 51 ent)) PI)) (assoc 51 ent) ent))
      (++ 'i)
    );while
    (princ (strcat "\nText Rotated on " (itoa (sslength ss)) " DIMENSIONS."))
  );progn
  (if (null flag) (princ "\nNo DIMENSIONS Selected!"))
);if
;;restore previous error function
(setq *error* olderr)
;;switch off CMDECHO while ending the UNDO group
(setq oldeko (getvar "CMDECHO"))
(setvar "CMDECHO" 0)
(command "._UNDO" "_END")
(setvar "CMDECHO" oldeko);restore old CMDECHO value
(princ);quiet exit
);


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;utility lines;;
(defun begin_utility ()
(setq oldlayer (getvar 'CLAYER))
(setq oldcolor (getvar 'CECOLOR))
(setq oldlinetype (getvar 'CELTYPE))
(setq oldlineweight (getvar 'CELWEIGHT))
(setq oldlinewid (getvar 'PLINEWID))
)

(defun end_utility ()
(setvar 'CLAYER oldlayer)
(setvar 'CECOLOR oldcolor)
(setvar 'CELTYPE oldlayer)
(setvar 'CELWEIGHT oldlineweight)
(setvar 'PLINEWID 0)
)




(defun C:WL()
(begin_utility)
(setvar 'CLAYER "water")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "ByLayer")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(command "pline" pause)
(end_utility)
)

(defun C:rw()
(begin_utility)
(setvar "CLAYER" "right of way")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "ByLayer")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(command "pline" pause)
(end_utility)
)

(defun C:ssl()
(begin_utility)
(setvar "CLAYER" "sewer")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "ByLayer")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(command "pline" pause)
(end_utility)gas 
)


(defun C:ss()
(begin_utility)
(setvar "CLAYER" "sewer")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "ByLayer")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(command "pline" pause)
(end_utility)gas 
)

(defun C:stm()
(begin_utility)
(setvar "CLAYER" "stm-swr")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "bylayer")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(command "pline" pause)
(end_utility)
)

(defun C:stl()
(begin_utility)
(setvar "CLAYER" "stm-swr")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "bylayer")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(command "pline" pause)
(end_utility)
)

(defun C:tl()
(begin_utility)
(setvar "CLAYER" "telephone")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "ByLayer")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(command "pline" pause)
(end_utility)
)

(defun C:gas()
(begin_utility)
(setvar "CLAYER" "utility")
(setvar 'CECOLOR "yellow")
(setvar 'CELTYPE "gas")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(command "pline" pause)
(end_utility)
)

(defun C:ue()
(begin_utility)
(setvar "CLAYER" "util-esmt")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "ByLayer")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(command "pline" pause)
(end_utility)
)

(defun C:el()
(begin_utility)
(setvar "CLAYER" "power")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "ByLayer")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(command "pline" pause)
(end_utility)
)

(defun C:cl ()
(begin_utility)
(setvar "CLAYER" "center line")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "ByLayer")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(setq p (getpoint))
(setq q (getpoint))
(setq s (entsel))
(COMMAND "offset" "l" "c" "t" s "m2p" p q )
(COMMAND "offset" "l" "s" "" "")
(end_utility)
)

(defun C:DB ()
(begin_utility)
(setvar "CLAYER" "BORE")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "DB")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 2)
(COMMAND "offset" "l" "c" pause PAUSE pause "")
(COMMAND "offset" "l" "s" "" "")
(setq width 2)
(setq b1 (entget (entlast)))
(setq c (assoc 43 b1))
(setq d (cons (car c) width))
(setq b2 (subst d c b1))
(setvar 'PLINEWID 0)
(entmod b2)
(end_utility)
(command "_.UNDO" "_E")
(setq *error* olderr)
(princ)
(princ)

)
(defun C:row ()
(begin_utility)
(setvar "CLAYER" "right of way")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "ByLayer")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 0)
(COMMAND "offset" "l" "c" pause PAUSE pause "")
(COMMAND "offset" "l" "s" "" "")
(end_utility)
)

(defun C:aerial()
(begin_utility)
(setvar "CLAYER" "route")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "aerial")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 1)
(command "pline" pause)
(end_utility)
)

(defun C:pt()
(begin_utility)
(setvar "CLAYER" "route")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "hidden")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 1)
(command "pline" pause)
(end_utility)
)

(defun C:trench()
(begin_utility)
(setvar "CLAYER" "trench")
(setvar 'CECOLOR "ByLayer")
(setvar 'CELTYPE "hdpe")
(setvar 'CELWEIGHT 0)
(setvar 'PLINEWID 1)
(command "pline" pause)
(end_utility)
)


(defun c:add_sheet ()
(setq addNumber (getint "specify the number to add on the sheet no"))
)


(defun c:CHML ()
	(setq diff addNumber)

	(REPEAT 2
		 
			
			(setq newNumber (+ diff (atoi (getvar "ctab"))))
					
			

				(if (< newNumber  10)
				
					(setq y (strcat "00" (itoa newNumber)))
					
					(if (< newNumber  100) 
						(setq y (strcat "0" (itoa newNumber)))
						(setq y (itoa newNumber))
					)
				)
			
			
			(setq m (CAR (ENTSEL)))
			(setq elem (entget m))
			(setq element (assoc 1 elem))
			(setq preveiousNumber (substr (cdr element) 1 (- (strlen (cdr element)) 3)))
			
			(setq z (strcat preveiousNumber y ))
			(setq lst2 (cons '1 z)) 
			(setq lst3 (subst lst2 element elem))
			(setq lst4 (entmod lst3))
			(setq diff (+ diff 2))
		 
	
	)
)

(defun c:CHMLP ()
	(setq diff addNumber)

	(REPEAT 2
		 
			
			(setq newNumber (+ diff (atoi (getvar "ctab"))))
					
			

				(if (< newNumber  10)
				
					(setq y (strcat "0" (itoa newNumber) "A"))
					
					
					(setq y (strcat (itoa newNumber) "A"))
					
				)
			
			
			(setq m (CAR (ENTSEL)))
			(setq elem (entget m))
			(setq element (assoc 1 elem))
			(setq preveiousNumber (substr (cdr element) 1 (- (strlen (cdr element)) 3)))
			
			(setq z (strcat preveiousNumber y ))
			(setq lst2 (cons '1 z)) 
			(setq lst3 (subst lst2 element elem))
			(setq lst4 (entmod lst3))
			(setq diff (+ diff 2))
		 
	
	)
)


(defun C:googlemap ( / filename shell)

(setvar "ucsfollow" 0)
(command "ucs" "world")  

(setq initialPoint (list 2.3314e+006 6.9566e+006 0.0)) ;; fortworth

(setq initialLongitude -97.31954) ;; fortworth
(setq initialLatitude 32.74663)   ;; fortworth
(setq longitudeFactor 307570.94)
(setq latitudeFactor 362778)

(setq finalPoint (getpoint "specify point to see on google map"))
(setq finalLongitude (+ initialLongitude (/ ( - (car finalPoint) (car initialPoint)) 309550)))

(setq finalLatitude (+ initialLatitude (/ (- (cadr finalPoint) (cadr initialPoint)) latitudeFactor)))
(setq filename (strcat "http://maps.google.com/maps?q=loc:" (rtos finalLatitude) "," (rtos finalLongitude) "&z=8"))

  
  (setq shell
         (vla-getinterfaceobject
           (vlax-get-acad-object)
           "Shell.Application"
           )
        )
  (vlax-invoke-method shell 'Open filename)
  (vlax-release-object shell)

)


(defun C:gmap ( / filename shell)

(setvar "ucsfollow" 0)
(command "ucs" "world")  

(setq initialPoint (list 2.3314e+006 6.9566e+006 0.0)) ;; fortworth

(setq initialLongitude -97.31954) ;; fortworth
(setq initialLatitude 32.74663)   ;; fortworth
(setq longitudeFactor 307570.94)
(setq latitudeFactor 362778)

(setq finalPoint (getpoint "specify point to see on google map"))
(setq finalLongitude (+ initialLongitude (/ ( - (car finalPoint) (car initialPoint)) 309550)))

(setq finalLatitude (+ initialLatitude (/ (- (cadr finalPoint) (cadr initialPoint)) latitudeFactor)))
(setq filename (strcat "http://maps.google.com/maps?q=loc:" (rtos finalLatitude) "," (rtos finalLongitude) "&z=8"))

  
  (setq shell
         (vla-getinterfaceobject
           (vlax-get-acad-object)
           "Shell.Application"
           )
        )
  (vlax-invoke-method shell 'Open filename)
  (vlax-release-object shell)

)



(defun C:locatePoint ( / filename shell)

(command "ucsfollow" "1" )
(command "ucs" "W")

(setq initialPoint (list 2.33143e+006 6.95661e+006 0.0)) ;; fortworth

(setq initialLongitude -97.319549) ;; fortworth
(setq initialLatitude 32.746635)   ;; fortworth
(setq longitudeFactor 307570.94)
(setq latitudeFactor 363889.62)


(setq finalLatitude (getreal "insert Latitude of the point"))
(setq finalLongitude (getreal "insert Longitude of the point"))

(setq x_coordinate (+ (car initialPoint) (* (- finalLongitude initialLongitude) longitudeFactor)))
(setq y_coordinate (+ (cadr initialPoint) (* (- finalLatitude initialLatitude) LatitudeFactor)))
(setq finalPoint (list x_coordinate y_coordinate 0))

(command "zoom" "c" finalPoint "10000")

)


(defun C:latlong ( / filename shell)

(setvar "ucsfollow" 0)
(command "ucs" "world")  

(setq initialPoint (list 2.33134e+006 6.95657e+006 0.0)) ;; fortworth

(setq initialLongitude -97.319702) ;; fortworth
(setq initialLatitude 32.746555)   ;; fortworth
(setq longitudeFactor 303145.57)
(setq latitudeFactor 370490.04)

(setq finalPoint (getpoint "specify point to see on google map"))
(setq finalLongitude (+ initialLongitude (/ ( - (car finalPoint) (car initialPoint)) longitudeFactor)))

(setq finalLatitude (+ initialLatitude (/ (- (cadr finalPoint) (cadr initialPoint)) latitudeFactor)))
(princ)
(setq latitudelongitude (strcat (rtos finalLatitude) "," (rtos finalLongitude)))

(vlax-invoke
    (vlax-get (vlax-get (vlax-create-object "htmlfile") 'ParentWindow) 'ClipBoardData)
    'setData
    "TEXT"
    latitudelongitude
)

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;LISP FROM FIELD NOTE PROCESSOR
;;Code
";;This Autolisp is written specialy to be used with the Field Note Processor program.
;;But it could also be used independently to insert blocks manually at specific 
;;points relative to a refernce which usualy is the 'Baseline'.
;;It is convinient to insert blocks using scripts because the program asks the user 
;;only for three inputs which are :""command"",""station"", and ""offset""once the baseline 
;;and the guideline are specified.these 
;;are object's name, station and offset.and no dialogue box and pause are required 
;;to pick points in the auto CAD window.
;;Solomon Tsegazeab Tessema
;;December 2014
;;Revised February 2016" 

(defun C:CAUTION ()
(setq oldlayer (getvar "clayer"))
(setvar "CLAYER" "CAUTION-NOTE")
(command "_.-insert" "CAUTION" "R" "90" pause "" "" "")
(command "ROTATE" "L" "" "@" PAUSE)
(end)
)

(defun c:NT ()
(setq NOTE (getstring "Specify Note"))
(station)
(setvar "CLAYER" "GUIDELINES")

(setvar "attreq" 0)
(setvar "attdia" 1)
(command "_.-insert" "NT" qt "" "" ( - (/ (* 180 (angle '(0 0 0) deriv) ) pi) 90))

(changeAttribValue (entlast) "NOTE" "NOTE" NOTE)

(end)
)
(defun c:PDG ()
(setvar "CLAYER" "PROP-GUY-AND-ANCHOR")
(command "_.-insert" "P-DOWNGUY-ANCHOR" pause "0.8" "" "")
(command "ROTATE" "L" "" "@" PAUSE)
(setvar "CLAYER" oldlayer)
)

(defun c:PHH ()
(setq oldlayer (getvar "clayer"))
(setvar "CLAYER" "ZAYO")
(command "_.-insert" "ZAYO-HH" pause "1" "" "")
(command "ROTATE" "L" "" "@" PAUSE)
(setvar "CLAYER" oldlayer)
)

