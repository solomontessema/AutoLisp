;;AutoLisp to place street objects based on a specified baseline.
;;Instruction with the sample drawing "Sample Drawing.dwg"
;;  1. Type "BL" and select the purple baseline
;;	2. Type "PlaceObject" to start the command.
;;  3. It will prompt you to Specify Object, type "POLE" as an example.
;;  4. It will prompt you to specify Station, type "145" as an example.
;;  5. It will prompt you to specify offset, type "10" as an example.
;;  Result: It will place a "Pole" symbole at station 145 offset 10.

;;Solomon Tessema
;;December 2014
;;Revised March 2024

(defun annotation ()
(defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
	(if oldlayer (setvar "CLAYER" oldlayer))
	(if oldtext (setvar "TEXTSTYLE" oldtext))
	(setvar "CECOLOR" "BYLAYER")
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

(setq rt (vlax-curve-getclosestpointto *guidline qt))        
(setq a2 (angle qt rt))                                       
(setq angDEG (/ (* 180 a2) PI))
(setq sta100 (itoa (/ s 100)))                               
(setq sta1 (itoa (rem s 100) ))                               

(if (or (= sta1 "0") (= sta1 "1") (= sta1 "2") (= sta1 "3") (= sta1 "4") (= sta1 "5") (= sta1 "6") (= sta1 "7") (= sta1 "8") (= sta1 "9") )
(setq sta1 (strcat "0" sta1))
(setq sta1 sta1)
)

(
if (> os 0)
(setq dir "RT ") ;; Direction text, right or left             
(setq dir "LT ")
)

(setq os (itoa (abs os)  ))

(if (or (= *BLtype "B") (= *BLtype "b"))
(setq *BLtype "BOC")
(if (or (= *BLtype "E") (= *BLtype "e"))                      
(setq *BLtype "EOP")
(if (or (= *BLtype "C") (= *BLtype "c")) 
(setq *BLtype "C/L")
(if (or (= *BLtype "R") (= *BLtype "r"))
(setq *BLtype "R/L") 
(setq *BLtype "B/L")
)
)

)

)

(if (or (= *GLdirection "L") (= *GLdirection "l"))            
(setq angDEG angDEG)
(setq angDEG (+ angDEG 180))
)                                                            


(setq st (polar rt a2 17))
(setq st2 (polar rt a2 22))
(setq tt (polar st (- a2 (/ PI 2)) 4))  
(setq tt2 (polar st2 (- a2 (/ PI 2)) 4))                
(setq ut (polar st (- a2 (/ PI 2)) 8))
(setq vt (polar st (- a2 (/ PI 2)) 12))
(setq wt (polar st (- a2 (/ PI 2)) 16))
(setq xt (polar st (- a2 (/ PI 2)) 20))
(setq yt (polar st (- a2 (/ PI 2)) 24))

(if (OR (= *GLdirection "R") (= *GLdirection "r"))


(setq st (polar rt a2 -17) tt (polar st (- a2 (/ PI 2)) -4)  ut (polar st (- a2 (/ PI 2)) -8)  vt (polar st (- a2 (/ PI 2)) -12) wt (polar st (- a2 (/ PI 2)) -16) xt (polar st (- a2 (/ PI 2)) -20) yt (polar st (- a2 (/ PI 2)) -24))
                      


(princ)



)

(setq first-line-text (strcat "STA " sta100 "+" sta1 " " objectNAME ))


(setq second-line-text (strcat  os "' O/S " dir *BLtype))
(setvar "CLAYER" "ANNOTATION")
(
if (= objectNAME "\t  OWNER POLE # UNK. (00)")
(setvar "TEXTSTYLE" "TEXT-BOLD")
(setvar "TEXTSTYLE" "TEXT")
)

(command "MTEXT" rt "R" angDEG "w" "200"  first-line-text "")
(command "explode" "L" "")
(if (/= objectName " EDGE OF ROAD")
(command "MTEXT" tt2 "R" angDEG "w" "200"  second-line-text "")
(princ))
(command "explode" "L" "")
(end)
)

(defun C:BL ()
(setvar "ucsfollow" 0)
(command "ucs" "world")                                               
(setq *baseline (car (entsel "\nselect baseline entity\n\n")))  
)

(defun C:BLTYPE ()                                                
(setq *BLTYPE  (getstring "\n Baseline  Boc Eop R/l C/l:"))   
)

(defun end ()
(setvar "CLAYER" oldlayer)
(setvar "osmode" oldsnap)
(setvar "TEXTSTYLE" oldtext)
(setvar "CECOLOR" "BYLAYER")

)

(defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
	(if oldlayer (setvar "CLAYER" oldlayer))
	(if oldtext (setvar "TEXTSTYLE" oldtext))
	(setvar "CECOLOR" "BYLAYER")
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

(defun C:GL ()
(setvar "ucsfollow" 0)
(command "ucs" "world")                                               
(setq *guidline (car (entsel "\nselect guideline entity\n")))  
)

(defun C:GLDIRECTION ()                                                
(setq *GLDIRECTION  (getstring "\n Baseline  Boc Eop R/l C/l:"))
 
)

(defun place()
(setq qt (getpoint "specify point to insert"))
)

(defun station ()

                                       

(defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
	(if oldlayer (setvar "CLAYER" oldlayer))
	(if oldtext (setvar "TEXTSTYLE" oldtext))
	(setvar "CECOLOR" "BYLAYER")
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
(if (= stationmode "Place")
(place)
(stationOffset)
)
)

(defun station2 ()                                              


(defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
	(if oldlayer (setvar "CLAYER" oldlayer))
	(if oldtext (setvar "TEXTSTYLE" oldtext))
	(setvar "CECOLOR" "BYLAYER")
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

                                        
(setq oldcolor (getvar 'CECOLOR))
(setq oldlayer (getvar "CLAYER"))
(setq oldsnap (getvar "osmode")) 
(setq oldtext (getvar "TEXTSTYLE"))
(setvar "osmode" 0)
                         

     (
      if (= *baseline nil)
     (setq *baseline (car (entsel "\nselect baseline entity\n")))
         
     (princ)
     )
     
(Setq s (getreal "Station"))
(setq os1 (getint "offset1"))
(setq os2 (getint "offset2"))
(setq pt (vlax-curve-getpointAtDist *baseline s))             
(setq par (vlax-curve-getparamatpoint *baseline pt))
(setq deriv (vlax-curve-getfirstderiv *baseline par))         
(setq angrad  (- (angle '(0 0 0) deriv) (/ pi 2) ))           

(setq qt1 (polar pt angrad os1))
(setq qt2 (polar pt angrad os2))                         


)

(defun c:stationmode ()
(initget "Place Station")

    (setq stationmode (GETKWORD "\n Specify  station mode
     [
Place/
Station & offset
] "
          )
    )
)

(defun stationOffset ()
(setq oldcolor (getvar 'CECOLOR))
(setq oldlayer (getvar "CLAYER"))
(setq oldsnap (getvar "osmode")) 
(setq oldtext (getvar "TEXTSTYLE"))
(setvar "osmode" 0)
                         

     (
      if (= *baseline nil)
     (setq *baseline (car (entsel "\nselect baseline entity\n")))
         
     (princ)
     )
    
(Setq s (getreal "Station"))
(setq os (getint "offset"))
(setq pt (vlax-curve-getpointAtDist *baseline s))             
(setq par (vlax-curve-getparamatpoint *baseline pt))
(setq deriv (vlax-curve-getfirstderiv *baseline par))         
(setq angrad  (- (angle '(0 0 0) deriv) (/ pi 2) ))           
(setq qt (polar pt angrad os))       
                      
)

(defun C:TEXTVAR ()                                       
(setq textvar (getint "Specify text variable\n"))           
)
                                             
(defun C:VP (/ line1 line2 line3 line4 int1 int2)

   (defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
 

(setq oldcolor (getvar 'CECOLOR))
(setq oldlayer (getvar "CLAYER"))
(setq oldsnap (getvar "osmode"))
(setq oldtext (getvar "TEXTSTYLE"))

(setvar "osmode" 0)                                                    


(setvar "CLAYER" "VIEWPORT")
(setvar "ucsfollow" 0)
(command "ucs" "world") 
(setq length (getint "Specify the length of the viewport"))
(setq width (getint "Specify the width of the viewport"))
(setq *baseline (car (entsel "\nselect baseline entity\n")))
(Setq s 0)
(Setq layoutname 1)
(setq pt1(vlax-curve-getpointAtDist *baseline s))
(while (/= pt1 nil)
(Setq t (+ s length)) 
(setq pt1(vlax-curve-getpointAtDist *baseline S))
(setq pt2(vlax-curve-getpointAtDist *baseline t))
(setq par1 (vlax-curve-getparamatpoint *baseline pt1))
(setq par2 (vlax-curve-getparamatpoint *baseline pt2))
(setq der1 (vlax-curve-getfirstderiv *baseline par1))
(setq der2 (vlax-curve-getfirstderiv *baseline par2))
(setq ang1 (- (angle '(0 0 0) der1) (/ pi 2) ))
(setq ang2 (- (angle '(0 0 0) der2) (/ pi 2) ))

(setq qt (polar pt1 ang1 (/ width 2)))

(setq st (polar pt2 ang2 (/ width 2)))

(command "line" pt1 qt "")
(setq line1 ( entlast))



(command "line" qt st "") 
(setq line2 ( entlast))



(command "line" pt2 st "")
(setq line3 ( entlast))



(command "offset" width line2 pt1 "")
(setq line4 ( entlast))



(vl-load-com) 
  (setq ent1 (vlax-ename->vla-object line1) 
ent2 (vlax-ename->vla-object line2) 

ent3 (vlax-ename->vla-object line3) 

ent4 (vlax-ename->vla-object line4) 
  ) 
  (setq int1 (vlax-safearray->list 
    (vlax-variant-value 
      (vla-IntersectWith ent3 ent4 acExtendBoth) 
    ) 
  ) 
)

(setq int2 (vlax-safearray->list 
    (vlax-variant-value 
      (vla-IntersectWith ent1 ent4 acExtendBoth) 
    ) 
  ) 

)

  



(command "pline" qt st int1 int2 "c" )
(command "erase" line1 line2 line3 line4 "")



(Setq s (+ s length))
)

)

;;============================================================================================================
(defun c:PlaceObject()

(if (= BSCALE NIL)
	(SETQ BSCALE 200)
)
(defun *error* ( msg )
        (if oldsnap (setvar 'osmode oldsnap))
	(if oldlayer (setvar "CLAYER" oldlayer))
	(if oldtext (setvar "TEXTSTYLE" oldtext))
	(setvar "CECOLOR" "BYLAYER")
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
(setq oldcolor (getvar 'CECOLOR))
(setq oldlayer (getvar "CLAYER"))
(setq oldsnap (getvar "osmode")) 
(setq oldtext (getvar "TEXTSTYLE"))
(setvar "osmode" 0)

(setvar "CLAYER" "GUIDELINES")
(setvar "attreq" 0)
(setvar "attdia" 1)
(setvar "qaflags" 0)

(while
(setq NOTE (getstring "specify object"))
	(if (OR  (= NOTE "FM")  (= NOTE "fm"))
	(setq NOTE (strcat (getstring "specify type of marker") " MKR"))
	)
(Setq s (getreal "Station"))
(setq os (getint "offset"))
(setq pt (vlax-curve-getpointAtDist *baseline s))             
(setq par (vlax-curve-getparamatpoint *baseline pt))
(setq deriv (vlax-curve-getfirstderiv *baseline par))         
(setq angrad  (- (angle '(0 0 0) deriv) (/ pi 2) ))           
(setq qt (polar pt angrad os))
  
(Setq station (itoa (fix s)))
(Setq offset (itoa os))


(command "_.-insert" "NT-OBJECTS" qt 200 ""  (/ (* 180 (angle '(0 0 0) deriv) ) pi) )

(changeAttribValue (entlast) "OBJECT" "OBJECT" (strcase NOTE nil))
(changeAttribValue (entlast) "STATION" "0" station)
(changeAttribValue (entlast) "OFFSET" "0" offset)

(princ)
)

(setvar "CLAYER" oldlayer)
(setvar "osmode" oldsnap)
(setvar "QAFLAGS" 0)
(setvar "TEXTSTYLE" oldtext)
(setvar "CECOLOR" "BYLAYER")

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


