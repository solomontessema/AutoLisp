;; AutoLisp to create station marks on a baseline.
;;	1. Appload this Autolisp script type TM. 
;;  2. Type "TM" to call the command.
;;  3. Select "UP" from the options
;;  4. Specify initial number. Type "1"
;;  5. Select Object to measure. (Select the purple baseline on the drawing.)

;;Solomon Tessema
;;December 2014
;;Revised March 2024

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

