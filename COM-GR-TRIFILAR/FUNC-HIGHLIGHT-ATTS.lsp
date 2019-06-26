(defun HighlightAttributes(blk_QuadCargas blk_Trifilar blk_DG)

	;; Records the old CECOLOR value for future restauration on error 
	(setq oldVars (append (list (cons "CECOLOR" (getvar "CECOLOR"))) oldVars))
	
	;; Get the list of all attributes of the DG (disjuntor geral) and trifilar block 
	;;  with your respective entity list.                                            
	(setq QuadCargas_AttsList (get_at_list blk_QuadCargas))
	(setq Trif_AttsList (get_at_list blk_Trifilar))
	(setq DG_AttsList (get_at_list blk_DG))
	(setq AttsList (list
									;; Entity name of the "NOTA1" attribute 
									(cdr (assoc -1 (cadr (assoc "NOTA1" QuadCargas_AttsList))))
									;; Entity name of the "NOTA4" attribute
									(cdr (assoc -1 (cadr (assoc "NOTA4" QuadCargas_AttsList))))
									;; Entity name of the "XX-MÓDULOS" attribute
									(cdr (assoc -1 (cadr (assoc "XX-MÓDULOS" QuadCargas_AttsList))))
									;; Entity name of the "XXXX-SCHNEIDER" attribute
									(cdr (assoc -1 (cadr (assoc "XXXX-SCHNEIDER" QuadCargas_AttsList))))
									;; Entity name of the "IXX" attribute
									(cdr (assoc -1 (cadr (assoc "IXX" QuadCargas_AttsList))))
									;; Entity name of the "IXX" attribute
									(cdr (assoc -1 (cadr (assoc "IXX" QuadCargas_AttsList))))
									;; Entity name of the "ICC" attribute
									(cdr (assoc -1 (cadr (assoc "ICC" QuadCargas_AttsList))))
									;; Entity name of the "3#XX+N(XX)_-_XXXXV_-_T(#XX)" attribute 
									(cdr (assoc -1 (cadr (assoc "3#XX+N(XX)_-_XXXXV_-_T(#XX)" Trif_AttsList))))
									;; Entity name of DG (disjuntor geral) block 
									blk_DG
									)
				)

	(setq oldColor (getvar "CECOLOR"))
	(setvar "CECOLOR" "10")
	(mapcar '(lambda (a)
						 (vla-getBoundingBox (vlax-ename->vla-object a) 'pt1 'pt2)
						 (setq pt1 (vlax-safearray->list pt1)
									 pt2 (vlax-safearray->list pt2)
									 )

						 ;; Move the points a % more distances
						 (setq pt1 (polar pt1 (angle pt2 pt1) (* (distance pt1 pt2) 0.2)))
						 (setq pt2 (polar pt2 (angle pt1 pt2) (* (distance pt1 pt2) 0.2)))

						 (command "RECTANG" pt1 pt2)
						 )
					AttsList
					)
	(setvar "CECOLOR" oldColor)
	)