(defun HighlightAttributes(blk_QuadCargas blk_Trifilar blk_DG)
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

	(mapcar '(lambda (a)
						 (vla-getBoundingBox (vlax-ename->vla-object a) 'pt1 'pt2)
						 (setq pt1 (vlax-safearray->list pt1)
									 pt2 (vlax-safearray->list pt2)
									 )

						 (command "RECTANG" pt1 pt2)
						 )
					AttsList
					)							 
	)