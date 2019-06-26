;Altera layer de objejos listados passados como argunmento
(defun ChangeLayObjs(objList layer corLay)
	(vl-load-com)
	(setq activeDocument (vla-get-activedocument (vlax-get-acad-object)))
	(setq LayerTable (vla-get-layers activeDocument))

	(setq tipoObjList (type objList))
	(cond
		((= tipoObjList 'PICKSET)
		 (setq objList (LM:SS->LIST objList))
		 (setq objList
						(mapcar
							'(lambda (o)
								 (vlax-ename->vla-object o)
								 )
							objList
							)
					 )
		 )

		((= tipoObjList 'ENAME)
		 (setq objList (list (vlax-ename->vla-object objList)))
		 )

		((= tipoObjList 'LIST)
		 (setq objList
						(mapcar
							'(lambda (o)
								 (vlax-ename->vla-object o)
								 )
							objList
							)
					 )
		 )
		)

	;Se a layer não existe
  (if (not (tblsearch "LAYER" layer))
    (progn
      ;Cria-a
      (vla-add LayerTable layer)
      (setq layer (vla-item LayerTable layer))
      ;e configura a cor
      (vla-put-color layer corLay)
      )
    (setq layer (vla-item LayerTable layer))
    )

		;Pega estado inicial da layer
	  (setq LockLayerInicial (vla-get-lock layer))
		;Destranca
  	(vla-put-lock layer :vlax-false)

	(foreach obj objList
		(vla-put-layer obj (vla-get-Name layer))
		(vla-put-color obj acByLayer)
		)

	(vla-put-lock layer LockLayerInicial)
	
	)