(defun InsereCircuitos()

	;Lista que será usada no momento de decidir qual o próximo circuito. 
	(setq listaRequisicao (list (list "S" "S+T" "R+S+T") (list "T" "R+T" "R+S+T") (list "R" "R+S" "R+S+T")))

	;Pega os pontos iniciais das posições das fases
	(setq props_Trifilar (LM:GetDynProps obj_Trifilar))
	(setq pt_R (list (+ (car pt_Trifilar) (cdr (assoc "C1R X" props_Trifilar))) (+ (cadr pt_Trifilar) (cdr (assoc "C1R Y" props_Trifilar)))))
	(setq pt_S (list (+ (car pt_Trifilar) (cdr (assoc "C1S X" props_Trifilar))) (+ (cadr pt_Trifilar) (cdr (assoc "C1S Y" props_Trifilar)))))
	(setq pt_Ref (list (+ (car pt_Trifilar) (cdr (assoc "REFERENCIA X" props_Trifilar))) (+ (cadr pt_Trifilar) (cdr (assoc "REFERENCIA Y" props_Trifilar)))))
	
	;Pega a coluna da tabela onde se encontram as fases
	(setq coluna
				 (cond
					 ((vl-position "Fases" (car dadosTab)))
					 ((vl-position "FASES" (car dadosTab)))
					 ;((vl-position "Fezes" (car dadosTab)))
					 )
				)

	;Define a visibilidade do diagrama trifilar de acordo 
	;com o número de fases, e os pontos de inserção dos   
	;primeiros circuitos em suas fases.                   
	(if (= (strlen (nth coluna (last dadosTab))) 3)
		(progn
			(setq pt_TN (list (+ (car pt_Trifilar) (cdr (assoc "C1S X" props_Trifilar))) (+ (cadr pt_Trifilar) (cdr (assoc "C1S Y" props_Trifilar)))))
			(setq pt_NPe (list (+ (car pt_Trifilar) (cdr (assoc "C1TN X" props_Trifilar))) (+ (cadr pt_Trifilar) (cdr (assoc "C1TN Y" props_Trifilar)))))
			(setq pt_Pe (list (+ (car pt_Trifilar) (cdr (assoc "C1NPe X" props_Trifilar))) (+ (cadr pt_Trifilar) (cdr (assoc "C1NPe Y" props_Trifilar)))))
			(LM:SetVisibilityState obj_Trifilar "2F 1L")
			)
		
		(progn
			(setq pt_TN (list (+ (car pt_Trifilar) (cdr (assoc "C1TN X" props_Trifilar))) (+ (cadr pt_Trifilar) (cdr (assoc "C1TN Y" props_Trifilar)))))
			(setq pt_NPe (list (+ (car pt_Trifilar) (cdr (assoc "C1NPe X" props_Trifilar))) (+ (cadr pt_Trifilar) (cdr (assoc "C1NPe Y" props_Trifilar)))))
			(setq pt_Pe (list (+ (car pt_Trifilar) (cdr (assoc "C1Pe X" props_Trifilar))) (+ (cadr pt_Trifilar) (cdr (assoc "C1Pe Y" props_Trifilar)))))
			(LM:SetVisibilityState obj_Trifilar "3F 1L")
			)
		)

	;Distância entre um circuito e outro
	(setq dist (distance pt_Pe pt_Ref))
	(setq pt pt_Pe)
	
	;Cria uma lista de associação da fase e dor circuito a que pertence
	(setq fasesList (list))
	(foreach linha (cddr dadosTab)
		(setq fasesList (append
											fasesList
											(list (cons (nth coluna linha) (nth 0 linha)))
											)
					)
		)

	;A última linha da tabela não é necessária, pois é o TOTAL
	(setq fasesList (vl-remove (last fasesList) fasesList))

	;Número mínimo de "reserva" segundo a norma 5410.
	(cond
		((<= (length fasesList) 6) (setq nReserva 2))

		((and (<= 7 (length fasesList)) (<= (length fasesList) 12)) (setq nReserva 3))

		((and (<= 13 (length fasesList)) (<= (length fasesList) 30) (setq nReserva 4)))

		((> (length fasesList) 30) (setq nReserva (fix (* (length fasesList) 0.15))))
		)
		 
	;O primeiro circuito deve ser, preferencialmente, iniciado com "R",       
	;se não houver um deste, começar com "S".                                 
	;O bloco a seguir verifica a posição dos primeiros circuitos de cada tipo:
	;(R, S, T, R+S, R+T, R+S+T, S+T).                                         
	;Se o circuito verificado não existe na lista,                            
	;atribui a respectiva variável com o valor de 10000 apenas                
	;para o fim de comparação de posição.                                     
	(setq R (cond ((vl-position (assoc "R" fasesList) fasesList)) (10000)))
	(setq RS (cond ((vl-position (assoc "R+S" fasesList) fasesList)) (10000)))
	(setq RT (cond ((vl-position (assoc "R+T" fasesList) fasesList)) (10000)))
	(setq RST (cond ((vl-position (assoc "R+S+T" fasesList) fasesList)) (10000)))
	(setq S (cond ((vl-position (assoc "S" fasesList) fasesList)) (10000)))
	(setq ST (cond ((vl-position (assoc "S+T" fasesList) fasesList)) (10000)))

	;Se existe algum circuito "R...",     
	;pega o primeiro que aparece na lista.
	(if (or (< R 10000) (< RS 10000) (< RT 10000) (< RST 10000))
		(setq faseAtual (nth (min R RS RT RST) fasesList))

		;Se não, pega o primeiro circuito "S" ou "S+T"
		;que aparece.                                 
		(setq faseAtual (nth (min S ST) fasesList))
		)

	(setq distCirc
				(cond
					((= (strlen (car faseAtual)) 1) (* dist 2.5))
					((= (strlen (car faseAtual)) 3) (* dist 3.5))
					((= (strlen (car faseAtual)) 5) (* dist 4.5))
					)
				)

	(acet-ui-progress "Trabalhando..." (length fasesList))
	(setq iBar 0)

	(setq nDisjuntor 0)
		
	;Enquanto houver fases na lista "fasesList"
	;ou enquanto houver circuito reservas      
	;para ser inserido, executa o loop.        
	(while (or fasesList (>= nReserva 0))

		(if (< nReserva 0)
			(progn
				(prompt "\nErro. Comando cancelado.")
				(RestauraVariaveis)
				)
			)

		(if (member (cdr faseAtual) numsDDR) (setq distCirc (* (/ 5 4) distCirc)))

		(command
			"._INSERT"
			(strcat caminho "\\BLOCOS\\BLOCO-CIRCUITO-TRIFILAR.dwg")
			(setq pt (polar pt 0 distCirc)) ;******************************************************************************************
			"" "" ""
			)
		(setq circ (entlast))
		(setq objCirc (vlax-ename->vla-object circ))
		(vla-ScaleEntity objCirc (vlax-3d-point pt) escalaBloco)

		;Se não foi encontrada a fase requisitada na lista,      
		;quer dizer que o circuito usado é um reserva,           
		;então subtrai-se 1 da quantidade necessária de reservas.
		(if (not faseAtual)
			(setq nReserva (- nReserva 1))
				
			;Se foi encontrado a fase requisitada na lista,  
			;nomeia o circuito inserido e o remove da lista. 
			;São feitas as nomeações dos 3 atributos que o   
			;bloco pode tomar dependendo da sua visibilidade,
			;além nomear os outros atributos necessários.
			
			(progn
				(setq fasesList (vl-remove faseAtual fasesList))
				(LM:vl-SetAttributeValue objCirc "CIRC." (strcat "CIRC. " (cdr faseAtual)))
				(LM:vl-SetAttributeValue objCirc "CIRC.." (strcat "CIRC. " (cdr faseAtual)))
				(LM:vl-SetAttributeValue objCirc "CIRC..." (strcat "CIRC. " (cdr faseAtual)))
				(LM:vl-SetAttributeValue objCirc "XXA"
					(strcat
						(itoa (atoi (nth (vl-position "DISJ (A)" (car dadosTab)) (nth (atoi (cdr faseAtual)) dadosTab))))
						"A"))
				)
			)

		(LM:vl-SetAttributeValue objCirc "DX" (strcat "D" (itoa (setq nDisjuntor (1+ nDisjuntor)))))
		
		(cond
			;Se a fase do circuito tem apenas 1 caractere (1 fase), 
			;ou não foi definido faseAtual (circuito Reserva).      
		((or (not faseAtual) (= (strlen (car faseAtual)) 1))

		   ;Se não foi definido faseAtual,             
		   ;quer dizer que o circuito atual é Reserva. 
			 (if (not faseAtual)

				 (progn
				 	(LM:SetVisibilityState objCirc "R1")
					(setq distCirc (* dist 2))
					)
						
				 (if (member (cdr faseAtual) numsDDR)
					 (progn
						 (LM:SetVisibilityState objCirc "DDR (1F)")
						  ;Distância entre um circuito de apenas uma fase e outro qualquer. 
						 (setq distCirc (* dist 3.5))
						 (vla-move objCirc (vlax-3d-point pt) (vlax-3d-point (polar pt 0 (/ distCirc 3.0))))
						 )
					 (progn
						 (LM:SetVisibilityState objCirc "CIRC. (1F)")
						 (setq distCirc (* dist 2.5))
						 )
					 )
				 )

			 (cond
				 ((or (= faseAnt "T") (= (car faseAtual) "R"))
					(LM:SetDynPropValue objCirc "Distance1" (distance pt (list (car pt)
																																		 ;Se existe circuito R 
																																		 (cadr (if (> (atof cargaR) 0)
																																						 (progn (setq faseAnt "R") pt_R)
																																						 (progn (setq faseAnt "S") pt_S))))));Fase R
					)

				 ((or (= faseAnt "R") (= (car faseAtual) "S"))
					(LM:SetDynPropValue objCirc "Distance1" (distance pt (list (car pt)
																																		 ;Se existe circuito S 
																																		 (cadr (if (> (atof cargaS) 0)
																																						 (progn (setq faseAnt "S") pt_S)
																																						 (progn (setq faseAnt "T") pt_TN))))));Fase S 
					)

				 ((or (= faseAnt "S") (= (car faseAtual) "T"))
					(LM:SetDynPropValue objCirc "Distance1" (distance pt (list (car pt)
																																		 ;Se existe circuito T 
																																		 (cadr (if (> (atof cargaT) 0)
																																						 (progn (setq faseAnt "T") pt_TN)
																																						 (progn (setq faseAnt "R") pt_R))))));Fase T 
					)
				 )
			 )	

			;Se a fase do circuito tem apenas 3 caracteres (2 fases)
			((= (strlen (car faseAtual)) 3)
			 (if (member (cdr faseAtual) numsDDR)
				 (progn
				 	(LM:SetVisibilityState objCirc "DDR (2F)")
					(setq distCirc (* dist 5.0))
					;(command "MOVE" circ "" pt (strcat (rtos (/ distCirc 4.0) 2 2) "<0"))
					(vla-move objCirc (vlax-3d-point pt) (vlax-3d-point (polar pt 0 (/ distCirc 3.0))))
					)
				 (progn
					 (LM:SetVisibilityState objCirc "CIRC. (2F)")
					 (setq distCirc (* dist 3.5))
					 )
				 )

			 (cond
				 ((= (car faseAtual) "R+S")
					(LM:SetDynPropValue objCirc "Distance1" (distance pt (list (car pt) (cadr pt_R))))
					(LM:SetDynPropValue objCirc "Distance2" (distance pt (list (car pt) (cadr pt_S))))
					(setq faseAnt "S")
					)

				 ((= (car faseAtual) "S+T")
					(LM:SetDynPropValue objCirc "Distance1" (distance pt (list (car pt) (cadr pt_S))))
					(LM:SetDynPropValue objCirc "Distance2" (distance pt (list (car pt) (cadr pt_TN))))
					(setq faseAnt "T")
					)

				 ((= (car faseAtual) "R+T")
					(LM:SetDynPropValue objCirc "Distance1" (distance pt (list (car pt) (cadr pt_TN))))
					(LM:SetDynPropValue objCirc "Distance2" (distance pt (list (car pt) (cadr pt_R))))
					(setq faseAnt "R")
					)
				 )
			 )

			;Se a fase do circuito tem 5 caracteres, ou seja, é "R+S+T" (3 fases)
			((= (strlen (car faseAtual)) 5)
			 (if (member (cdr faseAtual) numsDDR)
				 (progn
				 	(LM:SetVisibilityState objCirc "DDR (3F)")
					(setq distCirc (* dist 5.5))
					;(command "MOVE" circ "" pt (strcat (rtos (/ distCirc 4.0) 2 2) "<0"))
					(vla-move objCirc (vlax-3d-point pt) (vlax-3d-point (polar pt 0 (/ distCirc 3.0))))
					)
				 (progn
					(LM:SetVisibilityState objCirc "CIRC. (3F)")
					(setq distCirc (* dist 4.5))
					)
				 )
			
			 (cond
				 ((= faseAnt "R")
					(LM:SetDynPropValue objCirc "Distance1" (distance pt (list (car pt) (cadr pt_S)))) ;Fase S
					(LM:SetDynPropValue objCirc "Distance2" (distance pt (list (car pt) (cadr pt_TN))));Fase T
					(LM:SetDynPropValue objCirc "Distance3" (distance pt (list (car pt) (cadr pt_R)))) ;Fase R
					(setq faseAnt "R")
					)

				 ((= faseAnt "S")
					(LM:SetDynPropValue objCirc "Distance1" (distance pt (list (car pt) (cadr pt_TN))));Fase T
					(LM:SetDynPropValue objCirc "Distance2" (distance pt (list (car pt) (cadr pt_R)))) ;Fase R
					(LM:SetDynPropValue objCirc "Distance3" (distance pt (list (car pt) (cadr pt_S)))) ;Fase S
					(setq faseAnt "S")
					)

				 ((= faseAnt "T")
					(LM:SetDynPropValue objCirc "Distance1" (distance pt (list (car pt) (cadr pt_R)))) ;Fase R
					(LM:SetDynPropValue objCirc "Distance2" (distance pt (list (car pt) (cadr pt_S)))) ;Fase S
					(LM:SetDynPropValue objCirc "Distance3" (distance pt (list (car pt) (cadr pt_TN))));Fase T
					(setq faseAnt "T")
					)
				 )
			 )
			)

	;; Bloco para setar os valores dos circuitos DDR
	(setq vis (cdr (assoc "Visibility1" (LM:GetDynProps objCirc))))
	(if (wcmatch vis "DDR*")
		(progn
			(LM:vl-SetAttributeValue objCirc "DDRX" (strcat "D" (itoa nDisjuntor)))
			(LM:vl-SetAttributeValue objCirc "30MA" "30MA")
			(LM:vl-SetAttributeValue objCirc "XXXA" (strcat
					(itoa (atoi (nth (vl-position "DISJ (A)" (car dadosTab)) (nth (atoi (cdr faseAtual)) dadosTab))))
					"A"))
			)
		)

		(setq proxFase
					 (cond
						 ((= faseAnt "R")
							(if (> (atof cargaS) 0)
								(nth 0 listaRequisicao)
								(nth 1 listaRequisicao)
								)
							)
						 
						 ((= faseAnt "S")
							(if (> (atof cargaT) 0)
								(nth 1 listaRequisicao)
								(nth 2 listaRequisicao)
								)
							)
						 
						 ((= faseAnt "T")
							(if (> (atof cargaR) 0)
								(nth 2 listaRequisicao)
								(nth 1 listaRequisicao)
								)
							)
						 )
					)

		;A 'faseAtual' é o primeiro circuito que aparece obedencendo a requisição
		;da variável "proxFase".
		(if fasesList
			(setq faseAtual
						 (nth
							 (min
								 (cond ((vl-position (assoc (nth 0 proxFase) fasesList) fasesList)) (10000))
								 (cond ((vl-position (assoc (nth 1 proxFase) fasesList) fasesList)) (10000))
								 (cond ((vl-position (assoc (nth 2 proxFase) fasesList) fasesList)) (10000))
								 )
							 fasesList
							 )
						)
			(setq faseAtual nil)
			)
		(acet-ui-progress (setq iBar (1+ iBar)))
		)
	(acet-ui-progress)
						 
	;Ajusta o comprimento da tabela "Diagrama Trifilar"
	(LM:SetDynPropValue obj_Trifilar "Distance1" (+ (distance (list (car pt_Trifilar) 0) (list (car pt) 0)) (* 2 distCirc)))

	(setq potVA (atof (nth (vl-position "POT. TOTAL. (VA)" (car dadosTab)) (last dadosTab))))

	;Divisão necessária para saber a correnteDG
	(if (= (substr nFases 1 1) "2")
		(setq correnteDG (/ potVA 220))
		(if (= (substr nFases 1 1) "3")
			(setq correnteDG (/ potVA 381))
			)
		)

	;Lista de valores padrões da correnteDG, que será usada no próximo passo 
	(setq padroesDG (list 40 50 63 80 100 125 150 175 200 225 250 300 400 500 630 800 1200 1600))
	
	;Função que encontra o número maior-igual ao correnteDG na lista padroesDG    
	(setq correnteDG (FindNum > correnteDG padroesDG));; substituído para "maior"
	
	;Inserir o circuito do disjuntor geral
	(setq auxList (LM:GetDynProps obj_Trifilar));Lista de propriedades dinâmicas do bloco diagrama trifilar

	;Pega a posição do ponto para inserção do Disjunto Geral
	(setq pt
				 (list
					 (+ (car pt_Trifilar) (cdr (assoc "DG X" auxList)))
					 (+ (cadr pt_Trifilar) (cdr (assoc "DG Y" auxList)))
					 )
					 )
	
	(command
			"._INSERT"
			(strcat caminho "\\BLOCOS\\BLOCO-CIRCUITO-TRIFILAR.dwg")
			pt
			"" "" ""
			)
	(setq circ (entlast))
	(setq objCirc (vlax-ename->vla-object circ))
	(vla-ScaleEntity objCirc (vlax-3d-point pt) escalaBloco)
	(LM:SetVisibilityState
		objCirc
		(cond
			((wcmatch nFases "2*") "DJ (2F)")
			((wcmatch nFases "3*") "DJ (3F)")
			)
		)
	(LM:vl-SetAttributeValue objCirc "DX" "DG")
	(LM:vl-SetAttributeValue objCirc "XXA" (strcat (itoa correnteDG) "A"))

	;Sigla/abreviação do titulo do Quadro de Cargas
	(setq abrev (vl-list->string (reverse (cdr (member 41 (reverse (cdr (member 40 (vl-string->list tituloQuadro)))))))))

	;Bloco para encontrar o valor da SEÇÃO NOMINAL----------------------------------------------
	(setq padroes1 (list 8 10 12 15.5 21 28 36 50 68 89 110 134 171 207 239 275 314 370 426 510 587 678 788 906));; lista de correntes que o cabo aguenta 
	(setq padroes2 (list 0.5 0.75 1 1.5 2.5 4 6 10 16 25 35 50 70 95 120 150 185 240 300 400 500 630 800 1000));; lista de seções nominais do cabo 

	;; p1 é a corrente que o cabo aguenta 
	;Encontrar um número maior-igual ao 'correnteDG' na lista 'padroes1'
	(setq p1 (FindNum > correnteDG padroes1)); substituido para "maior" 

	;*****************************************************************************************************************************************************************************

	;; p2 é a seção nominal do cabo                  
	;; ela é o elemento da lista "padroes2" que está 
	;; na mesma posição de p1 na lista "padroes1"    
	(setq p2 (rtos (nth (vl-position p1 padroes1) padroes2) 2 1))

	;;Este bloco foi desativado pois estava errado.               
	;;Encontrar um númeor maior-igual ao 'p1' na lista 'padroes2' 
	;;(setq p2 (rtos (FindNum >= p1 padroes2) 2 1))               
	
	;Se exite ponto '.' no número
	(if (vl-string-search "." p2)
		;Se o último número decimal é zero '0', remove-o junto com o ponto 
		(if (= (substr p2 (strlen p2) 1) "0") (setq p2 (substr p2 1 (- (strlen p2) 2))))
		;Substituir a vírgula ',' no lugar do ponto '.' 
		(setq p2 (vl-string-subst "," "." p2))
		)

	;para <= 10 pega o proprio valor 
	;de 16 até 35 é 16.
	(if (<= (atof p2) 10)
		(setq p2T p2)
		(if (<= (atof p2) 35)
			(setq p2T "16")

			(progn
				(setq p2T (rtos (FindNum > (/ (atof p2) 2) padroes2) 2 2))
				)
			)
		)
	
		;Se exite ponto '.' no número
		(if (vl-string-search "." p2T)
			;Se o último número decimal é zero '0', remove-o junto com o ponto 
			(if (= (substr p2T (strlen p2T) 1) "0") (setq p2T (vl-string-trim "." (substr p2T 1 (- (strlen p2T) 2)))))
			;Substituir a vírgula ',' no lugar do ponto '.' 
			(setq p2T (vl-string-subst "," "." p2T))
			)
	
	;--------------------------------------------------------------------------------------------
						
	(LM:vl-SetAttributeValue obj_Trifilar "ABREV." abrev)
	(LM:vl-SetAttributeValue obj_Trifilar "DIAGRAMA_TRIFILAR_-_NOME" (strcat "DIAGRAMA TRIFILAR - " abrev))
	(LM:vl-SetAttributeValue obj_Trifilar "2#XX+N(XX)_-_XXXXV_-_T(#XX)" (strcat "2#(" p2 ")+N(" p2 ")-750V-T(#" p2T ")"))
	(LM:vl-SetAttributeValue obj_Trifilar "3#XX+N(XX)_-_XXXXV_-_T(#XX)" (strcat "3#(" p2 ")+N(" p2 ")-750V-T(#" p2T ")"))
	(LM:vl-SetAttributeValue obj_Trifilar "VEM_DO_XXX-X" "VEM DO QFLT-G")
	
	)
			

			 
			 