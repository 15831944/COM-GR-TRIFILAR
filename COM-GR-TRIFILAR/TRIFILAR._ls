;Weiglas Ribeiro                                              
;TRIFILAR                                                     
;Pede ao usu�rio para selecionar a tabela "Quadro de Cargas"  
;E insere os blocos "Quadro de Cargas" e "Diagrama Trifilares"
;com as informa��es retiradas da tabela selecionada.          
;A fun��o "SelecionaTabela" se encontra em                    

;nFases, correnteDG n�o devem ser variaveies privadas!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defun C:TRIF(/ ;|doc retorno lHor lVer dadosTab i linha dado j ss tituloQuadro
								auxLinhas desenhoTodo1 pt1_LHorLast pt2_LHorLast pt1_LVer1
								pt2_LVer1 listTextosTabela blk_QuadCargas ObjBlk_QuadCargas
								desenhoTodo2 textoReferencia pBox1 pBox2 divisor penultimaLH
								dividendo escalaBloco X1Cargas X2Cargas Y1Cargas Y2Cargas potW
								potVA fatorPA queda maiorQueda col cargaR cargaS cargaT cargaVA
								cargaW Dis1_DynBlock Dis2_DynBlock distX_QuadCargas p1 pt_Trifilar
								blk_Trifilar obj_Trifilar|;)

	(setq doc (vla-get-activeDocument (vlax-get-acad-object)))

	(vla-startUndoMark doc)

	(command "COLOR" "BYLAYER")

	;Fun��o que pede ao usu�rio para selecionar a tabela.
	;Formata-a e retorna uma lista contendo as linhas que
	;a desenham e uma lista contendo os dados.           
	(setq retorno (SelecionaTabela))
	(setq lHor (car (nth 0 retorno)))
	(setq lVer (cadr (nth 0 retorno)))
	(setq dadosTab (nth 1 retorno))

	;Fazer todos os dados em letra maiuscula
	(setq i 0)
	(repeat (length dadosTab)
		(setq j 0)
		(setq linha (nth i dadosTab))
		(repeat (length (nth i dadosTab))
			(setq dado (nth j linha))
			(setq linha (subst (strcase dado) dado linha))
			(setq j (1+ j))
			)
		
			(setq dadosTab (subst linha (nth i dadosTab) dadosTab))
			(setq i (1+ i))
		)
	(setq linha nil i nil j nil dado nil)


	(setq ss (ssname (ssget "_C" ptUser1 ptUser2 (list (cons 0 "TEXT,MTEXT") (cons 1 "Quadro de Cargas*"))) 0))
	(setq tituloQuadro (strcase (cdr (assoc 1 (entget ss)))))
	(entdel ss)

	;; Pega os n�meros da tabela que est�o em cor amarela. Ser� usado em "Insere circuitos"
	(setq numsDDR (LM:SS->LIST (ssget "_C" ptUser1 ptUser2 '((1 . "[0-9]*") (-4 . "<OR") (62 . 2) (62 . 50) (-4 . "OR>")))))
	;; Substitui as ENAMES pelo texto dos objetos (o n�mero). 
	(setq numsDDR (mapcar '(lambda (en) (cdr (assoc 1 (entget en)))) numsDDR))

	;Vari�vel para selecionar linhas espec�ficas.
	;Num primeiro momento, seleciona as linhas   
	;que ir�o para a layer "LANG-DI-LINHAS 1".   
	(setq auxLinhas
				 ;(append
					 (list (nth 0 lHor)
								 (nth 1 lHor)
								 (nth 2 lHor)
								 (nth 0 (reverse lHor))
								 (nth 1 (reverse lHor))
								 )
					 ;lVer
					; )
				)

	;Altera a layer das linhas selecionadas da lista auxLinhas
	(ChangeLayObjs auxLinhas "LANG-DI-LINHAS 1" 113)

	;Agora ser�o selecionados as linhas que ir�o para a layer "LANG-DI-LINHAS 3"
	(setq auxLinhas (append lHor lVer))
	(foreach l
					(list
						 (nth 0 lHor)
						 (nth 1 lHor)
						 (nth 2 lHor)
						 (nth 0 (reverse lHor))
						 (nth 1 (reverse lHor))
						 )
	 
	 (setq auxLinhas (vl-remove l auxLinhas))
	 )

	(ChangeLayObjs auxLinhas "LANG-DI-LINHAS 3" 8)

	;Este selection set ser� usado mais pra frente,
	;ap�s explodir o bloco "QUADRO DE CARGAS"       
	(setq desenhoTodo1 (ssget "_X"))

	;Pontos 1 e 2 da �ltima linha horizontal da tabela
	(setq pt1_LHorLast (cdr (assoc 10 (entget (last lHor)))))
	(setq pt2_LHorLast (cdr (assoc 11 (entget (last lHor)))))

	;Ponto 2 da primeira linha horizontal
	(setq pt2_LHor1 (cdr (assoc 11 (entget (car lHor)))))

	;Pontos 1 e 2 da primeira linha vertical da tabela.
	;O ponto 2 "pt2_LVer1" � o ponto de inser��o       
	;do bloco "QUADRO DE CARGAS".                      
	(setq pt1_LVer1 (cdr (assoc 10 (entget (nth 0 lVer)))))
	(setq pt2_LVer1 (cdr (assoc 11 (entget (nth 0 lVer)))))
	(command "INSERT" (strcat caminho "\\BLOCOS\\BLOCO-QUADRO-DE-CARGAS.dwg") pt2_LVer1 "" "" "")

	;; Mudar a layer dos textos da tabela
	(setq listTextosTabela (LM:SS->LIST (ssget "_W" pt1_LVer1 pt2_LHorLast '((0 . "TEXT,MTEXT")))))
	(ChangeLayObjs listTextosTabela "LANG-GR-TEXTO 2" 9)

	;A partir daqui, inicia-se o processo de escalamento do bloco
	;Objeto Bloco QUADRO DE CARGAS.                              
	(setq blk_QuadCargas (entlast))
	(setq ObjBlk_QuadCargas (vlax-ename->vla-object blk_QuadCargas))
	
	(vla-explode ObjBlk_QuadCargas)
	(entdel blk_QuadCargas)

	;Este selection set ser� comparado com o "desenhoTodo1"
	;para remover os objetos do bloco explodido.           
	(setq desenhoTodo2 (ssget "_X"))

	;Pegaremos a altura da caixa do texto refer�ncia para calcular o fator de escala
	(setq textoReferencia
				 (ssget "_C"
								(polar ptUser1 (angle ptUser2  ptUser1) (* (distance ptUser1 ptUser2) 1.5))
								(polar ptUser2 (angle ptUser1  ptUser2) (* (distance ptUser1 ptUser2) 1.5))
								(list (cons 0 "TEXT,MTEXT") (cons 1 "DADOS DA INSTALA��O"))
								)
				)

	(setq textoReferencia (vlax-ename->vla-object (ssname textoReferencia 0)))
	(vla-getBoundingBox textoReferencia 'pBox1 'pBox2)
	(setq pBox1 (vlax-safearray->list pBox1))
	(setq pBox2 (vlax-safearray->list pBox2))
	;Diferen�a entra as alturas
	(setq divisor (- (cadr pBox2) (cadr pBox1)))

	;Auxiliar para calcular a diferen�a de altura entre as linhas
	;da tabela de dados, para calcular a escala do bloco         
	(setq penultimaLH (cdr (assoc 10 (entget (nth (- (length lHor) 2) lHor)))))
	(setq dividendo (- (cadr penultimaLH) (cadr pt1_LHorLast)))
	
	;Escala do bloco "QUADRO DE CARGAS", que ser� reinserido
	(setq escalaBloco (* (/ dividendo divisor) 1.66667))

	;Deleta os objetos provenientes do bloco "QUADRO DE CARGAS" explodido
	(setq i 0)
	(setq desenhoTodo1 (LM:SS->LIST desenhoTodo1))
	(setq desenhoTodo2 (LM:SS->LIST desenhoTodo2))
	(while (and (< i (length desenhoTodo1)) (> (length desenhoTodo2) (length desenhoTodo1)))
		(setq obj (nth i desenhoTodo2))
		(if (not (member obj desenhoTodo1))
			(progn

				;Verificar se o objeto � a borda mais externa do bloco (parte cinza)
				;para pegar os dois pontos de limite do bloco.                      
				;Pega-se estes pontos nesta parte, pois quando pegava-se depois     
				;de escalar o bloco inteiro, as caixas de texto influenciavam num   
				;tamanho maior.                                                     
				(setq elObj (entget obj))
				(if (and (= (cdr (assoc 0 elObj)) "LWPOLYLINE") (= (cdr (assoc 8 elObj)) "LANG-GR-PRANCHA 1"))
					(progn
					 (vla-ScaleEntity (vlax-ename->vla-object obj) (vlax-3d-point pt2_LVer1) escalaBloco)
					 (vla-getBoundingBox (vlax-ename->vla-object obj) 'p1 'p2)
						(setq p1 (vlax-safearray->list p1)
									p2 (vlax-safearray->list p2)
									)
						(setq X1Cargas (car p1))
						(setq X2Cargas (car p2))
						(setq Y1Cargas (cadr p1))
						(setq Y2Cargas (cadr p2))
						)
					)


				
				 
				(entdel obj)
				(setq desenhoTodo2 (vl-remove obj desenhoTodo2))
				)
			(setq i (1+ i))
			)
		
		)

	

	;Reinser��o do bloco "QUADRO DE CARGAS"
	(command "INSERT" "BLOCO-QUADRO-DE-CARGAS" pt2_LVer1 "" "" "")
	(setq blk_QuadCargas (entlast))
	(setq ObjBlk_QuadCargas (vlax-ename->vla-object blk_QuadCargas))
	(ChangeLayObjs (list blk_QuadCargas) "0" 7)

	;Pegar o n�mero de fases
	(setq colFases (cond
									 ((vl-position "FASES" (car dadosTab)))
									 ((vl-position "Fases" (car dadosTab)))
									 )
				)
	(if (= (strlen (nth colFases (last dadosTab))) 3)
		(setq nFases "2F+N+Pe")
		(progn
			(if (= (strlen (nth colFases (last dadosTab))) 5)
				(setq nFases "3F+N+Pe")
				)
			)
		)

	;Calcular "Fator de pot�ncia aproximado"
	;e formatar o n�mero                    
	(setq potW (atof (nth (vl-position "POT. TOTAL. (W)" (car dadosTab)) (last dadosTab))))
	(setq potVA (atof (nth (vl-position "POT. TOTAL. (VA)" (car dadosTab)) (last dadosTab))))
	(setq fatorPA (vl-string->list (rtos (/ (float potW) (float potVA)) 2 2)))
	(setq fatorPA (subst 44 46 fatorPA))
	(setq fatorPA (append (list 48) fatorPA))
	(setq fatorPA (vl-list->string fatorPA))
	(if (= (strlen fatorPA) 5)
		(setq fatorPA (substr fatorPA 2))
		)

	
	
	;Pegar o valor "Queda de tens�o...",         
	;que � o maior valor da coluna "DV PARC (%)".
	(setq queda (append))
	(setq maiorQueda "-99999999")
	(setq col (cond
							((vl-position "DV PARC (%)" (car dadosTab)))
							((vl-position "DV TOTAL (%)" (car dadosTab)))
							)
				)

	;; Se n�o for encontrada a coluna "DV PARC (%)" ou TOTAL
	;; apenas pula esta etapa.
	(if (/= col nil)
		(foreach linha (cdr dadosTab)
			(if (and (/=  (nth col linha) "") (> (atof (nth col linha)) (atof maiorQueda)))
				(setq maiorQueda (nth col linha))
				)
			)
		)

	;; Arredondar o valor da queda    
	;; para deix�-lo no formato X,X0. 
	(setq maiorQueda (atof maiorQueda))
	(setq maiorQueda (/ (1+ (fix (* 10 maiorQueda))) 10.0))
	(setq maiorQueda (vl-string->list (rtos maiorQueda 2 2)))
	(setq maiorQueda (append (subst 44 46 maiorQueda) (list 48)))
	(setq maiorQueda (vl-list->string maiorQueda))
	(setq maiorQueda (substr maiorQueda 1 4))


	;Pegar os totais das cargas parciais de cada fase
	(setq cargaR (nth (vl-position "POT. - R (W)" (car dadosTab)) (last dadosTab)))
	(setq cargaS (nth (vl-position "POT. - S (W)" (car dadosTab)) (last dadosTab)))
	(setq cargaT (nth (vl-position "POT. - T (W)" (car dadosTab)) (last dadosTab)))

	;Pegar os valores das cargas totais W e VA
	(setq cargaVA (nth (vl-position "POT. TOTAL. (VA)" (car dadosTab)) (last dadosTab)))
	(setq cargaW (nth (vl-position "POT. TOTAL. (W)" (car dadosTab)) (last dadosTab)))

  ;Preencher atributos do bloco
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "QUADRO" tituloQuadro)
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "XF+N+Pe" nFases)
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "XXX/XXX" "127/220")
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "60" "60")
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "ICC" "10")
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "0,92" fatorPA)
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "%" "%")
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "4%" (strcat maiorQueda "%"))
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "RRRR" cargaR)
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "SSSS" cargaS)
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "TTTT" cargaT)
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "VAVA" cargaVA)
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "WWWW" cargaW)
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "XX-M�DULOS" "XXXXXXXXXX")
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "XXXX-SCHNEIDER" "XXXXXXXXXX")

	

	;***********************************************************************************************************************************************************************************************
	;Considerar colocar as notas gerais como parte do bloco,
	;para n�o precisara do programa de completar essa parte.
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "NOTA1" "1 - A INSTALA��O DESTE QUADRO SER� EMBUTIDA.")
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "NOTA2" "2 - DEVER� ATENDER A NBR 60439-1 E A NR-10.")
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "NOTA3" "3 - ESPECIFICA��ES COMPLEMENTARES EST�O NO MEMORIAL DESCRITIVO.")
	(LM:vl-SetAttributeValue ObjBlk_QuadCargas "NOTA4" "4 - Icc DOS DISJUNTORES GERAL E PARCIAL DEVER�O SER DE 3kA.")
	;***********************************************************************************************************************************************************************************************
	
	(vla-ScaleEntity ObjBlk_QuadCargas (vlax-3d-point pt2_LVer1) escalaBloco)
	
	(setq Dis1_DynBlock (LM:GetDynProps ObjBlk_QuadCargas))
	(setq Dis2_DynBlock (cdr (assoc "Distance2" Dis1_DynBlock)))
	(setq Dis1_DynBlock (cdr (assoc "Distance1" Dis1_DynBlock)))

	(setq distX_QuadCargas (- (car pt2_LHor1) (car pt2_LVer1)))

	(if (>= distX_QuadCargas Dis1_DynBlock)
		(LM:SetDynPropValue ObjBlk_QuadCargas "Distance1" distX_QuadCargas)
		(progn
			(vla-move ObjBlk_QuadCargas (vlax-3d-point pt2_LVer1) (vlax-3d-point (polar pt2_LVer1 pi (/ (- Dis1_DynBlock distX_QuadCargas) 2.0))))
			(setq p1 (polar p1 pi (/ (- Dis1_DynBlock distX_QuadCargas) 2.0)))
			)
		)
	(LM:SetDynPropValue ObjBlk_QuadCargas "Distance2" (- (cadr pt2_LHor1) (cadr pt2_LVer1)))

	(setq pt_Trifilar (polar p1 (* 3.0 (/ pi 2.0)) (* 0.5 dividendo)))
		
	;Insere o bloco "Diagrama Trifilar", escala-o
	(command "_.INSERT" (strcat caminho "\\BLOCOS\\BLOCO-DIAGRAMA-TRIFILAR.dwg") pt_Trifilar "" "" "")
	(setq blk_Trifilar (entlast))
	(setq obj_Trifilar (vlax-ename->vla-object blk_Trifilar))
	(vla-ScaleEntity obj_Trifilar (vlax-3d-point pt_Trifilar) escalaBloco)
	(ChangeLayObjs (list blk_Trifilar) "0" 7)

	;(vla-regen (vla-get-activedocument (vlax-get-acad-object)))

	(InsereCircuitos)

	;; Seta o valor da corrente do barramento 
	(if (>= correnteDG 100)
		(LM:vl-SetAttributeValue ObjBlk_QuadCargas "IXX" (itoa (FindNum > (1+ correnteDG) padroesDG)))
		(LM:vl-SetAttributeValue ObjBlk_QuadCargas "IXX" "100")
		)

	;; Highlights the attributes from circ block (Quadro de cargas) 
	;; e blk_Trifilar block (Trifilar)                              
	(HighlightAttributes blk_QuadCargas blk_Trifilar circ)
				

	

	
	
	
	(command "_.ZOOM" "O" blk_QuadCargas blk_Trifilar "")
	(vla-endUndoMark doc)

	
	(RestauraVariaveis)
	)


	