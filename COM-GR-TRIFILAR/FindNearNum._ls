;; Weiglas Ribeiro                         
;; FindNum                                 
;; Encontra um número numa lista de acordo 
;; com a restrição definida:               
;; "menor-igual/maior-igual/igual".        
(defun FindNum (func num numList / meio len result i j)

	(setq result nil)
	(setq len (1- (length numList)))
	(setq bool T)
	
	(setq i 0
				j len)
	(while bool
		(setq meio (/ (+ i j) 2))

		(if (>= i j)
			(progn
				(setq i (1+ j))
				(setq bool nil)
				
				(cond
					((equal func <=)
						(if (<= (nth meio numList) num)
							(setq result (nth meio numList))
							(setq result (if (> meio 0) (nth (1- meio) numList) nil))
							)
						)

					((equal func >=)
						(if (>= (nth meio numList) num)
							(setq result (nth meio numList))
							(setq result (if (< meio len) (nth (1+ meio) numList) nil))
							)
						)

					((equal func =) 
						(if (= (nth meio numList) num)
							(setq result (nth meio numList))
							nil
							)
						)
					)
				)

			(if (= (nth meio numList) num)
				(setq result (nth meio numList)
							i (1+ j))
				
				(if (< (nth meio numList) num)
					(setq i (1+ meio))
					(setq j (1- meio))
					)
				)
			)
		)

	
	result
	)