; GET_AT_LIST(BLNAME) returns a list of the attributes and their entity names
; and entity data for a block whose entity name is BLNAME.  Returns nil if
; there are no attributes.  Attribute names are raised to upper case.
;; https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/how-to-get-attribute-location/td-p/6801483

(defun-q get_at_list(blname / atlist bldata blflag )
 (setq atlist nil)
 (if blname ; if you sent parameter
  (if (= (type blname)(quote ename)) ; if it is an entity name
   (if (= "INSERT" (cdr (assoc 0 (setq bldata (entget blname))))) ; if insert
    (if (setq blflag (cdr (assoc 66 bldata))) ; if it has atts follow flag
     (if (= 1 blflag) ; if atts follow flag indicates atts
      (while
       (=
        "ATTRIB"
        (cdr (assoc 0 (setq bldata (entget (setq blname (entnext blname))))))
       )
       (setq atlist (cons (list (strcase (cdr (assoc 2 bldata))) bldata) atlist))
      )
     )
    )
   )
  )
 )
 atlist
)
