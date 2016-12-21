(deftemplate jugador
    (slot name)
    (slot dead)
    (slot rept)
    (slot madness)
    (slot karma)
)

(deftemplate question-yes-no
    (slot type (default yes-no))
    (slot name)
    (slot text)
)

(deftemplate question-multi
    (slot type (default multi))
    (slot name)
    (slot text)
    (multislot answers)
)

(deftemplate message
    (slot type (default message))
    (slot name)
    (slot text)
)

(deffunction print-answers-multi($?ans)
    (bind ?len (length $?ans))
    (loop-for-count (?i 1 ?len)
	   (printout t ?i ". " (nth$ ?i $?ans) crlf))
)

(deffunction print-answers-yes-no()
    (printout t "(yes/no)" crlf)
)

(deffacts initial-facts
    (where cave)
    (jugador (dead no) (rept 0) (madness 0) (karma 0))
)

(defrule start
    (not (initiated)) ;; Intentar con salience
    =>
    (printout t "Bienvenido a la historia inter..." crlf crlf)
    (assert (initiated))
)

;; Decidir preguntas

(defrule Q1
    (initiated)
    (where cave)
    (jugador (rept 0))
    =>
    (assert (show Q1))
    (assert (incoming-transmision))
)

(defrule Q2
    ?trans <- (incoming-transmision)
    ?where <- (where cave)
    (initiated)
    =>
    (assert (show Q2))
    (retract ?where)
    (retract ?trans)
)

;; Mostrar preguntas

(defrule ask-question-yes-no
    (declare (salience 10))
    ?show <- (show ?q)
    (initiated)
    (question-yes-no (name ?q) (text ?text))
    =>
    (printout t ?text crlf)
    (print-answers-yes-no)

    (assert (response-to ?q (readline)))
    (retract ?show)
)

(defrule ask-question-multi
    (declare (salience 10))
    ?show <- (show ?q)
    (initiated)
    (question-multi (name ?q) (text ?text) (answers $?ans))
    =>
    (printout t ?text crlf)
    (print-answers-multi $?ans)

    (assert (response-to ?q (readline)))
    (retract ?show)
)

(defrule display-message
    (declare (salience 10))
    ?show <- (show ?q)
    (initiated)
    (message (name ?q) (text ?text))
    =>
    (printout t ?text crlf)
    (readline)

    (retract ?show)
)

;; Base de datos de preguntas

(deffacts QA

    (message (name Q1)
        (text "Escuchar el tio"))

    (question-multi (name Q2)
        (text "Hay una transmision entrante, que haces?")
        (answers
            "Ir a la nave"
            "Ir a la ciudad"
        )
    )
)
