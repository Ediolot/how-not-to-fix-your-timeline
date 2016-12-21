
;; Añadir los type, default si fuera necesario y allowed

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
    (declare (salience 1000))
    =>
    (printout t "Bienvenido a la historia inter..." crlf crlf)
    (readline)
)

;; Decidir preguntas

(defrule Q1
    (where cave)
    (jugador (rept 0))
    =>
    (assert (show Q1))
    (assert (incoming-transmision))
)

(defrule Q2
    ?trans <- (incoming-transmision)
    ?where <- (where cave)
    =>
    (assert (show Q2))
    (retract ?where)
    (retract ?trans)
)

(defrule Q2-A1
    ?res <- (answer-to Q2 1)
    =>
    (printout t "Asserting answer 1")
    (retract ?res)
)

(defrule Q2-A2
    ?res <- (answer-to Q2 1)
    =>
    (printout t "Asserting answer 2")
    (retract ?res)
)

;; Mostrar preguntas

(defrule ask-question-yes-no
    (declare (salience 10))
    ?show <- (show ?q)
    (question-yes-no (name ?q) (text ?text))
    =>
    (printout t ?text crlf)
    (print-answers-yes-no)

    (assert (answer-to ?q (read)))
    (retract ?show)
)

(defrule ask-question-multi
    (declare (salience 10))
    ?show <- (show ?q)
    (question-multi (name ?q) (text ?text) (answers $?ans))
    =>
    (printout t ?text crlf)
    (print-answers-multi $?ans)

    (assert (answer-to ?q (read)))
    (retract ?show)
)

(defrule display-message
    (declare (salience 10))
    ?show <- (show ?q)
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
