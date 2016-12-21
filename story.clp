(deftemplate jugador
    (slot name)
    (slot dead)
    (slot rept)
    (slot madness)
    (slot karma)
)

(deftemplate question
    (slot name)
    (slot text)
    (slot type) ;; Multi / yes-no / none
    (multislot answers)
)

(deffunction print-answers(?type $?ans)
    (bind ?len (length $?ans))

    (if (eq ?type multi)
        then
            (loop-for-count (?i 1 ?len)
        	   (printout t ?i ". " (nth$ ?i $?ans) crlf))
        else
            (printout t "(yes/no)" crlf)
    )
)

(deffacts initial-facts
    (where cave)
    (jugador (dead no) (rept 0) (madness 0) (karma 0))
)

(defrule start
    (not (initiated))
    =>
    (printout t "Bienvenido a la historia inter..." crlf crlf)
    (assert (initiated))
)

(defrule wait-user-enter
    (declare (salience 10))
    (initiated)
    ?enter <- (wait-user enter)
    =>
    (readline)
    (retract ?enter)
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

(defrule ask-question
    (declare (salience 100))
    ?show <- (show ?q)
    (initiated)
    (question (name ?q) (text ?text) (answers $?ans) (type ?type))
    =>
    (printout t ?text crlf)
    (print-answers ?type $?ans)

    (switch ?type
        (case multi then
            (assert (response-to ?q (readline)))
        )
        (case yes-no then
            (assert (response-to ?q (readline)))
        )
        (case none then
            (readline)
        )
    )

    (retract ?show)
)

;; Base de datos de preguntas

(deffacts Qs

    (question (name Q1)
        (type yes-no)
        (text "Escuchar el tio"))

    (question (name Q2)
        (type multi)
        (text "Hay una transmision entrante, que haces?")
        (answers
            "Ir a la nave"
            "Ir a la ciudad"
        )
    )
)
