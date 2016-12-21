(deftemplate jugador
    (slot nombre)
    (slot vivo)
    (slot rept)
    (slot locura)
    (slot karma)
)

(deffacts initial-facts
    (where cave)
    (jugador (vivo yes) (rept 0) (locura 0) (karma 0))
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
    (assert (wait-user enter))
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

(defrule ask-Q1
    (declare (salience 100))
    ?show <- (show ?q)
    (initiated)
    (question ?q ?text)
    =>
    (printout t ?text crlf)
    (retract ?show)
)

;; Base de datos de preguntas

(deffacts Qs

    (question Q1 "Escuchar el tio")

    (question Q2 "Hay una transmision entrante, que haces?
        A. algo
        B. perez")
)
