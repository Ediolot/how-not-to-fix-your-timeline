
;; Añadir los type, default si fuera necesario y allowed

;;==============================================================================

(defclass PLAYER (is-a USER)
    (multislot user-name)
    (slot dead)
    (slot rept)
    (slot madness)
    (slot karma)
)

(deffunction get-user-name(?p)
    (send [?p] get-user-name)
)

(deffunction set-user-name(?p ?x)
    (send [?p] put-user-name ?x)
)

(deffunction kill(?p)
    (send [?p] put-dead yes)
)

(deffunction is-dead(?p)
    (send [?p] get-dead)
)

(deffunction is-alive(?p)
    (bind ?dead (send [?p] get-dead))
    (bind ?return yes)
    (if (eq ?dead yes) then (bind ?return no))
    ?return
)

(definstances players
    (player of PLAYER (dead no) (rept 0) (madness 0) (karma 0))
)

;;==============================================================================

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
    (incoming-transmision)
    ?where <- (where cave)
    =>
    (assert (show Q2))
    (retract ?where)
    (retract ?trans)
)

(defrule Q2-A1
    ?res <- (answer-to Q2 1)
    =>
    (assert (incoming-transmision ignored))
    (retract ?res)
)

(defrule Q2-A2
    ?res <- (answer-to Q2 2)
    =>
    (assert (incoming-transmision listened))
    (retract ?res)
)

(defrule Q3
    (assert )
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
            "Ignorar la transmision"
            "Escuchar la transmision"
        )
    )

    (message (name Q3)
        (text "Ecuchas un poco más al tio y empiezas a caminar (seguirle)"))

    (message (name Q4)
        (text "Escuchas la transmision de radio que te pide que vuelvas al punto de encuentro"))
)
