
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
    (send ?p get-user-name)
)

(deffunction set-user-name(?p ?x)
    (send ?p put-user-name ?x)
)

(deffunction kill(?p)
    (send ?p put-dead yes)
)

(deffunction is-dead(?p)
    (send ?p get-dead)
)

(deffunction get-rept(?p)
    (send ?p get-rept)
)

(deffunction inc-rept(?p)
    (bind ?rept (get-rept ?p))
    (bind ?rept (+ ?rept 1))
    (send ?p put-rept ?rept)
)

(deffunction get-madness(?p)
    (send ?p get-madness)
)

(deffunction inc-madness(?p)
    (bind ?madness (get-madness ?p))
    (bind ?madness (+ ?madness 1))
    (send ?p put-madness ?madness)
)

(deffunction get-karma(?p)
    (send ?p get-karma)
)

(deffunction inc-karma(?p)
    (bind ?karma (get-karma ?p))
    (bind ?karma (+ ?karma 1))
    (send ?p put-karma ?karma)
)

(deffunction is-alive(?p)
    (bind ?dead (send ?p get-dead))
    (bind ?return yes)
    (if (eq ?dead yes) then (bind ?return no))
    ?return
)

(definstances players
    (player of PLAYER (dead no) (rept 1) (madness 0) (karma 0))
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
    (printout t "
    _                                 _     _           __ _                                 _   _                _ _
   | |__   _____      __  _ __   ___ | |_  | |_ ___    / _(___  __  _   _  ___  _   _ _ __  | |_(_)_ __ ___   ___| (_)_ __   ___
   | '_ \\ / _ \\ \\ /\\ / / | '_ \\ / _ \\| __| | __/ _ \\  | |_| \\ \\/ / | | | |/ _ \\| | | | '__| | __| | '_ ` _ \\ / _ | | | '_ \\ / _ \\
   | | | | (_) \\ V  V /  | | | | (_) | |_  | || (_) | |  _| |>  <  | |_| | (_) | |_| | |    | |_| | | | | | |  __| | | | | |  __/
   |_| |_|\\___/ \\_/\\_/   |_| |_|\\___/ \\__|  \\__\\___/  |_| |_/_/\\_\\  \\__, |\\___/ \\__,_|_|     \\__|_|_| |_| |_|\\___|_|_|_| |_|\\___|
                                                                    |___/


    Presiona enter para comenzar ...
    " crlf)
    (readline)
)

;; Decidir preguntas

(defrule M4
    (where cave)
    (or (test (= (get-rept [player]) 0))
        (deja-vu))
    =>
    (assert (show M4))
    (assert (incoming-transmision))
)

(defrule M1
    (test (> (get-rept [player]) 0))
    =>
    (assert (show M1))
    (assert (already-happened))
)

(defrule M3
    ?already <- (already-happened)
    (test (> (get-rept [player]) 2))
    =>
    (assert (show M3))
    (retract ?already)
    (inc-madness [player])
    (assert (deja-vu))

)

(defrule M2
    ?already <- (already-happened)
    (test (> (get-rept [player]) 7))
    =>
    (assert (show M2))
    (kill [player])
    (retract ?already)
)

(defrule M5
    (threat-rip)
    =>
    (assert (show M5))
    (inc-madness [player])
    )

(defrule M6
    (threat-rip)
    =>
    (assert (show M6))
)

(defrule M7
    ?threat <- (threat-rip)
    =>
    (assert (show M7))
    (retract ?threat)
    (kill [player])

    )

(defrule M34
    (ask-rip)
    =>
    (assert (show M34))
    )

(defrule M8
    (show-rip)
    =>
    (assert (show M8))
    )

(defrule M9
    (show-rip)
    =>
    (assert (show M9))
    )

(defrule M10
    (declare (salience 5))
    (not-under-effect)
    ?under <- (not-under-effect)
    =>
    (assert (show M10))
    (retract ?under)
    )

(defrule M11

    ?race <- (its-x)
    =>
    (assert (show M11))
    (assert (go-to-planet))
    (retract ?race)
    )


(defrule Q1
    ?where <- (where cave)
    ?trans <- (incoming-transmision)
    (test (= (get-rept [player]) 0))
    =>
    (assert (show Q1))

    (retract ?trans)
    (retract ?where)
)

(defrule Q1-A1
    ?ans <- (answer-to Q1 1)
    =>
    (assert (where way-imp-ship))
    (retract ?ans)
)

(defrule Q1-A2
    ?ans <- (answer-to Q1 2)
    =>
    (assert (where way-city))
    (retract ?ans)
)

(defrule Q2
    (declare (salience 5))
    ?already <- (already-happened)
    =>
    (assert (show Q2))
    (retract ?already)
)

(defrule Q2-A1
    ?ans <- (answer-to Q2 1)
    =>
    (assert (threat-rip))
    (retract ?ans)
    )

(defrule Q2-A2
    ?ans <- (answer-to Q2 2)
    =>
    (assert (ask-rip))
    (retract ?ans)
    )

(defrule Q2-A3
    ?ans <- (answer-to Q2 3)
    =>
    (assert (show-rip))
    (retract ?ans)
    )

(defrule Q4
    (ask-rip)
    =>
    (assert (show Q4))
    )

(defrule Q4-A1
    ?ans <- (answer-to Q4 no)
    ?rip <- (ask-rip)
    =>
    (assert (threat-rip))
    (retract ?ans)
    (retract ?rip)
    )

(defrule Q4-A2
    ?ans <- (answer-to Q4 yes)
    ?rip <- (ask-rip)
    =>
    (assert (show-rip))
    (retract ?ans)
    (retract ?rip)
    )

(defrule Q5
    ?show <- (show-rip)
    =>
    (assert (show Q5))
    (retract ?show)
    )

(defrule Q5-A1
    ?ans <- (answer-to Q5 no)
    =>
    (assert (not-under-effect))
    (assert (know-race))
    (retract ?ans)
    )

(defrule Q5-A2
    ?ans <- (answer-to Q5 yes)
    =>
    (assert (know-race))
    (retract ?ans)
    )

(defrule Q6
    ?race <- (know-race)
    =>
    (assert (show Q6))

)

(defrule Q6-A1
    ?ans <- (answer-to Q6 1)
    ?know <- (know-race)
    =>
    (assert (its-x-or-y))
    (retract ?ans)
    (retract ?know)
    )

(defrule Q6-A2
    ?ans <- (answer-to Q6 2)
    ?know <- (know-race)
    =>
    (assert (its-x))
    (retract ?ans)
    (retract ?know)
    )

(defrule Q7
    (its-x-or-y)
    =>
    (assert (show Q7))
    )

(defrule Q7-A1
    ?doubt <- (its-x-or-y)
    ?ans <- (answer-to Q7 1)
    =>
    (assert (its-x))
    (retract ?doubt)
    (retract ?ans)
    )

(defrule Q7-A2
    ?doubt <- (its-x-or-y)
    ?ans <- (answer-to Q7 2)
    =>
    (assert (its-x))
    (retract ?doubt)
    (retract ?ans)
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

    (message (name M30)
        (text "Vuelves a las nave imperial"))

    (question-yes-no (name Q8)
        (text "Te das cuenta de que las estrllas están cambiadas, miras de nuevo ?"))

    (message (name M31)
        (text "Te olvidas del tema y sigues caminando"))

    (message (name M32)
        (text "Vuelves a mirar"))

    (message (name M33)
        (text "Tropiezas y te quedas inconsciente"))

    (message (name M35)
        (text "Has muerto del golpe. END"))

    (message (name M36)
        (text "El objeto ha sido activado mientras estabas inconsciente. RESET"))

    (message (name M37)
        (text "Te reunes con la patrulla"))

    (message (name M38)
        (text "Falta A37"))

    (message (name M39)
        (text "Vuelves a la nave"))

    (message (name M40)
        (text "Vuelves con la nave. END"))

    (message (name M41)
        (text "Suena la alarma, hay un problema de sobrecarga"))

    (question-multi (name Q13)
        (text "El problema puede estar en la sala de generadores o en la sala de escudos, a cual vas ?")
        (answers
            "Sala de escudos"
            "Sala de generadores"))

    (message (name M42)
        (text "Vas a comprobar los generadores"))

    (message (name M43)
        (text "Muere A37 por una explosión de camino a la sala"))

    (message (name M44)
        (text "El objeto es activado por la sobrecarga"))

    (message (name M45)
        (text "Mueres por una explosión de camino a la sala"))

    (message (name M46)
        (text "Vas a comprobar los escudos"))

    (question-multi (name Q9-A)
        (text "Escuchas lo que te dice mientras vais a la zona de la ciudad")
        (answers
            "Acompañarle a la ciudad"
            "(Answer blocked)"))

    (question-multi (name Q9-B)
        (text "Escuchas lo que te dice mientras vais a la zona de la ciudad, sin embargo, después de lo que ha pasado no sabes qué pensar")
        (answers
            "Acompañarle a la ciudad"
            "Dispararle y volver sólo con el objeto"))

    (message (name M51)
        (text "Llegais a la zona de la ciudad"))

    (message (name M48)
        (text "Le disparas"))

    (message (name M49)
        (text "El ha sido mas rapido y ha dispardo antes"))

    (message (name M50)
        (text "El ha sido mas rapido y consigue activar el objeto"))

    (question-yes-no (name Q12)
        (text "Llegas a la ciudad y entrais a un sotano propiedad de Rip, recojes un objeto ?"))

    (message (name M58)
        (text "Recoges el objeto"))

    (message (name M59)
        (text "La nave a la que perteneces realiza un bombardeo orbital a tu posición"))

    (message (name M60)
        (text "Rip consigue activar el dispositivo a tiempo"))

    (question-yes-no (name Q10)
        (text "La zona de la ciudad esta destruida, pero parece que Rip tiene alli su nave, recojes un objeto de su interior?"))

    (message (name M52)
        (text "Recoges el objeto"))

    (message (name M53)
        (text "Despegas en la nave y recives una transmision, sino aterrizais os vuelan"))

    (message (name M54)
        (text "Te das cuenta de que la transmision es enviada por A37"))

    (message (name M55)
        (text "Te das cuenta de que la transmision no es enviada por A37, sin embargo debería ser suya"))

    (question-yes-no (name Q11)
        (text "Podria ser conveniente hacer caso a la transmision. Rendirse ?"))

    (message (name M56)
        (text "No te rindes y el caza dispara, Rip consigue activar el dispositivo a tiempo"))

    (message (name M57)
        (text "Te rindes pero el caza hace caso omiso y dispara, ambos moris"))

    (message (name M1)
        (text "Ya ha sucedido antes"))

    (message (name M2)
        (text "Te has disparado debido a la demencia"))

    (message (name M3)
        (text "Sufres un Deja vu"))

    (message (name M4)
        (text "Escuchas a Rip"))

    (question-multi (name Q1)
        (text "Escuchas una transmision, ¿Que haces?")
        (answers
            "Hacer caso y volver"
            "Ignorar y seguir a Rip"))

    (question-multi (name Q2)
        (text "Te cuestionas sobre lo que ocurre, ¿Que haces?")
        (answers
            "Amenazar"
            "Preguntar"
            "Enseñar objeto"))

    (message (name M34)
        (text "Le preguntas a Rip sobre lo que esta sucediendo"))

    (message (name M5)
        (text "Amenazas a Rip, debido a que desconfias"))

    (message (name M6)
        (text "Se palpa la tension del ambiente"))

    (message (name M7)
        (text "Intentas dispararle, pero fallas, resultando en que el te dispara y mueres"))

    (question-yes-no (name Q4)
        (text "¿Tienes alguna prueba?"))

    (message (name M8)
        (text "Le ensenas el objeto a Rip"))

    (message (name M9)
        (text "Rip piensa que sabes la verdad"))

    (question-yes-no (name Q5)
        (text "¿Te esta afectando el dispositivo?"))

    (message (name M10)
        (text "Explicacion del objeto"))

    (question-multi (name Q6)
        (text "¿Sabes que raza ha creado semejante dispositivo?")
        (answers
            "Puede ser la X o la Y"
            "Creo que es la X"))

    (message (name M11)
            (text "En realidad a sido la X"))

    (question-multi (name Q7)
        (text "Crees que la X tiene posiblidadeds, pero la Y también ha hecho cosas raras")
        (answers
            "Confias que e sla X"
            "Te decantas por la Y"))

    (message (name M12)
        (text "Llegas al planeta"))

    (message (name M13)
        (text "El planeta esta desierto"))

    (question-multi (name Q14)
        (text "¿Quien baja de la nave?")
        (answers
            "Deberias enviar a un tripulante"
            "Deberias bajar a mirar"
            "Deberia ir a mirar"))

    (message (name M14)
        (text "Rip ha bajado a mirar"))

    (message (name M15)
        (text "Rip se ve forzado a activar el objeto"))

    (message (name M16)
        (text "Bajas tu a mirar"))

    (message (name M17)
        (text "Encuentras una trampa"))

    (message (name M18)
        (text "Se activa una trampa y mueres"))

    (message (name M19)
        (text "Desactivas la trampa sin problemas"))

    (question-multi (name Q15)
        (text "Has avistado una patrulla, ¿Que haces?")
        (answers
            "Haces señales"
            "La eliminas y te infiltras"))

    (message (name M20)
        (text "La eliminas y logras infiltrarte"))

    (message (name M21)
        (text "Le haces señales a la patrulla"))

    (message (name M22)
        (text "Mueres acribillado, pensaron que eras un enemigo"))

    (question-yes-no (name Q16)
        (text "Encuentras un Xniano en una celda, el cual te pide educadamente que lo liberes. ¿Lo liberas?"))

    (question-yes-no (name Q17)
        (text "El prisionero llama a los guardias, y te ves acorralado. ¿Activas el objeto?"))

    (message (name M23)
        (text "Activas el objeto"))

    (message (name M24)
        (text "Te rindes ante los guardias"))

    (message (name M25)
        (text "Mueres a manos de los guardias"))

    (question-yes-no (name Q18)
        (text "El prisionero te cuenta su historia de cientifico y como perdio el objeto ¿Se lo entregas?"))

    (message (name M26)
        (text "Al no entregarselo, saca un cuchillo para intentar que le devuelvas el objeto por la fuerza"))

    (message (name M27)
        (text "Forcejeas y te roba el objeto, ademas te hiere de gravedad y mueres terriblemente"))

    (message (name M28)
        (text "Te lleva ante el lider de los Xnianos"))

    (message (name M29)
        (text "Su lider te agradece tu esfuerzo. ¡Has salvado la linea temporal!"))
)
