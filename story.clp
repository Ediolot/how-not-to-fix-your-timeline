
;;==============================================================================

(defclass PLAYER (is-a USER)
    (multislot user-name)
    (slot dead    (type SYMBOL) (default no) (allowed-symbols yes no))
    (slot rept    (type NUMBER) (default 0))
    (slot madness (type NUMBER) (default 0))
    (slot karma   (type NUMBER) (default 0))
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
    (player of PLAYER)
)

;;==============================================================================

(deftemplate question-yes-no
    (slot name (type SYMBOL))
    (slot text (type STRING))
)

(deftemplate question-multi
    (slot name (type SYMBOL))
    (slot text (type STRING))
    (multislot answers (type STRING))
)

(deftemplate message
    (slot name (type SYMBOL))
    (slot text (type STRING))
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
    (city not-destroyed)
)

(defrule start
    (declare (salience 1000))
    =>
    (seed (round (time)))
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

;; CAVE

(defrule M1
    (declare (salience 8))
    (where cave)
    (object (is-a PLAYER) (rept ?value))
    (test (> ?value 0))
    =>
    (assert (show M1))
    (assert (already-happened))
)

(defrule M4
    (where cave)
    (object (is-a PLAYER) (rept ?value))
    (or (test (= ?value 0))
        (deja-vu))
    =>
    (assert (show M4))
    (assert (incoming-transmision))
)

(defrule Q1
    ?where <- (where cave)
    ?trans <- (incoming-transmision)
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

(defrule M2
    ?already <- (already-happened)
    (object (is-a PLAYER) (madness ?value))
    (test (> ?value 10))
    =>
    (assert (show M2))
    (kill [player])
    (assert (end))
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
    (assert (end))
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
    (assert (on-ship))
    (retract ?race)
    )

(defrule Q2-A
    ?already <- (already-happened)
    (not (have-object))
    (object (is-a PLAYER) (rept ?value))
    (test (>= ?value 3))
    =>
    (assert (show Q2-A))
    (retract ?already)
)

(defrule Q2-B
    ?already <- (already-happened)
    (have-object)
    (object (is-a PLAYER) (rept ?value))
    (test (>= ?value 3))
    =>
    (assert (show Q2-B))
    (retract ?already)
)

(defrule Q2-A-A1
    ?ans <- (answer-to Q2-A 1)
    =>
    (assert (threat-rip))
    (retract ?ans)
    )

(defrule Q2-A-A2
    ?ans <- (answer-to Q2-A 2)
    =>
    (assert (ask-rip))
    (retract ?ans)
    )

(defrule Q2-B-A1
    ?ans <- (answer-to Q2-B 1)
    =>
    (assert (threat-rip))
    (retract ?ans)
    )

(defrule Q2-B-A2
    ?ans <- (answer-to Q2-B 2)
    =>
    (assert (ask-rip))
    (retract ?ans)
    )

(defrule Q2-B-A3
    ?ans <- (answer-to Q2-B 3)
    =>
    (assert (show-rip))
    (retract ?ans)
    )

(defrule Q4-A
    (have-object)
    (ask-rip)
    =>
    (assert (show Q4-A))
    )

(defrule Q4-B
    (not (have-object))
    (ask-rip)
    =>
    (assert (show Q4-B))
    )

(defrule Q4-A-A1
    ?ans <- (answer-to Q4-A 1)
    ?rip <- (ask-rip)
    =>
    (assert (threat-rip))
    (retract ?ans)
    (retract ?rip)
    )

(defrule Q4-B-A1
    ?ans <- (answer-to Q4-B 1)
    ?rip <- (ask-rip)
    =>
    (assert (threat-rip))
    (retract ?ans)
    (retract ?rip)
    )

(defrule Q4-B-A2
    ?ans <- (answer-to Q4-B 2)
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

;; NAVE

(defrule M12
    ?where <- (where cave)
    (on-ship)
    =>
    (assert (show M12))
    (retract ?where)
    (assert (where planet))
)

(defrule M13
    (where planet)
    (on-ship)
    =>
    (assert (show M13))
    )

    (defrule M3
        ?already <- (already-happened)
        (or (test (= (get-rept [player]) 1))
            (test (= (get-rept [player]) 2)))
        =>
        (assert (show M3))
        (retract ?already)
        (inc-madness [player])
        (assert (deja-vu))
    )

(defrule Q14
    (on-ship)
    (where planet)
    =>
    (assert (show Q14))
    )

(defrule Q14-A1
    (where planet)
    ?ship <- (on-ship)
    ?ans <- (answer-to Q14 1)
    =>
    (assert (crew-member))
    (assert (outside))
    (retract ?ans)
    (retract ?ship)
    )

(defrule Q14-A2
    (where planet)
    ?ship <- (on-ship)
    ?ans <- (answer-to Q14 2)
    =>
    (assert (you-member))
    (assert (outside))
    (retract ?ans)
    (retract ?ship)
    )

(defrule Q14-A3
    (where planet)
    ?ship <- (on-ship)
    ?ans <- (answer-to Q14 3)
    =>
    (assert (rip-member))
    (assert (outside))
    (retract ?ans)
    (retract ?ship)
    )

;; EXTERIOR

(defrule M61
    (where planet)
    (crew-member)
    (outside)
    =>
    (assert (show M61))
    (assert (explore))
    )

(defrule M62
    (where planet)
    (crew-member)
    (outside)
    ?exp <- (explore)
    =>
    (assert (show M62))
    (retract ?exp)
    (assert (patrol-near))
    )

(defrule M63
    (outside)
    (where planet)
    ?lost <- (crew-member)
    ?patr <- (patrol-near)
    =>
    (assert (show M63))
    (kill [player])
    )

(defrule M14
    (where planet)
    (outside)
    (rip-member)
    =>
    (assert (show M14))
    (assert (rip-goes))
    )

(defrule M15
    ?member <- (rip-member)
    ?goes <- (rip-goes)
    ?where <- (where planet)
    ?outside <- (outside)
    =>
    (assert (show M15))
    (retract ?member)
    (retract ?goes)
    (retract ?where)
    (retract ?outside)
    (assert (where cave))
    (inc-rept [player])
    )

(defrule M16
    (outside)
    (where planet)
    ?member <- (you-member)
    =>
    (assert (show M16))
    (assert (you-go))
    (retract ?member)
    )

(defrule M17
    (outside)
    (where planet)
    ?you <- (you-go)
    =>
    (assert (show M17))
    (retract ?you)
    (if (= (random 0 2) 0)
        then
            (assert (explodes))
        else
            (assert (disabled))
    )
)

(defrule M18
    (outside)
    (where planet)
    (explodes)
    =>
    (assert (show M18))
    (kill [player])
)

(defrule M19
    (outside)
    (where planet)
    ?dis <- (disabled)
    =>
    (assert (show M19))
    (retract ?dis)
    (assert (safe-for-now))
    )

(defrule M21
    (outside)
    (where planet)
    ?signals <- (signals)
    =>
    (assert (show M21))
    (retract ?signals)
    (assert (seen-by-patrol))
    )

(defrule M22
    (outside)
    (where planet)
    (seen-by-patrol)
    =>
    (assert (show M22))
    (kill [player])
    (assert (end))
    )

(defrule M20
    ?out <- (outside)
    (where planet)
    ?kill <- (kill-and-sneak)
    =>
    (assert (show M20))
    (retract ?out)
    (retract ?kill)
    (assert (cells))
    )

(defrule Q15
    (outside)
    (where planet)
    ?safe <- (safe-for-now)
    =>
    (assert (show Q15))
    (retract ?safe)
    )

(defrule Q15-A1
    ?ans <- (answer-to Q15 1)
    =>
    (retract ?ans)
    (assert (signals))
    )

(defrule Q15-A2
    ?ans <- (answer-to Q15 2)
    =>
    (retract ?ans)
    (assert (kill-and-sneak))
    )

;; CAMINO A LA CIUDAD

(defrule Q9-A
    (where way-city)
    (object (is-a PLAYER) (madness ?value))
    (test (< ?value 3))
    =>
    (assert (show Q9-A))
)

(defrule Q9-B
    (where way-city)
    (object (is-a PLAYER) (madness ?value))
    (test (>= ?value 3))
    =>
    (assert (show Q9-B))
)

(defrule Q9-A-A1
    ?ans <- (answer-to Q9-A 1)
    =>
    (assert (go city-zone))
    (retract ?ans)
)

(defrule Q9-B-A1
    ?ans <- (answer-to Q9-B 1)
    =>
    (assert (go city-zone))
    (retract ?ans)
)

(defrule Q9-B-A2
    ?ans <- (answer-to Q9-B 2)
    =>
    (assert (shoot rip))
    (retract ?ans)
)

(defrule M51
    ?where <- (where way-city)
    ?go <- (go city-zone)
    =>
    (retract ?where)
    (retract ?go)
    (assert (show M51))
    (assert (where city-zone))
)

(defrule M48
    (where way-city)
    (shoot rip)
    =>
    (assert (show M48))
    (inc-madness [player])

    (if (= (random 0 1) 0)
        then
            (assert (rip faster)) ; 33%
        else
            (assert (rip reset)) ; 66%
    )
)

(defrule M49
    (where way-city)
    (rip faster)
    (shoot rip)
    =>
    (assert (show M49))
    (assert (end))
)

(defrule M50
    ?where <- (where way-city)
    ?rip   <- (rip reset)
    ?shoot <- (shoot rip)
    =>
    (assert (show M50))
    (assert (where cave))
    (inc-rept [player])
    (retract ?shoot)
    (retract ?rip)
    (retract ?where)
)

;; NAVE

(defrule Q10
    (where city-zone)
    (city destroyed)
    =>
    (assert (show Q10))
    (assert (where rip-ship))
)

(defrule Q10-A1
    ?ans <- (answer-to Q10 yes)
    =>
    (retract ?ans)
    (assert (pick-object))
    (assert (incoming-transmision))
)

(defrule Q10-A2
    ?ans <- (answer-to Q10 no)
    =>
    (retract ?ans)
    (assert (incoming-transmision))
)

(defrule M52
    ?pick <- (pick-object)
    (where city-zone)
    (where rip-ship)
    (city destroyed)
    =>
    (assert (show M52))
    (assert (have-object))
    (retract ?pick)
)

(defrule M53
    (not (pick-object))
    ?trans <- (incoming-transmision)
    (where city-zone)
    (where rip-ship)
    (city destroyed)
    =>
    (assert (show M53))
    (assert (incoming-fighter))
    (retract ?trans)
)

(defrule M54
    (not (A37 missing))
    ?fighter <- (incoming-fighter)
    (where city-zone)
    (where rip-ship)
    (city destroyed)
    =>
    (assert (show M54))
    (assert (try surrender))
    (retract ?fighter)
)

(defrule M55
    (A37 missing)
    ?fighter <- (incoming-fighter)
    (where city-zone)
    (where rip-ship)
    (city destroyed)
    =>
    (assert (show M55))
    (assert (try surrender))
    (retract ?fighter)
)

(defrule Q11
    ?try <- (try surrender)
    (where city-zone)
    (where rip-ship)
    (city destroyed)
    =>
    (assert (show Q11))
    (retract ?try)
)

(defrule Q11-A1
    ?ans <- (answer-to Q11 yes)
    =>
    (assert (fighter-shoots))
    (retract ?ans)
)

(defrule Q11-A2
    ?ans <- (answer-to Q11 no)
    =>
    (assert (fighter-shoots reset))
    (retract ?ans)
)

(defrule M56
    ?shoot  <- (fighter-shoots reset)
    ?where1 <- (where city-zone)
    ?where2 <- (where rip-ship)
    (city destroyed)
    =>
    (assert (show M56))
    (assert (where cave))
    (inc-rept [player])
    (retract ?shoot)
    (retract ?where1)
    (retract ?where2)
)

(defrule M57
    (fighter-shoots)
    (where city-zone)
    (where rip-ship)
    (city destroyed)
    =>
    (assert (show M57))
    (assert (end))
)

;; SOTANO

(defrule Q12
    (where city-zone)
    (city not-destroyed)
    =>
    (assert (show Q12))
    (assert (where basement))
)

(defrule Q12-A1
    ?ans <- (answer-to Q12 yes)
    =>
    (retract ?ans)
    (assert (pick-object))
    (assert (orbital-shoot))
)

(defrule Q12-A2
    ?ans <- (answer-to Q12 no)
    =>
    (retract ?ans)
    (assert (orbital-shoot))
)

(defrule M58
    ?pick <- (pick-object)
    (where city-zone)
    (where basement)
    (city not-destroyed)
    =>
    (assert (show M58))
    (assert (have-object))
    (retract ?pick)
)

(defrule M59
    (not (pick-object))
    (orbital-shoot)
    (where city-zone)
    (where basement)
    (city not-destroyed)
    =>
    (assert (show M59))
    (assert (obj activated))
)

(defrule M60
    (not (pick-object))
    ?obj    <- (obj activated)
    ?shoot  <- (orbital-shoot)
    ?where1 <- (where city-zone)
    ?where2 <- (where basement)
    ?city   <- (city not-destroyed)
    =>
    (assert (show M60))
    (assert (city destroyed))
    (assert (where cave))
    (inc-rept [player])
    (retract ?obj)
    (retract ?shoot)
    (retract ?where1)
    (retract ?where2)
    (retract ?city)
)

;; CAMINO NAVE IMP

(defrule M30
    (where way-imp-ship)
    =>
    (assert (show M30))
    (assert (go check-point))
)

(defrule Q8
    (declare (salience 5))
    (go check-point)
    (where way-imp-ship)
    (object (is-a PLAYER) (rept ?value))
    (test (>= ?value 1))
    =>
    (assert (show Q8))
    (inc-madness [player])
)

(defrule Q8-A1
    ?ans <- (answer-to Q8 yes)
    ?go  <- (go check-point)
    =>
    (assert (look sky again))
    (inc-madness [player])
    (retract ?go)
    (retract ?ans)
)

(defrule Q8-A2
    ?ans <- (answer-to Q8 no)
    =>
    (assert (ignore sky))
    (retract ?ans)
)

(defrule M32
    ?look <- (look sky again)
    (where way-imp-ship)
    =>
    (inc-madness [player])
    (assert (show M32))
    (assert (stun-hit))
    (retract ?look)
)

(defrule M33
    ?stun <- (stun-hit)
    (where way-imp-ship)
    =>
    (assert (show M33))
    (retract ?stun)

    (if (= (random 0 2) 0)
        then
            (assert (critical-hit)) ; 33%
        else
            (assert (reset-hit)) ; 66%
    )
)

(defrule M35
    (where way-imp-ship)
    (critical-hit)
    =>
    (assert (show M35))
    (kill [player])
    (assert (end))
)

(defrule M36
    ?where <- (where way-imp-ship)
    ?reset <- (reset-hit)
    =>
    (assert (show M36))
    (assert (where cave))
    (inc-rept [player])
    (retract ?where)
    (retract ?reset)
)

(defrule M31
    ?sky <- (ignore sky)
    (where way-imp-ship)
    =>
    (assert (show M31))
    (assert (go check-point))
    (retract ?sky)
)

;; PUNTO DE ENCUENTRO

(defrule M37
    ?where <- (where way-imp-ship)
    ?go    <- (go check-point)
    =>
    (retract ?go)
    (retract ?where)

    (assert (show M37))
    (assert (where check-point))
    (assert (go ship))
)

(defrule M38
    (declare (salience 5))
    (where check-point)
    (A37 missing)
    =>
    (assert (show M38))
)

;; SHIP

(defrule M39
    ?where <- (where check-point)
    ?go    <- (go ship)
    =>
    (assert (show M39))
    (assert (where imp-ship))
    (retract ?go)
    (retract ?where)

    (if (= (random 0 2) 0)
        then
            (assert (all-safe))
        else
            (assert (alarm-start))
    )
)

(defrule M40
    (where imp-ship)
    (all-safe)
    =>
    (assert (show M40))
    (assert (end))
)

(defrule M41
    (where imp-ship)
    (alarm-start)
    =>
    (assert (show M41))
    (assert (check-problem))
)

(defrule Q13
    ?check <- (check-problem)
    (where imp-ship)
    (alarm-start)
    =>
    (assert (show Q13))
    (retract ?check)
)

(defrule Q13-A1
    ?ans <- (answer-to Q13 1) ; Escudos
    =>
    (assert (go shields-room))
    (retract ?ans)
)

(defrule Q13-A2
    ?ans <- (answer-to Q13 2) ; Generadores
    =>
    (assert (go generators-room))
    (retract ?ans)
)

(defrule Q16
    (where planet)
    (cells)
    =>
    (assert (show Q16))
    )

(defrule Q16-A1
    ?cells <- (cells)
    ?ans <- (answer-to Q16 yes)
    =>
    (retract ?ans)
    (assert (free-him))
    (assert (inside))
    (retract ?cells)
    )

(defrule Q16-A2
    ?ans <- (answer-to Q16 no)
    =>
    (retract ?ans)
    (assert (ignore-him))
    )

(defrule Q17
    (ignore-him)
    (where planet)
    (cells)
    =>
    (assert (show Q17))
    )

(defrule Q17-A1
    ?ans <- (answer-to Q17 yes)
    =>
    (assert (escape))
    (retract ?ans)
    )

(defrule Q17-A2
    ?ans <- (answer-to Q17 no)
    =>
    (assert (surrender))
    (retract ?ans)
    )


(defrule M46
    (go shields-room)
    (where imp-ship)
    (alarm-start)
    =>
    (assert (show M46))
    (assert (explosion))
)

(defrule M42
    (go generators-room)
    (where imp-ship)
    (alarm-start)
    =>
    (assert (show M42))
    (assert (explosion))
)

(defrule M45
    (where imp-ship)
    (alarm-start)
    (explosion)
    (or (go shields-room)
        (and (go generators-room)
             (A37 missing))
    )
    =>
    (assert (show M45))
    (assert (end))
    (kill [player])
)

(defrule M43
    ?go    <- (go generators-room)
    (where imp-ship)
    (alarm-start)
    (explosion)
    =>
    (assert (show M43))
    (assert (A37 die))
    (inc-madness [player])
    (retract ?go)
)

(defrule M44
    ?die   <- (A37 die)
    ?where <- (where imp-ship)
    ?alarm <- (alarm-start)
    ?explo <- (explosion)
    =>
    (assert (show M44))
    (assert (where cave))
    (assert (A37 missing))
    (inc-rept [player])
    (retract ?die)
    (retract ?where)
    (retract ?alarm)
    (retract ?explo)
)

(defrule M23
    ?escape <- (escape)
    ?where <- (where planet)
    ?cells <- (cells)
    =>
    (assert (show M23))
    (assert (where cave))
    (retract ?escape)
    (retract ?where)
    (retract ?cells)
    (inc-rept [player])
    )

(defrule M24
    ?surrender <- (surrender)
    (where planet)
    (cells)
    =>
    (assert (show M24))
    (retract ?surrender)
    (assert (give-up))
    )

(defrule M25
    (give-up)
    (where planet)
    (cells)
    =>
    (assert (show M25))
    (kill [player])
    (assert (end))
    )

(defrule Q18
    (inside)
    (where planet)
    ?free <- (free-him)
    =>
    (assert (show Q18))
    (retract ?free)
    )

(defrule Q18-A1
    ?ans <- (answer-to Q18 yes)
    =>
    (retract ?ans)
    (assert (leader))

    )


(defrule Q18-A2
    ?ans <- (answer-to Q18 no)
    =>
    (retract ?ans)
    (assert (steal-and-murder))
)

(defrule M26
    (where planet)
    (inside)
    ?steal <- (steal-and-murder)
    =>
    (assert (show M26))
    (retract ?steal)
    (assert (stabs-you))
    )

(defrule M27
    (where planet)
    (inside)
    (stabs-you)
    =>
    (assert (show M27))
    (kill [player])
    (assert (end))
    )
(defrule M28
    (where planet)
    (inside)
    ?lead <- (leader)
    =>
    (assert (show M28))
    (retract ?lead)
    (assert (thankful))
    )

(defrule M29
    (where planet)
    (inside)
    (thankful)
    =>
    (assert (show M29))
    (assert (end))
    )

;; Mostrar preguntas
(defrule end
    (declare (salience 9))
    (end)
    =>
    (halt)
)

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
        (text "Te das cuenta de que las estrellas están cambiadas, ¿Miras de nuevo?"))

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
        (text "Sigues al hombre, su nombre es Rip, a la zona de la ciudad mientras te explica que es parte de una vieja
organizacion encargada de mantener el orden a cualquier precio.")
        (answers
            "Continuas con Rip a la ciudad"
            "(Answer blocked)"))

    (question-multi (name Q9-B)
        (text "Sigues al hombre, su nombre es Rip, a la zona de la ciudad mientras te explica que es parte de una vieja
organizacion encargada de mantener el orden a cualquier precio.")
        (answers
            "Continuas con Rip a la ciudad"
            "Crees que la mejor solucion seria dispararle y volver con el objeto"))

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
        (text "Despegas en la nave y recibes una transmision, 'sino aterrizais os vuelan'"))

    (message (name M54)
        (text "Te das cuenta de que la transmision es enviada por A37"))

    (message (name M55)
        (text "Te das cuenta de que la transmision no es enviada por A37, sin embargo debería ser suya"))

    (question-yes-no (name Q11)
        (text "Podria ser conveniente hacer caso a la transmision.  ¿Rendirse?"))

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
        (text "Te encuentras dentro de una pequeña cueva, apuntando firmemente a quien en principio parece
un delincuente bastante comun sosteniendo un objeto extraño.
- ¿Estas seguro de lo que estas haciend ? - Pregunta."))

    (question-multi (name Q1)
        (text "Antes de que te de tiempo a procesar la extraña pregunta del hombre, la radio empieza a sonar.
se trata de una transmision que indica a todos los miembros de las distintas patrullas que vuelvan a reunirse
y volver a la nave.")
        (answers
            "Decides obedecer a la transmision y vuelves a la nave junto con el hombre como prisionero."
            "Ignoras la transmision y escuchas lo que tiene que decir."))

    (question-multi (name Q2-A)
        (text "Te cuestionas sobre lo que ocurre, ¿Que haces?")
        (answers
            "Amenazar"
            "Preguntar"
            "(Answer blocked)"))

    (question-multi (name Q2-B)
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

    (question-multi (name Q4-A)
        (text "¿Tienes alguna prueba?")
        (answers
            "No, no tengo nada para probarlo"
            "Si, he recogido esto antes"))

    (question-multi (name Q4-B)
        (text "¿Tienes alguna prueba?")
        (answers
            "No, no tengo nada para probarlo"
            "(Answer blocked)"))

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
            (text "Gideon nos informa que ha sido la raza X"))

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
            "Deberia ir Rip a mirar"))

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
