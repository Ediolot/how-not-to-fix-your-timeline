
(defclass PLAYER
    (is-a USER)
    (slot value)
)

(definstances insta
    (player of PLAYER (value 12))
)

(deffacts name
    (start)
)

(defrule above-threshold
    (object (is-a PLAYER)
            (value ?value))
    (test (> ?value 10))
    =>
    (printout t "*** Above threshold!! ***" crlf))

(defrule start
    (start)
    (never)
    =>
    (send [player] put-value 11)
)
