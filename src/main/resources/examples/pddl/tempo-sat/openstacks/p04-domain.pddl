(define (domain openstacks-time-numeric-nonADL-nonNegated)
(:requirements :typing :durative-actions :numeric-fluents)
(:types order product)
(:constants 
 p1 p2 p3 p4 p5 p6 p7 p8 - product
 o1 o2 o3 o4 o5 o6 o7 o8 - order
)

(:predicates 
	(includes ?o - order ?p - product)
	(waiting ?o - order)
	(started ?o - order)
	(shipped ?o - order)
	(made ?p - product)
	(not-made ?p - product)
)

(:functions
(stacks-in-use) (max-stacks)
)

(:durative-action start-order
:parameters (?o - order)
:duration (= ?duration 1)
:condition (and (at start (waiting ?o))(at start (< (stacks-in-use) (max-stacks))))
:effect (and (at start (not (waiting ?o)))(at end (started ?o))(at start (increase (stacks-in-use) 1)))
)

(:durative-action make-product-p1
:parameters ()
:duration (= ?duration 80)
:condition (and (at start (not-made p1))(at start (started o1))(at start (started o6))(at start (started o7)))
:effect (and (at start (not (not-made p1))) (at end (made p1)))
)

(:durative-action make-product-p2
:parameters ()
:duration (= ?duration 60)
:condition (and (at start (not-made p2))(at start (started o5)))
:effect (and (at start (not (not-made p2))) (at end (made p2)))
)

(:durative-action make-product-p3
:parameters ()
:duration (= ?duration 80)
:condition (and (at start (not-made p3))(at start (started o8)))
:effect (and (at start (not (not-made p3))) (at end (made p3)))
)

(:durative-action make-product-p4
:parameters ()
:duration (= ?duration 30)
:condition (and (at start (not-made p4))(at start (started o3)))
:effect (and (at start (not (not-made p4))) (at end (made p4)))
)

(:durative-action make-product-p5
:parameters ()
:duration (= ?duration 40)
:condition (and (at start (not-made p5))(at start (started o2)))
:effect (and (at start (not (not-made p5))) (at end (made p5)))
)

(:durative-action make-product-p6
:parameters ()
:duration (= ?duration 50)
:condition (and (at start (not-made p6))(at start (started o5)))
:effect (and (at start (not (not-made p6))) (at end (made p6)))
)

(:durative-action make-product-p7
:parameters ()
:duration (= ?duration 40)
:condition (and (at start (not-made p7))(at start (started o4)))
:effect (and (at start (not (not-made p7))) (at end (made p7)))
)

(:durative-action make-product-p8
:parameters ()
:duration (= ?duration 20)
:condition (and (at start (not-made p8))(at start (started o3)))
:effect (and (at start (not (not-made p8))) (at end (made p8)))
)

(:durative-action ship-order-o1
:parameters ()
:duration (= ?duration 1)
:condition (and (at start (started o1))(at start (made p1)))
:effect (and (at start (not (started o1)))(at end (shipped o1))(at end (decrease (stacks-in-use) 1))))

(:durative-action ship-order-o2
:parameters ()
:duration (= ?duration 1)
:condition (and (at start (started o2))(at start (made p5)))
:effect (and (at start (not (started o2)))(at end (shipped o2))(at end (decrease (stacks-in-use) 1))))

(:durative-action ship-order-o3
:parameters ()
:duration (= ?duration 1)
:condition (and (at start (started o3))(at start (made p4))(at start (made p8)))
:effect (and (at start (not (started o3)))(at end (shipped o3))(at end (decrease (stacks-in-use) 1))))

(:durative-action ship-order-o4
:parameters ()
:duration (= ?duration 1)
:condition (and (at start (started o4))(at start (made p7)))
:effect (and (at start (not (started o4)))(at end (shipped o4))(at end (decrease (stacks-in-use) 1))))

(:durative-action ship-order-o5
:parameters ()
:duration (= ?duration 1)
:condition (and (at start (started o5))(at start (made p2))(at start (made p6)))
:effect (and (at start (not (started o5)))(at end (shipped o5))(at end (decrease (stacks-in-use) 1))))

(:durative-action ship-order-o6
:parameters ()
:duration (= ?duration 1)
:condition (and (at start (started o6))(at start (made p1)))
:effect (and (at start (not (started o6)))(at end (shipped o6))(at end (decrease (stacks-in-use) 1))))

(:durative-action ship-order-o7
:parameters ()
:duration (= ?duration 1)
:condition (and (at start (started o7))(at start (made p1)))
:effect (and (at start (not (started o7)))(at end (shipped o7))(at end (decrease (stacks-in-use) 1))))

(:durative-action ship-order-o8
:parameters ()
:duration (= ?duration 1)
:condition (and (at start (started o8))(at start (made p3)))
:effect (and (at start (not (started o8)))(at end (shipped o8))(at end (decrease (stacks-in-use) 1))))

)

