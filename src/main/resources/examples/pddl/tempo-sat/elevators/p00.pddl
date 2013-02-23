(define (problem elevators-time-p8_4_1)
(:domain elevators-time-numeric)

(:objects 
f0 f1 f2 f3 - floor
p0 - passenger
fast0 fast1  - fast-elevator
slow0-0 - slow-elevator
)

(:init
(above f0 f1) (above f0 f2) (above f0 f3) 
(above f1 f2) (above f1 f3) 
(above f2 f3) 

(lift-at fast0 f0)
(= (passengers fast0) 0)
(= (capacity fast0) 3)
(reachable-floor fast0 f0)(reachable-floor fast0 f2)

(lift-at fast1 f0)
(= (passengers fast1) 0)
(= (capacity fast1) 3)
(reachable-floor fast1 f0)(reachable-floor fast1 f2)

(lift-at slow0-0 f0)
(= (passengers slow0-0) 0)
(= (capacity slow0-0) 2)
(reachable-floor slow0-0 f0)(reachable-floor slow0-0 f1)(reachable-floor slow0-0 f2)(reachable-floor slow0-0 f3)

(passenger-at p0 f0)

(= (travel-slow f0 f1) 12) (= (travel-slow f0 f2) 20) (= (travel-slow f0 f3) 28) (= (travel-slow f1 f2) 12) (= (travel-slow f1 f3) 20) (= (travel-slow f2 f3) 12)   

(= (travel-fast f0 f2) 11) 

)

(:goal
(and
(passenger-at p0 f1)
))

(:metric minimize (total-time))

)
