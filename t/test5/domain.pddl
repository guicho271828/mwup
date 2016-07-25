(define (domain driverlog-t)
  (:requirements :strips)
  (:types obj truck location driver)
  (:predicates (at ?obj - object ?loc - location)
               (in ?obj1 - obj ?obj2 - truck)
               (driving ?d - driver ?v - truck)
               (link ?x ?y - location)
               (path ?x ?y - location)
               (empty ?v - truck))

  (:action LOAD :parameters (?obj - obj ?truck - truck ?loc - location)
           :precondition (and (at ?truck ?loc) (at ?obj ?loc))
           :effect (and (not (at ?obj ?loc)) (in ?obj ?truck)))

  (:action UNLOAD
           :parameters (?obj - obj ?truck - truck ?loc - location)
           :precondition (and (at ?truck ?loc) (in ?obj ?truck))
           :effect (and (not (in ?obj ?truck)) (at ?obj ?loc)))

  (:action BOARD
           :parameters (?driver - driver ?truck - truck ?loc - location)
           :precondition (and (at ?truck ?loc) (at ?driver ?loc) (empty ?truck))
           :effect (and (not (at ?driver ?loc)) (driving ?driver ?truck) (not (empty ?truck))))

  (:action DISEMBARK
           :parameters (?driver - driver ?truck - truck ?loc - location)
           :precondition (and (at ?truck ?loc) (driving ?driver ?truck))
           :effect (and (not (driving ?driver ?truck)) (at ?driver ?loc) (empty ?truck)))

  (:action DRIVE
           :parameters (?truck - truck ?loc-from ?loc-to - location ?driver - driver)
           :precondition (and (at ?truck ?loc-from)
                              (driving ?driver ?truck) (link ?loc-from ?loc-to))
           :effect (and (not (at ?truck ?loc-from)) (at ?truck ?loc-to)))

  (:action WALK
           :parameters (?driver - driver ?loc-from ?loc-to - location)
           :precondition (and (at ?driver ?loc-from) (path ?loc-from ?loc-to))
           :effect (and (not (at ?driver ?loc-from)) (at ?driver ?loc-to))))
