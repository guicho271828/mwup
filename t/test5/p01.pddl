(define (problem DLOG-2-2-5)
  (:domain driverlog-t)
  (:objects
   driver1 - driver
   truck1 - truck
   package1 - obj
   s0 s1 s2 - location
   )
  (:init
   (at truck1 s1)
   (driving driver1 truck1)
   (at package1 s2)
   (link s0 s1) (link s1 s0)
   (link s1 s2) (link s2 s1)
   )
  (:goal (and
          (at package1 s0))))
