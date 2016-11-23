(define (problem DLOG-2-2-4)
  (:domain driverlog-t)
  (:objects
   driver1
   driver2 - driver
   truck1
   truck2 - truck
   package1
   package2
   package3 - obj
   s0
   s1
   s2
   p0-1
   p0-2
   p1-0
   p2-1 - location
   )
  (:init
   (at driver1 s0)
   (at driver2 s0)
   (at truck1 s0)
   (empty truck1)
   (at truck2 s1)
   (empty truck2)
   (at package1 s2)
   (at package2 s1)
   (at package3 s1)
   (path s0 p0-1)
   (path p0-1 s0)
   (path s1 p0-1)
   (path p0-1 s1)
   (path s0 p0-2)
   (path p0-2 s0)
   (path s2 p0-2)
   (path p0-2 s2)
   (path s2 p2-1)
   (path p2-1 s2)
   (path s1 p2-1)
   (path p2-1 s1)
   (link s0 s2)
   (link s2 s0)
   (link s1 s0)
   (link s0 s1)
   (link s1 s2)
   (link s2 s1)
   )
  (:goal (and
          (at driver1 s1)
          (at driver2 s1)
          (at truck1 s2)
          (at truck2 s0)
          (at package1 s0)
          (at package2 s2)
          (at package3 s0))))