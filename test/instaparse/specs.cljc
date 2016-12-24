(ns instaparse.specs)

(def cfg1 "S = 'a'")
(def cfg2 
  "S = X
   X = Y
   Y = Z")
(def cfg3
  "S = X | Y
   Y = A Z
   Z = 'a'")
(def cfg4
  "S := A B | C
   C := (A | B) C")
(def cfg5
  "S=A?")
(def cfg6
  "S =(A | B)?")
(def cfg7
  "S = A, B?, (C C)*, D+, E")
(def cfg8
  "<S> = <A B?> (C | D)")
(def cfg9
  "S = A, &B")
(def cfg10
  "S = &B A")
(def cfg11
  "S = &B+ A")
(def cfg12
  "S = !B A")
(def cfg13
  "S = !&B A")
(def cfg15
  "S = 'a' S | Epsilon;
   C = 'b'.
   D = A")
(def cfg16
  "S = 'a' / 'b'")
(def cfg17
  "S = 'a' / 'b' | 'c'")
(def cfg18
  "S = 'a' | 'b' / 'c'")
(def cfg19
  "S = A ('a' | 'b')+
   A = !B
   B = 'a' !'b'")
(def cfg20
  "(* A comment about this grammar 
   *split* (across) lines *)
   (* And some (* nested *) comments *)
   S = (A*)
   A = 'a'")
