(asdf:defsystem :brk
  :serial t
  :pathname "src"
  :components
  ((:file "brk")
   (:file "gaming/2d")
   (:file "gaming/gaming"))
  :depends-on (:lispbuilder-sdl))
