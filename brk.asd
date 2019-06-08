(asdf:defsystem :brk
  :serial t
  :pathname "src"
  :components
  ((:file "2d/2d")
   (:file "2d/play")
   (:file "sdl/scenes")
   (:file "sdl/sdl"))
  :depends-on (:lispbuilder-sdl))
