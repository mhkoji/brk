(asdf:defsystem :brk
  :serial t
  :pathname "src"
  :components
  ((:file "2d")
   (:file "play/scenes/playing-2d")
   (:file "play/scenes/scenes")
   (:file "play/play"))
  :depends-on (:lispbuilder-sdl))
