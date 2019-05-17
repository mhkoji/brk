(defpackage :brk
  (:use :cl)
  (:export :main))
(in-package :brk)

(defun draw (surface)
  (sdl:draw-box-* 10 440 30 5
                  :surface surface
                  :color sdl:*white*)
  (sdl:update-display surface))

(defun main ()
  (sdl:with-init ()
    (sdl:window 320 480
                :title-caption "BRK")
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event ()
        (sdl:update-display))
      (:idle ()
        (draw sdl:*default-display*)))))
