(defpackage :brk
  (:use :cl)
  (:export :main))
(in-package :brk)

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
        (sdl:update-display)))))
