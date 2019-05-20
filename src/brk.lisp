(defpackage :brk
  (:use :cl)
  (:shadow :position)
  (:export :main))
(in-package :brk)

(defstruct position x y)

(defun move (pos direction)
  (let ((unit 2))
    (case direction
      (:left
       (incf (position-x pos) (- unit)))
      (:right
       (incf (position-x pos) unit)))))


(defstruct paddle w h)

(defun draw (paddle paddle-pos surface)
  (sdl:clear-display sdl:*black*
                     :surface surface)
  (sdl:draw-box-* (position-x paddle-pos)
                  (position-y paddle-pos)
                  (paddle-w paddle)
                  (paddle-h paddle)
                  :surface surface
                  :color sdl:*white*)
  (sdl:update-display surface))


(defun main ()
  (sdl:with-init ()
    (sdl:window 320 480
                :title-caption "BRK")
    (setf (sdl:frame-rate) 60)
    (let ((paddle (make-paddle :w 30 :h 5))
          (paddle-pos (make-position :x 10 :y 440)))
      (sdl:with-events ()
        (:quit-event () t)
        (:video-expose-event ()
          (sdl:update-display))
        (:idle ()
          (cond ((sdl:key-down-p :sdl-key-left)
                 (move paddle-pos :left))
                ((sdl:key-down-p :sdl-key-right)
                 (move paddle-pos :right)))
          (draw paddle paddle-pos sdl:*default-display*))))))
