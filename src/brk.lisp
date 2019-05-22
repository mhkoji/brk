(defpackage :brk
  (:use :cl)
  (:shadow :position)
  (:export :main))
(in-package :brk)

(defclass paddle ()
  ((w :initarg :w
      :reader paddle-w)
   (h :initarg :h
      :reader paddle-h)))

;;;

(defstruct position x y)

(defclass positionable ()
  ((position :initarg :position
             :accessor position)))

(defun move (positionable direction)
  (let ((pos (position positionable))
        (unit 2))
    (case direction
      (:left
       (incf (position-x pos) (- unit)))
      (:right
       (incf (position-x pos) unit)))))


(defclass positionable-paddle (paddle positionable) ())

(defun draw (paddle surface)
  (sdl:clear-display sdl:*black*
                     :surface surface)
  (let ((pos (position paddle)))
    (sdl:draw-box-* (position-x pos)
                    (position-y pos)
                    (paddle-w paddle)
                    (paddle-h paddle)
                    :surface surface
                    :color sdl:*white*))
  (sdl:update-display surface))

(defstruct world width height paddle)

(defun main ()
  (let ((world (make-world
                :width 320
                :height 480
                :paddle (make-instance 'positionable-paddle
                         :w 30
                         :h 5
                         :position (make-position :x 10 :y 440)))))
    (sdl:with-init ()
      (sdl:window (world-width world)
                  (world-height world)
                  :title-caption "BRK")
      (setf (sdl:frame-rate) 60)
      (sdl:with-events ()
        (:quit-event () t)
        (:video-expose-event ()
          (sdl:update-display))
        (:idle ()
          (let ((paddle (world-paddle world)))
            (cond ((sdl:key-down-p :sdl-key-left)
                   (move paddle :left))
                  ((sdl:key-down-p :sdl-key-right)
                   (move paddle :right)))
            (draw paddle sdl:*default-display*)))))))
