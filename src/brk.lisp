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

(defclass ball ()
  ((r :initarg :r
      :reader ball-r)))

;;;

(defstruct position x y)

(defclass positionable ()
  ((position :initarg :position
             :accessor position)))

(defclass velocity ()
  ((x :initarg :vx
      :reader velocity-x)
   (y :initarg :vy
      :reader velocity-y)))

(let ((unit 2))
  (defun move-left (positionable)
    (let ((pos (position positionable)))
      (incf (position-x pos) (- unit))))

  (defun move-right (positionable)
    (let ((pos (position positionable)))
      (incf (position-x pos) unit)))

  (defun move-by-velocity (positionable)
    (let ((pos (position positionable)))
      (incf (position-x pos) (* unit (velocity-x positionable)))
      (incf (position-y pos) (* unit (velocity-y positionable))))))


(defclass world-paddle (paddle positionable) ())

(defclass world-ball (ball positionable velocity) ())

(defstruct world width height paddle ball)

(defgeneric draw (object surface))

(defmethod draw ((ball world-ball) surface)
  (let ((pos (position ball)))
    (sdl:draw-filled-circle-* (ceiling (position-x pos))
                              (ceiling (position-y pos))
                              (ball-r ball)
                              :surface surface
                              :color sdl:*white*)))

(defmethod draw ((paddle world-paddle) surface)
  (let ((pos (position paddle)))
    (sdl:draw-box-* (position-x pos)
                    (position-y pos)
                    (paddle-w paddle)
                    (paddle-h paddle)
                    :surface surface
                    :color sdl:*white*)))

(defun main ()
  (let ((world (make-world
                :width 320
                :height 480
                :ball (make-instance 'world-ball
                       :r 5
                       :vx (sin (/ pi 4))
                       :vy (- (sin (/ pi 4)))
                       :position (make-position :x 10 :y 420))
                :paddle (make-instance 'world-paddle
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
                   (move-left paddle))
                  ((sdl:key-down-p :sdl-key-right)
                   (move-right paddle))))
          (let ((ball (world-ball world)))
            (move-by-velocity ball))
          (sdl:clear-display sdl:*black*)
          (draw (world-ball world) sdl:*default-display*)
          (draw (world-paddle world) sdl:*default-display*)
          (sdl:update-display))))))
