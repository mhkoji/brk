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

(defun square (x) (* x x))

(defun squared-distance (positionable1 positionable2)
  (let ((pos1 (position positionable1))
        (pos2 (position positionable2)))
    (+ (square (- (position-x pos1)
                  (position-x pos2)))
       (square (- (position-y pos1)
                  (position-y pos2))))))

(defclass velocity ()
  ((x :initarg :vx
      :accessor velocity-x)
   (y :initarg :vy
      :accessor velocity-y)))

(defclass world-paddle (paddle positionable) ())

(defclass world-ball (ball positionable velocity) ())

(defstruct world width height paddle ball)

(let ((unit 2))
  (defun move-paddle-left (world)
    (let ((pos (position (world-paddle world))))
      (when (< 0 (position-x pos))
        (incf (position-x pos) (- unit)))))

  (defun move-paddle-right (world)
    (let ((paddle (world-paddle world)))
      (let ((pos (position paddle)))
        (when (< (+ (position-x pos)
                    (paddle-w paddle)
                    unit)
                 (world-width world))
          (incf (position-x pos) unit)))))

  (defun move-by-velocity (positionable)
    (let ((pos (position positionable)))
      (incf (position-x pos) (* unit (velocity-x positionable)))
      (incf (position-y pos) (* unit (velocity-y positionable))))))

(defun change-ball-direction-on-collision-againt-wall (world wall)
  (let ((ball (world-ball world)))
    (let ((pos (position ball)))
      (ecase wall
        (:left
         (when (< (position-x pos) 0)
           (setf (velocity-x ball) (- (velocity-x ball)))))
        (:right
         (when (< (world-width world) (position-x pos))
           (setf (velocity-x ball) (- (velocity-x ball)))))
        (:top
         (when (< (position-y pos) 0)
           (setf (velocity-y ball) (- (velocity-y ball)))))))))

(defun ball-out-of-world-p (world)
  (let ((pos (position (world-ball world))))
    (< (world-height world) (position-y pos))))


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
                         :position (make-position :x 10 :y 440))))
        (playing-p t))
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
          (cond ((sdl:key-down-p :sdl-key-left)
                 (move-paddle-left world))
                ((sdl:key-down-p :sdl-key-right)
                 (move-paddle-right world)))
          (cond (playing-p
                 (dolist (wall '(:left :right :top))
                   (change-ball-direction-on-collision-againt-wall
                    world wall))
                 (let ((ball (world-ball world)))
                   (move-by-velocity ball))
                 (when (ball-out-of-world-p world)
                   (setf playing-p nil))
                 (sdl:clear-display sdl:*black*)
                 (draw (world-ball world) sdl:*default-display*)
                 (draw (world-paddle world) sdl:*default-display*)
                 (sdl:update-display))
                (t
                 (sdl:clear-display sdl:*black*)
                 (draw (world-paddle world) sdl:*default-display*)
                 (sdl:draw-string-solid-* "Game Over!"
                                          10 10)
                 (sdl:update-display))))))))
