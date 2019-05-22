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

(defclass brick ()
  ((w :initarg :w
      :reader brick-w)
   (h :initarg :h
      :reader brick-h)))

;;;

(defstruct position x y)

(defclass positionable ()
  ((position :initarg :position
             :accessor position)))

(defclass velocity ()
  ((x :initarg :vx
      :accessor velocity-x)
   (y :initarg :vy
      :accessor velocity-y)))

(defclass world-paddle (paddle positionable) ())

(defclass world-ball (ball positionable velocity) ())

(defclass world-brick (brick positionable) ())

(defstruct world width height paddle ball walls bricks)

(defun change-ball-direction-on-collision-against-wall (world wall)
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

(defun ball-out-of-world-p (world)
  (let ((pos (position (world-ball world))))
    (< (world-height world) (position-y pos))))


(defun auto-update-world (world)
  (dolist (wall (world-walls world))
    (change-ball-direction-on-collision-against-wall world wall))
  (let ((ball (world-ball world)))
    (move-by-velocity ball)))


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

(defmethod draw ((brick world-brick) surface)
  (let ((pos (position brick)))
    (sdl:draw-box-* (position-x pos)
                    (position-y pos)
                    (brick-w brick)
                    (brick-h brick)
                    :surface surface
                    :color sdl:*white*)))

(defun main ()
  (let ((world (make-world
                :width 320
                :height 480
                :ball (make-instance 'world-ball
                       :r 5
                       :vx (* 3 (sin (/ pi 4)))
                       :vy (* 3 (- (sin (/ pi 4))))
                       :position (make-position :x 10 :y 420))
                :paddle (make-instance 'world-paddle
                         :w 30
                         :h 5
                         :position (make-position :x 10 :y 440))
                :walls (list :left :right :top)
                :bricks (list (make-instance 'world-brick
                                             :w 30
                                             :h 5
                                             :position (make-position
                                                        :x 10 :y 20)))))
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
                 (auto-update-world world)
                 (when (ball-out-of-world-p world)
                   (setf playing-p nil))
                 (sdl:clear-display sdl:*black*)
                 (draw (world-ball world) sdl:*default-display*)
                 (draw (world-paddle world) sdl:*default-display*)
                 (dolist (brick (world-bricks world))
                   (draw brick sdl:*default-display*))
                 (sdl:update-display))
                (t
                 (sdl:clear-display sdl:*black*)
                 (draw (world-paddle world) sdl:*default-display*)
                 (sdl:draw-string-solid-* "Game Over!"
                                          10 10)
                 (sdl:update-display))))))))
