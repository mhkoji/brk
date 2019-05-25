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

(defstruct velocity x y)

(defclass positionable ()
  ((position :initarg :position
             :accessor position)))

(defclass move-automatically ()
  ((velocity :initarg :velocity
             :accessor velocity)))

(defclass world-paddle (paddle positionable) ())

(defclass world-ball (ball positionable move-automatically) ())

(defclass world-brick (brick positionable) ())

(defstruct world width height paddle ball walls bricks)

(defun change-ball-direction-on-collision-against-wall (world wall)
  (let ((ball (world-ball world)))
    (let ((v (velocity ball))
          (pos (position ball)))
      (ecase wall
        (:left
         (when (< (position-x pos) 0)
           (setf (velocity-x v) (- (velocity-x v)))))
        (:right
         (when (< (world-width world) (position-x pos))
           (setf (velocity-x v) (- (velocity-x v)))))
        (:top
         (when (< (position-y pos) 0)
           (setf (velocity-y v) (- (velocity-y v)))))))))

(defun change-ball-direction-on-collision (world)
  (let ((v (velocity (world-ball world))))
    (setf (velocity-y v) (- (velocity-y v)))))

(defun collide-p (ball obj &key w h)
  (labels ((square (x)
             (* x x)))
    (let ((ball-pos (position ball))
          (obj-pos (position obj)))
      (< (+ (square (- (+ (position-x obj-pos)
                          (/ (funcall w obj) 2))
                       (position-x ball-pos)))
            (square (- (+ (position-y obj-pos)
                          (/ (funcall h obj) 2))
                       (position-y ball-pos))))
         (square (max (ball-r ball)
                      (/ (funcall w obj) 2)))))))

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
    (let ((v (velocity positionable))
          (pos (position positionable)))
      (incf (position-x pos) (* unit (velocity-x v)))
      (incf (position-y pos) (* unit (velocity-y v))))))

(defun ball-out-of-world-p (world)
  (let ((pos (position (world-ball world))))
    (< (world-height world) (position-y pos))))


(defun auto-update-world (world)
  (dolist (wall (world-walls world))
    (change-ball-direction-on-collision-against-wall world wall))
  (let ((ball (world-ball world)))
    (move-by-velocity ball)

    (when (collide-p ball (world-paddle world)
                     :w #'paddle-w
                     :h #'paddle-h)
      (change-ball-direction-on-collision world))

    (let ((bricks (remove-if-not (lambda (brick)
                                   (collide-p ball brick
                                              :w #'brick-w
                                              :h #'brick-h))
                                 (world-bricks world))))
      (when bricks
        (change-ball-direction-on-collision world)
        (dolist (brick bricks)
          (setf (world-bricks world)
                (remove brick (world-bricks world))))))))


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


(defun create-world ()
  (make-world
   :width 290
   :height 480
   :ball (make-instance 'world-ball
                        :r 5
                        :velocity
                        (make-velocity
                         :x (* 3 (sin (/ pi 4)))
                         :y (* 3 (- (sin (/ pi 4)))))
                        :position
                        (make-position :x 10 :y 420))
   :paddle (make-instance 'world-paddle
                          :w 30
                          :h 5
                          :position (make-position :x 10 :y 440))
   :walls (list :left :right :top)
   :bricks (loop for y = 20 then (+ y 20)
                 while (< y 80)
                 nconc
                (loop for x = 10 then (+ x 40)
                      while (< (+ x 30) 320)
                      collect
                     (make-instance 'world-brick
                                    :w 30
                                    :h 5
                                    :position (make-position :x x :y y))))))

(defun main ()
  (sdl:initialise-default-font)
  (let ((world (create-world))
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
          (cond ((and playing-p
                      (null (world-bricks world)))
                 (sdl:clear-display sdl:*black*)
                 (draw (world-paddle world) sdl:*default-display*)
                 (sdl:draw-string-solid-* "Congratulations!"
                                          10 10)
                 (sdl:update-display))
                (playing-p
                 (auto-update-world world)
                 (when (ball-out-of-world-p world)
                   (setf playing-p nil))
                 (sdl:clear-display sdl:*black*)
                 (draw (world-ball world) sdl:*default-display*)
                 (draw (world-paddle world) sdl:*default-display*)
                 (dolist (brick (world-bricks world))
                   (draw brick sdl:*default-display*))
                 (sdl:update-display))
                ((sdl:key-down-p :sdl-key-space)
                 (setq world (create-world))
                 (setq playing-p t))
                (t
                 (sdl:clear-display sdl:*black*)
                 (draw (world-paddle world) sdl:*default-display*)
                 (sdl:draw-string-solid-* "Game Over!"
                                          10 10)
                 (sdl:draw-string-solid-* "Press SPACE key..."
                                          10 20)
                 (sdl:update-display))))))))
