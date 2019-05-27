(defpackage :brk.gaming.2d
  (:use :cl)
  (:shadow :position)
  (:export :make-world
           :make-velocity
           :make-position
           :position
           :position-x
           :position-y
           :auto-update-world
           :ball-out-of-world-p
           :paddle
           :ball
           :brick
           :move-paddle-left
           :move-paddle-right
           :make-world
           :world-width
           :world-height
           :world-ball
           :world-paddle
           :world-bricks))
(in-package :brk.gaming.2d)

(defstruct position x y)

(defstruct velocity x y)

(defclass positionable ()
  ((position :initarg :position
             :accessor position)))

(defclass move-automatically ()
  ((velocity :initarg :velocity
             :accessor velocity)))


(defclass paddle (brk:paddle positionable) ())

(defclass ball (brk:ball positionable move-automatically) ())

(defclass brick (brk:brick positionable) ())


(defstruct world width height paddle ball walls bricks)

(defun change-ball-direction-on-collision-against-wall (world wall)
  (let ((ball (world-ball world)))
    (let ((v (velocity ball))
          (pos (position ball)))
      (ecase wall
        (:left
         (when (< (position-x pos) 0)
           (setf (velocity-x v) (- (velocity-x v)))
           t))
        (:right
         (when (< (world-width world) (position-x pos))
           (setf (velocity-x v) (- (velocity-x v)))
           t))
        (:top
         (when (< (position-y pos) 0)
           (setf (velocity-y v) (- (velocity-y v)))
           t))))))

(defun change-ball-direction-on-collision (world)
  (let ((v (velocity (world-ball world))))
    (setf (velocity-y v) (- (velocity-y v)))))

(defun ball-will-collide-p (ball obj &key w h)
  (labels ((square (x) (* x x)))
    (let ((obj-center-pos-x (+ (position-x (position obj))
                               (/ (funcall w obj) 2)))
          (obj-center-pos-y (+ (position-y (position obj))
                               (/ (funcall h obj) 2))))
      (let ((ball-pos (position ball))
            (ball-vel (velocity ball)))
        (let ((squared-distance-before
               (+ (square (- obj-center-pos-x
                             (position-x ball-pos)))
                  (square (- obj-center-pos-y
                             (position-y ball-pos)))))
              (squared-distance-after
               (+ (square (- obj-center-pos-x
                             (+ (position-x ball-pos)
                                (velocity-x ball-vel))))
                  (square (- obj-center-pos-y
                             (+ (position-y ball-pos)
                                (velocity-y ball-vel)))))))
          (let ((getting-close-p
                 (> squared-distance-before squared-distance-after)))
            (and getting-close-p
                 (< squared-distance-after
                    (square (max (brk:ball-r ball)
                                 (/ (funcall w obj) 2)))))))))))

(let ((unit 2))
  (defun move-paddle-left (world)
    (let ((pos (position (world-paddle world))))
      (when (< 0 (position-x pos))
        (incf (position-x pos) (- unit)))))

  (defun move-paddle-right (world)
    (let ((paddle (world-paddle world)))
      (let ((pos (position paddle)))
        (when (< (+ (position-x pos)
                    (brk:paddle-w paddle)
                    unit)
                 (world-width world))
          (incf (position-x pos) unit))))))

(defun move-by-velocity (positionable)
  (let ((v (velocity positionable))
        (pos (position positionable)))
    (incf (position-x pos) (velocity-x v))
    (incf (position-y pos) (velocity-y v))))

(defun ball-out-of-world-p (world)
  (let ((pos (position (world-ball world))))
    (< (world-height world) (position-y pos))))


(defun auto-update-world (world)
  (let ((ball (world-ball world)))
    (cond ((some (lambda (w)
                   (change-ball-direction-on-collision-against-wall
                    world w))
                 (world-walls world))
           (move-by-velocity ball))
          ((ball-will-collide-p ball (world-paddle world)
                                :w #'brk:paddle-w
                                :h #'brk:paddle-h)
           (change-ball-direction-on-collision world)
           (move-by-velocity ball))
          (t
           (let ((bricks (remove-if-not
                          (lambda (brick)
                            (ball-will-collide-p ball brick
                                                 :w #'brk:brick-w
                                                 :h #'brk:brick-h))
                          (world-bricks world))))
             (if bricks
                 (progn
                   (setf (world-bricks world)
                         (remove-if (lambda (b) (find b bricks))
                                    (world-bricks world)))
                   (change-ball-direction-on-collision world)
                   (move-by-velocity ball))
                 (move-by-velocity ball)))))))
