(defpackage :brk.2d
  (:use :cl)
  (:shadow :position)
  (:export :make-world
           :make-velocity
           :make-position
           :position
           :position-x
           :position-y
           :ball-out-of-world-p
           :paddle
           :ball
           :brick
           :move-ball
           :move-paddle-left
           :move-paddle-right
           :make-world
           :world-width
           :world-height
           :world-ball
           :world-paddle
           :world-bricks))
(in-package :brk.2d)

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

(defun ball-out-of-world-p (world)
  (let ((pos (position (world-ball world))))
    (< (world-height world) (position-y pos))))

(defun ball-will-collide-p (ball obj &key w h)
  (labels ((square (x) (* x x)))
    (let ((v1-x (velocity-x (velocity ball)))
          (v1-y (velocity-y (velocity ball)))
          (v2-x (funcall w obj))
          (v2-y (funcall h obj))
          (v-x (- (position-x (position obj))
                  (position-x (position ball))))
          (v-y (- (position-y (position obj))
                  (position-y (position ball)))))
      (let ((t1 (/ (- (* v-x v2-y) (* v-y v2-x))
                   (- (* v1-x v2-y) (* v1-y v2-x))))
            (t2 (/ (- (* v-x v1-y) (* v-y v1-x))
                   (- (* v1-x v2-y) (* v1-y v2-x)))))
        (and (<= 0 t1 1)
             (cond ((< t2 0)
                    (<= (+ (square (* v2-x t2))
                           (square (* v2-y t2)))
                        (square (brk:ball-r ball))))
                   ((< 1 t2)
                    (<= (+ (square (* v2-x (- t2 1)))
                           (square (* v2-y (- t2 1))))
                        (square (brk:ball-r ball))))
                   (t t)))))
    #+nil
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

(defun move-ball (world)
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
