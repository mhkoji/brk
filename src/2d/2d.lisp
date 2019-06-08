(defpackage :brk.2d
  (:use :cl)
  (:shadow :position)
  (:export :paddle
           :paddle-w
           :paddle-h
           :ball
           :ball-r
           :brick
           :brick-w
           :brick-h
           :get-position
           :set-position
           :get-velocity
           :set-velocity
           :world
           :world-width
           :world-height
           :world-paddle
           :world-ball
           :world-walls
           :world-bricks
           :set-world-bricks
           :make-velocity
           :make-position
           :position
           :position-x
           :position-y
           :ball-out-of-world-p
           :move-ball
           :move-paddle-left
           :move-paddle-right)
  (:import-from :alexandria
                :if-let))
(in-package :brk.2d)

(defclass emplaced () ())
(defgeneric get-position (emplaced))
(defgeneric set-position (emplaced pos))

(defclass inertial () ())
(defgeneric get-velocity (inertial))
(defgeneric set-velocity (inertial v))


(defstruct position x y)

(defstruct velocity x y)


(defclass paddle (emplaced)
  ((w :initarg :w
      :reader paddle-w)
   (h :initarg :h
      :reader paddle-h)))

(defclass ball (emplaced inertial)
  ((r :initarg :r
      :reader ball-r)))

(defclass brick (emplaced)
  ((w :initarg :w
      :reader brick-w)
   (h :initarg :h
      :reader brick-h)))


(defclass world ()
  ((width
    :initarg :width
    :reader world-width)
   (height
    :initarg :height
    :reader world-height)
   (paddle
    :initarg :paddle
    :reader world-paddle)
   (ball
    :initarg :ball
    :reader world-ball)
   (walls
    :initform (list :wall-left :wall-right :wall-top)
    :reader world-walls)))

(defgeneric world-bricks (world))

(defgeneric set-world-bricks (world bricks))


(let ((unit 2))
  (defun move-paddle-left (world)
    (let ((paddle (world-paddle world)))
      (let ((pos (get-position paddle)))
        (when (< 0 (position-x pos))
          (let ((new-pos (make-position :x (+ (position-x pos) (- unit))
                                        :y (position-y pos))))
            (set-position paddle new-pos))))))

  (defun move-paddle-right (world)
    (let ((paddle (world-paddle world)))
      (let ((pos (get-position paddle)))
        (when (< (+ (position-x pos) (paddle-w paddle) unit)
                 (world-width world))
          (let ((new-pos (make-position :x (+ (position-x pos) unit)
                                        :y (position-y pos))))
            (set-position paddle new-pos)))))))


(defun move-ball-by-velocity (ball &optional new-vel)
  (let ((pos (get-position ball))
        (vel (or new-vel (get-velocity ball))))
    (let ((new-pos (make-position
                    :x (+ (position-x pos) (velocity-x vel))
                    :y (+ (position-y pos) (velocity-y vel)))))
      (set-position ball new-pos)
      (when new-vel
        (set-velocity ball new-vel)))))

(defun ball-out-of-world-p (world)
  (let ((pos (get-position (world-ball world))))
    (< (world-height world) (position-y pos))))

(defun ball-will-collide-against-rect-p (ball rect &key w h)
  (labels ((square (x) (* x x)))
    (let ((v1-x (velocity-x (get-velocity ball)))
          (v1-y (velocity-y (get-velocity ball)))
          (v2-x (funcall w rect))
          (v2-y (funcall h rect))
          (v-x (- (position-x (get-position rect))
                  (position-x (get-position ball))))
          (v-y (- (position-y (get-position rect))
                  (position-y (get-position ball)))))
      (let ((t1 (/ (- (* v-x v2-y) (* v-y v2-x))
                   (- (* v1-x v2-y) (* v1-y v2-x))))
            (t2 (/ (- (* v-x v1-y) (* v-y v1-x))
                   (- (* v1-x v2-y) (* v1-y v2-x)))))
        (and (<= 0 t1 1)
             (cond ((< t2 0)
                    (<= (+ (square (* v2-x t2))
                           (square (* v2-y t2)))
                        (square (ball-r ball))))
                   ((< 1 t2)
                    (<= (+ (square (* v2-x (- t2 1)))
                           (square (* v2-y (- t2 1))))
                        (square (ball-r ball))))
                   (t t)))))
    #+nil
    (let ((rect-center-pos-x (+ (position-x (get-position rect))
                               (/ (funcall w rect) 2)))
          (rect-center-pos-y (+ (position-y (get-position rect))
                               (/ (funcall h rect) 2))))
      (let ((ball-pos (get-position ball))
            (ball-vel (get-velocity ball)))
        (let ((squared-distance-before
               (+ (square (- rect-center-pos-x
                             (position-x ball-pos)))
                  (square (- rect-center-pos-y
                             (position-y ball-pos)))))
              (squared-distance-after
               (+ (square (- rect-center-pos-x
                             (+ (position-x ball-pos)
                                (velocity-x ball-vel))))
                  (square (- rect-center-pos-y
                             (+ (position-y ball-pos)
                                (velocity-y ball-vel)))))))
          (let ((getting-close-p
                 (> squared-distance-before squared-distance-after)))
            (and getting-close-p
                 (< squared-distance-after
                    (square (max (ball-r ball)
                                 (/ (funcall w rect) 2)))))))))))


(defgeneric ball-will-collide-p (ball obj))

(defmethod ball-will-collide-p ((ball ball) (paddle paddle))
  (ball-will-collide-against-rect-p ball paddle
                                    :w #'paddle-w
                                    :h #'paddle-h))

(defmethod ball-will-collide-p ((ball ball) (brick brick))
  (ball-will-collide-against-rect-p ball brick
                                    :w #'brick-w
                                    :h #'brick-h))

(defun ball-will-collide-against-wall-p (world ball wall)
  (let ((pos (get-position ball)))
    (ecase wall
      (:wall-left  (<    (position-x pos)               0))
      (:wall-right (< (world-width world) (position-x pos)))
      (:wall-top   (<    (position-y pos)               0)))))

(defun handle-ball-collision-against-wall (ball wall)
  (let ((vel (get-velocity ball)))
    (let ((new-vel
           (ecase wall
             (:wall-left  (make-velocity :x (- (velocity-x vel))
                                         :y (velocity-y vel)))
             (:wall-right (make-velocity :x (- (velocity-x vel))
                                         :y (velocity-y vel)))
             (:wall-top   (make-velocity :x (velocity-x vel)
                                         :y (- (velocity-y vel)))))))
      (move-ball-by-velocity ball new-vel))))

(defun handle-ball-collision (ball)
  (let ((vel (get-velocity ball)))
    (let ((new-vel (make-velocity :x (velocity-x vel)
                                  :y (- (velocity-y vel)))))
      (move-ball-by-velocity ball new-vel))))

(defun move-ball (world)
  (let ((ball (world-ball world)))
    (if-let ((collided-wall
              (find-if (lambda (wall)
                         (ball-will-collide-against-wall-p world ball wall))
                       (world-walls world))))
      (handle-ball-collision-against-wall ball collided-wall)
      (if (ball-will-collide-p ball (world-paddle world))
          (handle-ball-collision ball)
          (if-let ((collided-bricks
                    (remove-if-not (lambda (brick)
                                     (ball-will-collide-p ball brick))
                                   (world-bricks world))))
            (let ((remaining-bricks (remove-if (lambda (b)
                                                 (find b collided-bricks))
                                               (world-bricks world))))
              (set-world-bricks world remaining-bricks)
              (handle-ball-collision ball))
            (move-ball-by-velocity ball))))))
