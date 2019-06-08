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
           :move-paddle-right))
(in-package :brk.2d)

(defclass emplaced () ())

(defgeneric get-position (emplaced))

(defgeneric set-position (emplaced pos))

(defclass inertial () ())

(defgeneric get-velocity (inertial))

(defgeneric set-velocity (inertial v))


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
    :initform (list :left :right :top)
    :reader world-walls)))

(defgeneric world-bricks (world))

(defgeneric set-world-bricks (world bricks))


(defstruct position x y)

(defstruct velocity x y)


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


(defun ball-out-of-world-p (world)
  (let ((pos (get-position (world-ball world))))
    (< (world-height world) (position-y pos))))

(defun ball-will-collide-p (ball obj &key w h)
  (labels ((square (x) (* x x)))
    (let ((v1-x (velocity-x (get-velocity ball)))
          (v1-y (velocity-y (get-velocity ball)))
          (v2-x (funcall w obj))
          (v2-y (funcall h obj))
          (v-x (- (position-x (get-position obj))
                  (position-x (get-position ball))))
          (v-y (- (position-y (get-position obj))
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
    (let ((obj-center-pos-x (+ (position-x (get-position obj))
                               (/ (funcall w obj) 2)))
          (obj-center-pos-y (+ (position-y (get-position obj))
                               (/ (funcall h obj) 2))))
      (let ((ball-pos (get-position ball))
            (ball-vel (get-velocity ball)))
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
                    (square (max (ball-r ball)
                                 (/ (funcall w obj) 2)))))))))))

(defun move-by-velocity (ball &optional new-v)
  (let ((pos (get-position ball))
        (v (or new-v (get-velocity ball))))
    (let ((new-pos (make-position
                    :x (+ (position-x pos) (velocity-x v))
                    :y (+ (position-y pos) (velocity-y v)))))
      (set-position ball new-pos)
      (when new-v
        (set-velocity ball new-v)))))

(defun change-ball-direction-on-collision-against-wall (world wall)
  (let ((ball (world-ball world)))
    (let ((v (get-velocity ball))
          (pos (get-position ball)))
      (ecase wall
        (:left
         (when (< (position-x pos) 0)
           (move-by-velocity ball (make-velocity
                                   :x (- (velocity-x v))
                                   :y (velocity-y v)))
           t))
        (:right
         (when (< (world-width world) (position-x pos))
           (move-by-velocity ball (make-velocity
                                   :x (- (velocity-x v))
                                   :y (velocity-y v)))
           t))
        (:top
         (when (< (position-y pos) 0)
           (move-by-velocity ball (make-velocity
                                   :x (velocity-x v)
                                   :y (- (velocity-y v))))
           t))))))

(defun change-ball-direction-on-collision (ball)
  (let ((v (get-velocity ball)))
    (move-by-velocity ball (make-velocity
                            :x (velocity-x v)
                            :y (- (velocity-y v))))))

(defun move-ball (world)
  (let ((ball (world-ball world)))
    (or (some (lambda (w)
                (change-ball-direction-on-collision-against-wall
                 world w))
              (world-walls world))
        (cond ((ball-will-collide-p ball (world-paddle world)
                                    :w #'paddle-w
                                    :h #'paddle-h)
               (change-ball-direction-on-collision ball))
              (t
               (let ((bricks (remove-if-not
                              (lambda (brick)
                                (ball-will-collide-p ball brick
                                                     :w #'brick-w
                                                     :h #'brick-h))
                              (world-bricks world))))
                 (if bricks
                     (let ((new-bricks (remove-if (lambda (b)
                                                    (find b bricks))
                                                  (world-bricks world))))
                       (set-world-bricks world new-bricks)
                       (change-ball-direction-on-collision ball))
                     (move-by-velocity ball))))))))
