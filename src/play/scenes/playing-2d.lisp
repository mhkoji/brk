(defpackage :brk.play.scenes.playing-2d
  (:use :cl)
  (:export :scene
           :scene-world
           :create-world
           :create-initial-state
           :state-paddle-position
           :state-ball-position
           :state-ball-velocity
           :state-bricks

           :recorded-scene
           :recorded-scene-states
           :call-with-update-world

           :playing-back-scene
           :playing-back-scene-increment-idnex!))
(in-package :brk.play.scenes.playing-2d)

(defclass scene ()
  ((world
    :accessor scene-world)))

(defgeneric scene-current-state (scene))

(defclass scene-ref ()
  ((scene
    :initarg :scene
    :reader scene)))

(defstruct state
  paddle-position
  ball-position
  ball-velocity
  bricks)

(defclass paddle (brk.2d:paddle scene-ref) ())

(defclass ball (brk.2d:ball scene-ref) ())

(defclass brick (brk.2d:brick)
  ((position
    :initarg :position
    :reader brick-position)))

(defun create-world (scene)
  (make-instance 'world
   :width 290
   :height 480
   :ball (make-instance 'ball :r 5 :scene scene)
   :paddle (make-instance 'paddle :w 30 :h 5 :scene scene)
   :scene scene))

(defun create-initial-state ()
  (make-state
   :ball-position
   (brk.2d:make-position :x 10 :y 420)
   :ball-velocity
   (brk.2d:make-velocity :x (* 6 (sin (/ pi 4)))
                         :y (* 6 (- (sin (/ pi 4)))))
   :paddle-position
   (brk.2d:make-position :x 10 :y 440)
   :bricks
   (loop for y = 20 then (+ y 20)
         while (< y 80)
         nconc
           (loop for x = 10 then (+ x 40)
                 while (< (+ x 30) 320)
                 collect
                   (let ((pos (brk.2d:make-position :x x :y y)))
                     (make-instance 'brick :w 30 :h 5 :position pos))))))


(defclass recorded-scene (scene)
  ((states
    :initform nil
    :initarg :states
    :accessor recorded-scene-states)
   (updates
    :initform nil
    :accessor recorded-scene-updates)))

(defmethod scene-current-state ((scene recorded-scene))
  (car (recorded-scene-states scene)))

(defmethod brk.2d:get-position ((paddle paddle))
  (state-paddle-position (scene-current-state (scene paddle))))

(defmethod brk.2d:get-position ((ball ball))
  (state-ball-position (scene-current-state (scene ball))))

(defmethod brk.2d:get-velocity ((ball ball))
  (state-ball-velocity (scene-current-state (scene ball))))

(defmethod brk.2d:get-position ((brick brick))
  (brick-position brick))

(defclass world (brk.2d:world scene-ref) ())

(defmethod brk.2d:world-bricks ((world world))
  (state-bricks (scene-current-state (scene world))))


(defmethod brk.2d:set-position ((paddle paddle) pos)
  (push (lambda (state)
          (setf (state-paddle-position state) pos))
        (recorded-scene-updates (scene paddle))))

(defmethod brk.2d:set-position ((ball ball) pos)
  (push (lambda (state)
          (setf (state-ball-position state) pos))
        (recorded-scene-updates (scene ball))))

(defmethod brk.2d:set-velocity ((ball ball) v)
  (push (lambda (state)
          (setf (state-ball-velocity state) v))
        (recorded-scene-updates (scene ball))))

(defmethod brk.2d:set-world-bricks ((world world) (bricks list))
  (push (lambda (state)
          (setf (state-bricks state) bricks))
        (recorded-scene-updates (scene world))))

(defun apply-updates! (recorded-scene)
  (let ((state (copy-state (scene-current-state recorded-scene))))
    (dolist (fn (recorded-scene-updates recorded-scene))
      (funcall fn state))
    (push state (recorded-scene-states recorded-scene))
    (setf (recorded-scene-updates recorded-scene) nil)))

(defun call-with-update-world (recorded-scene fn)
  (funcall fn (scene-world recorded-scene))
  (apply-updates! recorded-scene))


(defclass playing-back-scene (scene)
  ((states
    :initarg :states
    :accessor playing-back-scene-states)
   (index
    :initform 0
    :accessor playing-back-scene-index)))

(defmethod scene-current-state ((scene playing-back-scene))
  (nth (playing-back-scene-index scene) (playing-back-scene-states scene)))

(defun playing-back-scene-increment-idnex! (playing-back-scene diff)
  (let ((new-index (+ (playing-back-scene-index playing-back-scene) diff)))
    (when (<= 0 new-index)
      (if (< new-index
             (length (playing-back-scene-states playing-back-scene)))
          (setf (playing-back-scene-index playing-back-scene) new-index)
          :eof))))
