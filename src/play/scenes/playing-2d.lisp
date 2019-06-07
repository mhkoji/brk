(defpackage :brk.play.scenes.playing-2d
  (:use :cl)
  (:export :scene
           :scene-states
           :scene-world
           :create-world
           :create-initial-state
           :state-paddle-position
           :state-ball-position
           :state-ball-velocity
           :state-bricks
           :update-scene)
  (:import-from :brk.2d
                :move-ball))
(in-package :brk.play.scenes.playing-2d)

(defclass scene ()
  ((world
    :accessor scene-world)
   (states
    :initform nil
    :initarg :states
    :accessor scene-states)
   (updates
    :initform nil
    :accessor scene-updates)))

(defclass scene-ref ()
  ((scene
    :initarg :scene
    :reader scene)))

(defstruct state
  paddle-position
  ball-position
  ball-velocity
  bricks)

(defclass paddle (brk:paddle scene-ref) ())

(defclass ball (brk:ball scene-ref) ())

(defclass brick (brk:brick)
  ((position
    :initarg :position
    :reader brick-position)))

(defun scene-last-state (scene)
  (car (scene-states scene)))

(defmethod brk:get-position ((paddle paddle))
  (state-paddle-position (scene-last-state (scene paddle))))

(defmethod brk:set-position ((paddle paddle) pos)
  (push (lambda (state)
          (setf (state-paddle-position state) pos))
        (scene-updates (scene paddle))))


(defmethod brk:get-position ((ball ball))
  (state-ball-position (scene-last-state (scene ball))))

(defmethod brk:set-position ((ball ball) pos)
  (push (lambda (state)
          (setf (state-ball-position state) pos))
        (scene-updates (scene ball))))

(defmethod brk:get-velocity ((ball ball))
  (state-ball-velocity (scene-last-state (scene ball))))

(defmethod brk:set-velocity ((ball ball) v)
  (push (lambda (state)
          (setf (state-ball-velocity state) v))
        (scene-updates (scene ball))))


(defmethod brk:get-position ((brick brick))
  (brick-position brick))


(defclass world (brk.2d:world scene-ref) ())

(defmethod brk.2d:world-bricks ((world world))
  (state-bricks (scene-last-state (scene world))))

(defmethod brk.2d:set-world-bricks ((world world) (bricks list))
  (push (lambda (state)
          (setf (state-bricks state) bricks))
        (scene-updates (scene world))))

(defun apply-updates! (scene)
  (let ((state (copy-state (scene-last-state scene))))
    (dolist (fn (scene-updates scene))
      (funcall fn state))
    (push state (scene-states scene))
    (setf (scene-updates scene) nil)))

(defun update-scene (scene &key paddle-direction)
  (let ((world (scene-world scene)))
    (case paddle-direction
      (:left
       (brk.2d:move-paddle-left world))
      (:right
       (brk.2d:move-paddle-right world)))
    (brk.2d:move-ball world))
  (apply-updates! scene))

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
