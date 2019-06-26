(defpackage :brk.2d.play
  (:use :cl)
  (:export :scene
           :scene-world
           :state-paddle-position
           :state-ball-position
           :state-ball-velocity
           :state-bricks

           :recording-scene
           :recording-scene-states
           :call-with-update-world

           :playing-back-scene
           :playing-back-scene-increment-idnex!))
(in-package :brk.2d.play)

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

(defclass world (brk.2d:world scene-ref) ())

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


(defclass recording-scene (scene)
  ((states
    :initform nil
    :initarg :states
    :accessor recording-scene-states)
   (updates
    :initform nil
    :accessor recording-scene-updates)))

(defmethod initialize-instance :after ((scene recording-scene) &key)
  (with-accessors ((world scene-world)
                   (states recording-scene-states)) scene
    (setf world (create-world scene))
    (setf states (list (create-initial-state)))))

(defmethod scene-current-state ((scene recording-scene))
  (car (recording-scene-states scene)))

(defmethod brk.2d:get-position ((paddle paddle))
  (state-paddle-position (scene-current-state (scene paddle))))

(defmethod brk.2d:get-position ((ball ball))
  (state-ball-position (scene-current-state (scene ball))))

(defmethod brk.2d:get-velocity ((ball ball))
  (state-ball-velocity (scene-current-state (scene ball))))

(defmethod brk.2d:get-position ((brick brick))
  (brick-position brick))

(defmethod brk.2d:world-bricks ((world world))
  (state-bricks (scene-current-state (scene world))))


(defmethod brk.2d:set-position ((paddle paddle) pos)
  (push (lambda (state)
          (setf (state-paddle-position state) pos))
        (recording-scene-updates (scene paddle))))

(defmethod brk.2d:set-position ((ball ball) pos)
  (push (lambda (state)
          (setf (state-ball-position state) pos))
        (recording-scene-updates (scene ball))))

(defmethod brk.2d:set-velocity ((ball ball) v)
  (push (lambda (state)
          (setf (state-ball-velocity state) v))
        (recording-scene-updates (scene ball))))

(defmethod brk.2d:set-world-bricks ((world world) (bricks list))
  (push (lambda (state)
          (setf (state-bricks state) bricks))
        (recording-scene-updates (scene world))))


(defun apply-updates! (recording-scene)
  (let ((new-state
         (reduce (lambda (state fn)
                   (funcall fn state)
                   state)
                 (recording-scene-updates recording-scene)
                 :initial-value (copy-state
                                 (scene-current-state recording-scene)))))
    (push new-state (recording-scene-states recording-scene))
    (setf (recording-scene-updates recording-scene) nil)))

(defun call-with-update-world (recording-scene fn)
  (funcall fn (scene-world recording-scene))
  (apply-updates! recording-scene))


(defclass playing-back-scene (scene)
  ((states
    :initarg :states
    :accessor playing-back-scene-states)
   (index
    :initform 0
    :accessor playing-back-scene-index)))

(defmethod initialize-instance :after ((scene playing-back-scene) &key)
  (with-accessors ((world scene-world)) scene
    (setf world (create-world scene))))

(defmethod scene-current-state ((scene playing-back-scene))
  (nth (playing-back-scene-index scene) (playing-back-scene-states scene)))

(defun playing-back-scene-increment-idnex! (playing-back-scene diff)
  (let ((new-index (+ (playing-back-scene-index playing-back-scene) diff)))
    (when (<= 0 new-index)
      (if (< new-index
             (length (playing-back-scene-states playing-back-scene)))
          (setf (playing-back-scene-index playing-back-scene) new-index)
          :eof))))
