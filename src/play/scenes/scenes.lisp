(defpackage :brk.play.scenes
  (:use :cl)
  (:export :handle-idle
           :title-scene
           :playing-scene))
(in-package :brk.play.scenes)

(defgeneric handle-idle (scene))

(defun clear-display ()
  (sdl:clear-display sdl:*black*))

;;;

(defclass title-scene ()
  ((selected-option
    :initform :play
    :accessor title-scene-selected-option)))

(defmethod handle-idle ((scene title-scene))
  (with-accessors ((selected-option title-scene-selected-option)) scene
    (clear-display)
    (sdl:draw-string-solid-* "Play" 20 20)
    (sdl:draw-string-solid-* "Review plays" 20 40)
    (let ((> ">"))
      (ecase selected-option
        (:play
         (sdl:draw-string-solid-* > 10 20))
        (:review
         (sdl:draw-string-solid-* > 10 40))))
    (sdl:update-display)
    (cond ((and (sdl:key-down-p :sdl-key-return)
                (eql selected-option :play))
           (make-instance 'playing-scene))
          (t
           (when (eql selected-option :play)
             (when (sdl:key-down-p :sdl-key-down)
               (setf selected-option :review)))
           (when (eql selected-option :review)
             (when (sdl:key-down-p :sdl-key-up)
               (setf selected-option :play)))
           scene))))

;;;

(defun draw-ball (ball surface)
  (let ((pos (brk:get-position ball)))
    (sdl:draw-filled-circle-* (ceiling (brk.2d:position-x pos))
                              (ceiling (brk.2d:position-y pos))
                              (brk:ball-r ball)
                              :surface surface
                              :color sdl:*white*)))

(defun draw-paddle (paddle surface)
  (let ((pos (brk:get-position paddle)))
    (sdl:draw-box-* (brk.2d:position-x pos)
                    (brk.2d:position-y pos)
                    (brk:paddle-w paddle)
                    (brk:paddle-h paddle)
                    :surface surface
                    :color sdl:*white*)))

(defun draw-brick (brick surface)
  (let ((pos (brk:get-position brick)))
    (sdl:draw-box-* (brk.2d:position-x pos)
                    (brk.2d:position-y pos)
                    (brk:brick-w brick)
                    (brk:brick-h brick)
                    :surface surface
                    :color sdl:*white*)))

(defun draw-world (world surface)
  (draw-ball (brk.2d:world-ball world) surface)
  (draw-paddle (brk.2d:world-paddle world) surface)
  (dolist (brick (brk.2d:world-bricks world))
    (draw-brick brick surface)))


(defun paddle-direction ()
  (cond ((sdl:key-down-p :sdl-key-left)
         :left)
        ((sdl:key-down-p :sdl-key-right)
         :right)))

(defclass playing-scene (brk.play.scenes.playing-2d:scene) ())

(defmethod initialize-instance :after ((scene playing-scene) &key)
  (with-accessors ((world brk.play.scenes.playing-2d:scene-world)
                   (states brk.play.scenes.playing-2d:scene-states)) scene
    (setf world (brk.play.scenes.playing-2d:create-world scene))
    (setf states (list (brk.play.scenes.playing-2d:create-initial-state)))))

(defun non-playing-world (playing-scene)
  (let ((world (brk.play.scenes.playing-2d:scene-world playing-scene)))
    (let ((paddle (brk.2d:world-paddle world)))
      (make-instance 'world
                     :width (brk.2d:world-width world)
                     :height (brk.2d:world-height world)
                     :paddle (change-class paddle 'paddle
                              :position (brk:get-position paddle))
                     :ball nil))))

(defmethod handle-idle ((scene playing-scene))
  (brk.play.scenes.playing-2d:update-scene
   scene
   :paddle-direction (paddle-direction))
  (let ((world (brk.play.scenes.playing-2d:scene-world scene)))
    (clear-display)
    (draw-world world sdl:*default-display*)
    (sdl:update-display)
    (cond ((null (brk.2d:world-bricks world))
           (make-instance 'finished-scene
                          :world (non-playing-world scene)))
          ((brk.2d:ball-out-of-world-p world)
           (let ((states (brk.play.scenes.playing-2d:scene-states scene)))
             (make-instance 'replaying-scene :states (reverse states))))
          (t
           scene))))


(defclass paddle (brk:paddle)
  ((position
    :initarg :position
    :accessor paddle-position)))

(defmethod brk:get-position ((paddle paddle))
  (paddle-position paddle))

(defmethod brk:set-position ((paddle paddle) pos)
  (setf (paddle-position paddle) pos))

(defclass world (brk.2d:world)
  ((bricks
    :initarg :bricks
    :initform nil
    :reader brk.2d:world-bricks)))

;;;

(defun move-paddle (world &key paddle-direction)
  (case paddle-direction
    (:left
     (brk.2d:move-paddle-left world))
    (:right
     (brk.2d:move-paddle-right world))))

(defclass game-over-scene ()
  ((world
    :initarg :world
    :reader game-over-scene-world)))

(defmethod handle-idle ((scene game-over-scene))
  (with-accessors ((world game-over-scene-world)) scene
    (move-paddle world :paddle-direction (paddle-direction))
    (clear-display)
    (draw-paddle (brk.2d:world-paddle world) sdl:*default-display*)
    (sdl:draw-string-solid-* "Game Over!" 10 10)
    (sdl:draw-string-solid-* "Press SPACE key to replay..." 10 20)
    (sdl:update-display))
  (if (sdl:key-down-p :sdl-key-space)
      (make-instance 'playing-scene)
      scene))

;;;

(defclass finished-scene ()
  ((world
    :initarg :world
    :reader finished-scene-world)))

;;;

(defmethod handle-idle ((scene finished-scene))
  (with-accessors ((world finished-scene-world)) scene
    (move-paddle world :paddle-direction (paddle-direction))
    (clear-display)
    (draw-paddle (brk.2d:world-paddle world) sdl:*default-display*)
    (sdl:draw-string-solid-* "Congratulations!" 10 10)
    (sdl:update-display))
  scene)

;;;

(defclass replaying-scene (brk.play.scenes.playing-2d:scene) ())

(defmethod initialize-instance :after ((scene replaying-scene) &key)
  (with-accessors ((world brk.play.scenes.playing-2d:scene-world)) scene
    (setf world (brk.play.scenes.playing-2d:create-world scene))))

(defmethod handle-idle ((scene replaying-scene))
  (let ((world (brk.play.scenes.playing-2d:scene-world scene)))
    (clear-display)
    (sdl:draw-string-solid-* "Replaying" 5 5)
    (draw-world world sdl:*default-display*)
    (sdl:update-display))
  (cond ((and (sdl:key-down-p :sdl-key-right)
              (null (cdr (brk.play.scenes.playing-2d:scene-states scene))))
         (make-instance 'game-over-scene
                        :world (non-playing-world scene)))
        ((sdl:key-down-p :sdl-key-right)
         (setf (brk.play.scenes.playing-2d:scene-states scene)
               (cdr (brk.play.scenes.playing-2d:scene-states scene)))
         scene)
        (t
         scene)))

