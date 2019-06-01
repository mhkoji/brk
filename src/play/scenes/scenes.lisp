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

(defclass playing-scene ()
  ((world
    :accessor playing-scene-world)))

(defmethod initialize-instance :after ((scene playing-scene) &key)
  (with-accessors ((world playing-scene-world)) scene
    (setf world (brk.play.scenes.playing-2d:create-world))))

(defmethod handle-idle ((scene playing-scene))
  (with-accessors ((world playing-scene-world)) scene
    (brk.play.scenes.playing-2d:move-paddle world)
    (brk.play.scenes.playing-2d:move-ball world)
    (clear-display)
    (brk.play.scenes.playing-2d:draw-world world sdl:*default-display*)
    (sdl:update-display)
    (cond ((null (brk.2d:world-bricks world))
           (make-instance 'finished-scene :world world))
          ((brk.2d:ball-out-of-world-p world)
           (make-instance 'game-over-scene :world world))
          (t
           scene))))

;;;

(defclass game-over-scene ()
  ((world
    :initarg :world
    :reader game-over-scene-world)))

(defmethod handle-idle ((scene game-over-scene))
  (with-accessors ((world game-over-scene-world)) scene
    (brk.play.scenes.playing-2d:move-paddle world)
    (clear-display)
    (brk.play.scenes.playing-2d:draw-paddle (brk.2d:world-paddle world)
                                            sdl:*default-display*)
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
    (brk.play.scenes.playing-2d:move-paddle world)
    (clear-display)
    (brk.play.scenes.playing-2d:draw-paddle (brk.2d:world-paddle world)
                                            sdl:*default-display*)
    (sdl:draw-string-solid-* "Congratulations!" 10 10)
    (sdl:update-display))
  scene)
