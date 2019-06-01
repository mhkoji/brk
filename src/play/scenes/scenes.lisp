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

;; TODO
(defclass title-scene ()
  ())

(defmethod handle-idle ((scene title-scene))
  scene)

;;;

(defclass playing-scene ()
  ((world
    :accessor playing-world)))

(defmethod initialize-instance :after ((scene playing-scene) &key)
  (with-slots (world) scene
    (setf world (brk.play.scenes.playing-2d:create-world))))

(defmethod handle-idle ((scene playing-scene))
  (with-slots (world) scene
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
  ((world :initarg :world)))

(defmethod handle-idle ((scene game-over-scene))
  (with-slots (world) scene
    (brk.play.scenes.playing-2d:move-paddle world)
    (clear-display)
    (brk.play.scenes.playing-2d:draw-paddle (brk.2d:world-paddle world)
                                            sdl:*default-display*)
    (sdl:draw-string-solid-* "Game Over!" 10 10)
    (sdl:draw-string-solid-* "Press SPACE key..." 10 20)
    (sdl:update-display))
  (if (sdl:key-down-p :sdl-key-space)
      (make-instance 'playing-scene)
      scene))

;;;

(defclass finished-scene ()
  ((world :initarg :world)))

;;;

(defmethod handle-idle ((scene finished-scene))
  (with-slots (world) scene
    (brk.play.scenes.playing-2d:move-paddle world)
    (clear-display)
    (brk.play.scenes.playing-2d:draw-paddle (brk.2d:world-paddle world)
                                            sdl:*default-display*)
    (sdl:draw-string-solid-* "Congratulations!" 10 10)
    (sdl:update-display))
  scene)
