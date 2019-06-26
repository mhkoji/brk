(defpackage :brk.sdl.scenes
  (:use :cl)
  (:export :handle-idle
           :title-scene
           :playing-scene))
(in-package :brk.sdl.scenes)

(defgeneric handle-idle (scene))

(defun clear-display ()
  (sdl:clear-display sdl:*black*))

;;;

(defclass title-scene ()
  ((selected-option
    :initform :play
    :accessor title-scene-selected-option)
   (available-replays
    :initform :available-replays
    :reader title-scene-available-replays)))

(defmethod handle-idle ((scene title-scene))
  (with-accessors ((selected-option title-scene-selected-option)) scene
    (clear-display)
    (sdl:draw-string-solid-* "Play" 20 20)
    (sdl:draw-string-solid-* "View replays" 20 40)
    (let ((> ">"))
      (ecase selected-option
        (:play
         (sdl:draw-string-solid-* > 10 20))
        (:view
         (sdl:draw-string-solid-* > 10 40))))
    (sdl:update-display)
    (cond ((and (sdl:key-down-p :sdl-key-return)
                (eql selected-option :play))
           (make-instance 'playing-scene))
          (t
           (when (eql selected-option :play)
             (when (sdl:key-down-p :sdl-key-down)
               (setf selected-option :view)))
           (when (eql selected-option :view)
             (when (sdl:key-down-p :sdl-key-up)
               (setf selected-option :play)))
           scene))))

;;;

(defun draw-ball (ball surface)
  (let ((pos (brk.2d:get-position ball)))
    (sdl:draw-filled-circle-* (ceiling (brk.2d:position-x pos))
                              (ceiling (brk.2d:position-y pos))
                              (brk.2d:ball-r ball)
                              :surface surface
                              :color sdl:*white*)))

(defun draw-paddle (paddle surface)
  (let ((pos (brk.2d:get-position paddle)))
    (sdl:draw-box-* (brk.2d:position-x pos)
                    (brk.2d:position-y pos)
                    (brk.2d:paddle-w paddle)
                    (brk.2d:paddle-h paddle)
                    :surface surface
                    :color sdl:*white*)))

(defun draw-brick (brick surface)
  (let ((pos (brk.2d:get-position brick)))
    (sdl:draw-box-* (brk.2d:position-x pos)
                    (brk.2d:position-y pos)
                    (brk.2d:brick-w brick)
                    (brk.2d:brick-h brick)
                    :surface surface
                    :color sdl:*white*)))

(defun draw-world (world surface)
  (draw-ball (brk.2d:world-ball world) surface)
  (draw-paddle (brk.2d:world-paddle world) surface)
  (dolist (brick (brk.2d:world-bricks world))
    (draw-brick brick surface)))

(defun move-paddle (world)
  (cond ((sdl:key-down-p :sdl-key-left)
         (brk.2d:move-paddle-left world))
        ((sdl:key-down-p :sdl-key-right)
         (brk.2d:move-paddle-right world))))

(defclass playing-scene (brk.2d.play:recording-scene) ())

(defun get-non-playing-world! (playing-scene)
  (let ((world (brk.2d.play:scene-world playing-scene)))
    (let ((ball (brk.2d:world-ball world))
          (paddle (brk.2d:world-paddle world)))
      (change-class world 'world
                    :ball (change-class ball 'ball
                           :position (brk.2d:get-position ball))
                    :paddle (change-class paddle 'paddle
                             :position (brk.2d:get-position paddle))))))

(defmethod handle-idle ((scene playing-scene))
  (brk.2d.play:call-with-update-world scene
    (lambda (world)
      (move-paddle world)
      (brk.2d:move-ball world)))
  (let ((world (brk.2d.play:scene-world scene)))
    (clear-display)
    (draw-world world sdl:*default-display*)
    (sdl:update-display)
    (cond ((null (brk.2d:world-bricks world))
           (make-instance 'finished-scene
                          :world (get-non-playing-world! scene)))
          ((brk.2d:ball-out-of-world-p world)
           (let ((states (brk.2d.play:recording-scene-states scene)))
             (make-instance 'replaying-scene :states (reverse states))))
          (t
           scene))))

;;;

(defclass paddle (brk.2d:paddle)
  ((position
    :initarg :position
    :accessor paddle-position)))

(defmethod brk.2d:get-position ((paddle paddle))
  (paddle-position paddle))

(defmethod brk.2d:set-position ((paddle paddle) pos)
  (setf (paddle-position paddle) pos))

(defclass ball (brk.2d:ball)
  ((position
    :initarg :position
    :accessor ball-position)))

(defmethod brk.2d:get-position ((ball ball))
  (ball-position ball))

(defclass world (brk.2d:world)
  ((bricks
    :initarg :bricks
    :initform nil
    :reader brk.2d:world-bricks)))

(defclass game-over-scene ()
  ((world
    :initarg :world
    :reader game-over-scene-world)))

(defmethod handle-idle ((scene game-over-scene))
  (with-accessors ((world game-over-scene-world)) scene
    (move-paddle world)
    (clear-display)
    (draw-paddle (brk.2d:world-paddle world) sdl:*default-display*)
    (sdl:draw-string-solid-* "Game Over!" 10 10)
    (sdl:draw-string-solid-* "Press SPACE key to play again..." 10 20)
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
    (move-paddle world)
    (clear-display)
    (draw-paddle (brk.2d:world-paddle world) sdl:*default-display*)
    (sdl:draw-string-solid-* "Congratulations!" 10 10)
    (sdl:update-display))
  scene)

;;;

(defclass replaying-scene (brk.2d.play:replaying-scene) ())

(defmethod handle-idle ((scene replaying-scene))
  (let ((world (brk.2d.play:scene-world scene)))
    (clear-display)
    (sdl:draw-string-solid-* "Replaying" 5 5)
    (draw-world world sdl:*default-display*)
    (sdl:update-display))
  (let ((diff (cond ((sdl:key-down-p :sdl-key-down) +1)
                    ((sdl:key-down-p :sdl-key-up)   -1))))
    (when diff
      (when (eql (brk.2d.play:replaying-scene-increment-index! scene diff)
                 :eof)
        (return-from handle-idle
          (make-instance 'game-over-scene
                         :world (get-non-playing-world! scene))))))
  scene)
