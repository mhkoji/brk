(defpackage :brk.gaming
  (:use :cl)
  (:export :main))
(in-package :brk.gaming)

(defgeneric draw (object surface))

(defmethod draw ((ball brk.gaming.2d:ball) surface)
  (let ((pos (brk.gaming.2d:position ball)))
    (sdl:draw-filled-circle-*
     (ceiling (brk.gaming.2d:position-x pos))
     (ceiling (brk.gaming.2d:position-y pos))
     (brk:ball-r ball)
     :surface surface
     :color sdl:*white*)))

(defmethod draw ((paddle brk.gaming.2d:paddle) surface)
  (let ((pos (brk.gaming.2d:position paddle)))
    (sdl:draw-box-*
     (brk.gaming.2d:position-x pos)
     (brk.gaming.2d:position-y pos)
     (brk:paddle-w paddle)
     (brk:paddle-h paddle)
     :surface surface
     :color sdl:*white*)))

(defmethod draw ((brick brk.gaming.2d:brick) surface)
  (let ((pos (brk.gaming.2d:position brick)))
    (sdl:draw-box-*
     (brk.gaming.2d:position-x pos)
     (brk.gaming.2d:position-y pos)
     (brk:brick-w brick)
     (brk:brick-h brick)
     :surface surface
     :color sdl:*white*)))

(defun move-paddle (world)
  (cond ((sdl:key-down-p :sdl-key-left)
         (brk.gaming.2d:move-paddle-left world))
        ((sdl:key-down-p :sdl-key-right)
         (brk.gaming.2d:move-paddle-right world))))

;;;

(defclass state () ())

(defgeneric handle-idle (state))


(defun create-world ()
  (brk.gaming.2d:make-world
   :width 290
   :height 480
   :ball (make-instance 'brk.gaming.2d:ball
                        :r 5
                        :velocity
                        (brk.gaming.2d:make-velocity
                         :x (* 6 (sin (/ pi 4)))
                         :y (* 6 (- (sin (/ pi 4)))))
                        :position
                        (brk.gaming.2d:make-position :x 10 :y 420))
   :paddle (make-instance 'brk.gaming.2d:paddle
                          :w 30
                          :h 5
                          :position
                          (brk.gaming.2d:make-position :x 10 :y 440))
   :walls (list :left :right :top)
   :bricks (loop for y = 20 then (+ y 20)
                 while (< y 80)
                 nconc
                (loop for x = 10 then (+ x 40)
                      while (< (+ x 30) 320)
                      collect
                     (make-instance 'brk.gaming.2d:brick
                      :w 30
                      :h 5
                      :position
                      (brk.gaming.2d:make-position :x x :y y))))))

(defclass playing (state)
  ((world)))

(defmethod initialize-instance :after ((playing playing) &key)
  (with-slots (world) playing
    (setf world (create-world))))

(defmethod handle-idle ((playing playing))
  (with-slots (world) playing
    (move-paddle world)
    (brk.gaming.2d:auto-update-world world)
    (sdl:clear-display sdl:*black*)
    (draw (brk.gaming.2d:world-ball world)
          sdl:*default-display*)
    (draw (brk.gaming.2d:world-paddle world)
          sdl:*default-display*)
    (dolist (brick (brk.gaming.2d:world-bricks world))
      (draw brick sdl:*default-display*))
    (sdl:update-display)
    (cond ((null (brk.gaming.2d:world-bricks world))
           (make-instance 'finished :world world))
          ((brk.gaming.2d:ball-out-of-world-p world)
           (make-instance 'game-over :world world)))))



(defclass game-over (state)
  ((world :initarg :world)))

(defmethod handle-idle ((state game-over))
  (with-slots (world) state
    (move-paddle world)
    (sdl:clear-display sdl:*black*)
    (draw (brk.gaming.2d:world-paddle world)
          sdl:*default-display*)
    (sdl:draw-string-solid-* "Game Over!" 10 10)
    (sdl:draw-string-solid-* "Press SPACE key..." 10 20)
    (sdl:update-display)
    (when (sdl:key-down-p :sdl-key-space)
      (make-instance 'playing))))


(defclass finished (state)
  ((world :initarg :world)))

(defmethod handle-idle ((state finished))
  (with-slots (world) state
    (move-paddle world)
    (sdl:clear-display sdl:*black*)
    (draw (brk.gaming.2d:world-paddle world)
          sdl:*default-display*)
    (sdl:draw-string-solid-* "Congratulations!" 10 10)
    (sdl:update-display)))

(defun main ()
  (sdl:initialise-default-font)
  (let ((state (make-instance 'playing)))
    (sdl:with-init ()
      (with-slots (world) state
        (sdl:window (brk.gaming.2d:world-width world)
                    (brk.gaming.2d:world-height world)
                    :title-caption "BRK"))
      (setf (sdl:frame-rate) 60)
      (sdl:with-events ()
        (:quit-event () t)
        (:video-expose-event ()
          (sdl:update-display))
        (:idle ()
          (let ((new-state (handle-idle state)))
            (when new-state
              (setq state new-state))))))))
