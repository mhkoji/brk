(defpackage :brk.play.scenes.playing-2d
  (:use :cl)
  (:export :create-world
           :draw-world
           :draw-paddle
           :move-ball
           :move-paddle)
  (:import-from :brk.2d
                :move-ball))
(in-package :brk.play.scenes.playing-2d)

(defun draw-ball (ball surface)
  (let ((pos (brk.2d:position ball)))
    (sdl:draw-filled-circle-* (ceiling (brk.2d:position-x pos))
                              (ceiling (brk.2d:position-y pos))
                              (brk:ball-r ball)
                              :surface surface
                              :color sdl:*white*)))

(defun draw-paddle (paddle surface)
  (let ((pos (brk.2d:position paddle)))
    (sdl:draw-box-* (brk.2d:position-x pos)
                    (brk.2d:position-y pos)
                    (brk:paddle-w paddle)
                    (brk:paddle-h paddle)
                    :surface surface
                    :color sdl:*white*)))

(defun draw-brick (brick surface)
  (let ((pos (brk.2d:position brick)))
    (sdl:draw-box-* (brk.2d:position-x pos)
                    (brk.2d:position-y pos)
                    (brk:brick-w brick)
                    (brk:brick-h brick)
                    :surface surface
                    :color sdl:*white*)))


(defun create-world ()
  (brk.2d:make-world
   :width 290
   :height 480
   :ball (make-instance 'brk.2d:ball
                        :r 5
                        :velocity
                        (brk.2d:make-velocity
                         :x (* 6 (sin (/ pi 4)))
                         :y (* 6 (- (sin (/ pi 4)))))
                        :position
                        (brk.2d:make-position :x 10 :y 420))
   :paddle (make-instance 'brk.2d:paddle
                          :w 30
                          :h 5
                          :position
                          (brk.2d:make-position :x 10 :y 440))
   :walls (list :left :right :top)
   :bricks (loop for y = 20 then (+ y 20)
                 while (< y 80)
                 nconc
                (loop for x = 10 then (+ x 40)
                      while (< (+ x 30) 320)
                      collect
                     (make-instance 'brk.2d:brick
                      :w 30
                      :h 5
                      :position
                      (brk.2d:make-position :x x :y y))))))

(defun move-paddle (world)
  (cond ((sdl:key-down-p :sdl-key-left)
         (brk.2d:move-paddle-left world))
        ((sdl:key-down-p :sdl-key-right)
         (brk.2d:move-paddle-right world))))

(defun draw-world (world surface)
  (draw-ball (brk.2d:world-ball world) surface)
  (draw-paddle (brk.2d:world-paddle world) surface)
  (dolist (brick (brk.2d:world-bricks world))
    (draw-brick brick surface)))
