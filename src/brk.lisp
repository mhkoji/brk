(defpackage :brk
  (:use :cl)
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
           :set-velocity))
(in-package :brk)

(defclass paddle ()
  ((w :initarg :w
      :reader paddle-w)
   (h :initarg :h
      :reader paddle-h)))

(defclass ball ()
  ((r :initarg :r
      :reader ball-r)))

(defclass brick ()
  ((w :initarg :w
      :reader brick-w)
   (h :initarg :h
      :reader brick-h)))


(defgeneric get-position (entity))

(defgeneric set-position (entity pos))


(defgeneric get-velocity (entity))

(defgeneric set-velocity (entity v))
