(defpackage :brk
  (:use :cl)
  (:export :paddle
           :paddle-w
           :paddle-h
           :ball
           :ball-r
           :brick
           :brick-w
           :brick-h))
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

