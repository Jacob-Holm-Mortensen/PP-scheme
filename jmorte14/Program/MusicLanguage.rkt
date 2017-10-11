#lang racket
;;; Music elements
;;; accessor functions, construction functions, and a recognizing predicate needs to be made for all music elements
;; Note
(define (note note-name octave duration instrument)
  (let ((p (get-pitch note-name octave))
        (d duration)
        (i (get-instrument instrument)))
      (define (getp) p)
      (define (getd) d)
      (define (geti) i)
      (define (setp new-n new-o) (set! p (get-pitch new-n new-o)))
      (define (setd new-d) (set! d new-d))
      (define (seti new-i) (set! i (get-instrument new-i)))
      (define (type-of) 'note)
      (define (self message)
        (cond ((eq? message 'getp) getp)
              ((eq? message 'getd) getd)
              ((eq? message 'geti) geti)
              ((eq? message 'setp) setp)
              ((eq? message 'setd) setd)
              ((eq? message 'seti) seti)
              ((eq? message 'type-of) type-of)
              (else (error "Message not understood"))))
    self))
;; Pause
(define (pause duration)
  (let ((d duration))
      (define (getd) d)
      (define (setd new-d) (set! d new-d))
      (define (type-of) 'pause)
      (define (self message)
        (cond ((eq? message 'getd) getd)
              ((eq? message 'setd) setd)
              ((eq? message 'type-of) type-of)
              (else (error "Message not understood"))))
    self))
;; Parallel element
(define (parallel-element elements)
  (let ((e elements))
      (define (gete) e)
      (define (sete new-e) (set! e new-e))
      (define (type-of) 'parallel-music-element)
      (define (self message)
        (cond ((eq? message 'gete) gete)
              ((eq? message 'sete) sete)
              ((eq? message 'type-of) type-of)
              (else (error "Message not understood"))))
    self))
;; Sequential element
(define (sequential-element element-list) element-list)

;;; Parameters
;; Second
(define (seconds amount)
  (cond ((integer? (* 2 amount)) (* amount 960))
        ((integer? (* 4 amount)) (* amount 960))
        ((integer? (* 8 amount)) (* amount 960))
        (else 'wrongInterval))) ; returns seconds in time ticks
;; Pitch
(define (get-pitch note-name octave)
  (+ (get-note-number note-name) (* octave 12)))
(define (get-note-number note-name)
  (cond ((eq? note-name 'C) 0)
        ((eq? note-name 'C#) 1)
        ((eq? note-name 'D) 2)
        ((eq? note-name 'D#) 3)
        ((eq? note-name 'E) 4)
        ((eq? note-name 'F) 5)
        ((eq? note-name 'F#) 6)
        ((eq? note-name 'G) 7)
        ((eq? note-name 'G#) 8)
        ((eq? note-name 'A) 9)
        ((eq? note-name 'A#) 10)
        ((eq? note-name 'B) 11)))
;; Instrument
(define (get-instrument type)
  (cond ((eq? type 'piano) 1)
        ((eq? type 'organ) 2)
        ((eq? type 'guitar) 3)
        ((eq? type 'violin) 4)
        ((eq? type 'flute) 5)
        ((eq? type 'trumpet) 6)
        ((eq? type 'helicopter) 7)
        ((eq? type 'telephone) 8)))

;;; Change music element
;; Transpose
(define (change-pitch note) 1) ; not implemented, returns modified music element
;; Scale
(define (change-duration note) 1) ; not implemented, returns modified music element
;; Re-instrument
(define (change-instrument note) 1) ; not implemented, returns modified music element

;; checking methods
;; Get duration
(define (get-duration music-element) 1) ; not implemented, returns duration of a music element
;; Is monophonic?
(define (is-monophonic? music-element) 1) ; not implemented, returns if a music element plays at most one note at a time
;; Get degree of polyphony
(define (degree-of-polyphony music-element) 1) ; not implemented, returns max amount of notes played simultaniously in the music element

;; Map music element with given function 
(define (map-music-element music-element) 1) ; not implemented, maps a music element with the function provided to get a list of absolutely timed note objects

; Setup
(define (method-lookup object selector)
  (cond ((procedure? object)
         (let ((result (object selector)))
           (if (procedure? result)
               result
               (error "Did not find any method")))
         )
        (else
         (error "innappropriate object in method-lookup: " object))))

(define (send message obj . par)
  (let ((method (method-lookup obj message)))
    (apply method par)))

(define (new-instance class . parameters)
  (apply class parameters))

(define note1 (new-instance note 'C# 8 (seconds 2) 'piano))
(define pause1 (new-instance pause (seconds 2)))