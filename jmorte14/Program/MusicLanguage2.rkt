#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Music elements
;; Structs
(struct music-element (type elements properties))
(struct note-properties (type pitch duration instrument))
;; Note
(define (note note-name octave duration instrument)
  (music-element 'note '() (note-properties 'note-properties
                                            (get-pitch note-name octave)
                                            (get-time-ticks duration)
                                            (get-instrument instrument))))
;; Pause
(define (pause duration)
  (music-element 'pause '() (note-properties 'note-properties
                                            '()
                                            (get-time-ticks duration)
                                            '())))
;; Parallel music element
(define (parallel elements)
  (music-element 'parallel (check-all-elements elements) '()))
;; Sequentiel music element
(define (sequentiel elements)
  (music-element 'sequentiel (check-all-elements elements) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Helper functions
;; Get type of music element
(define (type-of element)
  (cond ((note-properties? element) 'note-properties)
        ((music-element? element) (music-element-type element))
        (else (error "Not a music element or note property list"))))
;; Get elements of music element
(define (elements-of element)
  (if (music-element? element) (music-element-elements element)
                               (error "Not a music element")))
;; Get properties of music element
(define (properties-of element)
  (if (music-element? element) (music-element-properties element)
                               (error "Not a music element")))
;; Get pitch of note
(define (pitch-of note)
  (if (eq? (type-of note) 'note) (note-properties-pitch (properties-of note))
                                 (error "Not a note")))
;; Get duration of note or pause
(define (duration-of note-or-pause)
  (if (or (eq? (type-of note-or-pause) 'note)
          (eq? (type-of note-or-pause) 'pause)) (note-properties-duration (properties-of note-or-pause))
                                                (error "Not a note or pause")))
;; Get instrument of note
(define (instrument-of note)
  (if (eq? (type-of note) 'note) (note-properties-instrument (properties-of note))
                                 (error "Not a note")))
;; Check all elements of a music element for their type, returns the elements if correct types
(define (check-all-elements elements)
  (if (cond ((empty? elements) #t)
            ((pair?  elements) (and (music-element? (first elements))
                                    (check-all-elements (rest elements))))
            (else (error "Not a list in music element"))) elements
                                                          (error "contains non-music-elements")))
;; Convert seconds to time ticks
(define (get-time-ticks amount)
  (cond ((or (integer? (* 2 amount))
             (integer? (* 4 amount))
             (integer? (* 8 amount))) (exact-floor (* 960 amount)))
        (else (error "Unsupported duration"))))
;; Convert note-name and octave to pitch
(define (get-pitch note-name octave)
  (+ (get-note-number note-name) (* 12 (if (<= octave 8) octave (error "Unsupported octave")))))
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
        ((eq? note-name 'B) 11)
        (else (error "Unsupported note"))))
;; Convert instrument type to an integer
(define (get-instrument type)
  (cond ((eq? type 'piano) 1)
        ((eq? type 'organ) 2)
        ((eq? type 'guitar) 3)
        ((eq? type 'violin) 4)
        ((eq? type 'flute) 5)
        ((eq? type 'trumpet) 6)
        ((eq? type 'helicopter) 7)
        ((eq? type 'telephone) 8)
        (else (error "Unsupported instrument"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Change music element
;; Transpose a music element
(define (change-pitch element new-note-name new-octave)
  (if (music-element? element)'() '()))
;; Scale the duration of a music element
(define (change-duration element scale)
  (if (music-element? element)'() '()))
;; Re-instrument a music element
(define (change-instrument element new-instrument)
  (if (music-element? element)'() '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; checking functions
;; Get duration of a music element

;; Is monophonic?
(define (is-monophonic? music-element) 1)
;; Get degree of polyphony
(define (degree-of-polyphony music-element) 1)
(define (map-music-element music-element) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Test cases
(define m (parallel (list (parallel (list (note 'C# 4 3/4 'violin)
                                          (note 'C 4 2 'piano)))
                          (note 'C 4 2 'piano))))
(define n (note 'C 4 2 'piano))
(define p (pause 20))