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
;; Convert time ticks to seconds
(define (get-seconds time-ticks)
  (/ time-ticks 960))
;; Convert note-name and octave to pitch
(define (get-pitch note-name octave)
  (let ((p (+ (get-note-number note-name) (* 12 (if (and (<= octave 10) (>= octave 0)) octave (error "Unsupported octave"))))))
    (if (> p 127) (error "Unsupported pitch") p)))
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
;; Convert pitch to note-name and octave
(define (get-note-name pitch)
  (cond ((eq? (modulo pitch 12) 0) 'C)
        ((eq? (modulo pitch 12) 1) 'C#)
        ((eq? (modulo pitch 12) 2) 'D)
        ((eq? (modulo pitch 12) 3) 'D#)
        ((eq? (modulo pitch 12) 4) 'E)
        ((eq? (modulo pitch 12) 5) 'F)
        ((eq? (modulo pitch 12) 6) 'F#)
        ((eq? (modulo pitch 12) 7) 'G)
        ((eq? (modulo pitch 12) 8) 'G#)
        ((eq? (modulo pitch 12) 9) 'A)
        ((eq? (modulo pitch 12) 10) 'A#)
        ((eq? (modulo pitch 12) 11) 'B)
        (else (error "Unsupported pitch"))))
(define (get-octave pitch)
  (exact-floor (/ pitch 12)))
;; Convert instrument type to integer
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
;; Convert integer to instrument type
(define (get-instrument-from intrument-number)
  (cond ((eq? intrument-number 1) 'piano)
        ((eq? intrument-number 2) 'organ)
        ((eq? intrument-number 3) 'guitar)
        ((eq? intrument-number 4) 'violin)
        ((eq? intrument-number 5) 'flute)
        ((eq? intrument-number 6) 'trumpet)
        ((eq? intrument-number 7) 'helicopter)
        ((eq? intrument-number 8) 'telephone)
        (else (error "Unsupported instrument"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Change music element
;; Go through elements of a parallel or sequentiel music element, returning a modified version
(define (make-new elements funtion property-type parameter)
  (cond ((empty? elements) '())
        ((pair?  elements) (cons (change (first elements) funtion property-type parameter) (make-new (rest elements) funtion property-type parameter)))
        (else (error "Not a list in music element"))))
;; Creates new music elements from modified versions
(define (change element funtion property-type parameter)
  (cond ((eq? (type-of element) 'parallel) (parallel (make-new (music-element-elements element) funtion property-type parameter)))
        ((eq? (type-of element) 'sequentiel) (sequentiel (make-new (music-element-elements element) funtion property-type parameter)))
        ((or (eq? (type-of element) 'note)
             (eq? (type-of element) 'pause)) (funtion element property-type parameter))))
;; Modifies a note or pause
(define (change-element-property element property-type change)
  (cond ((eq? (type-of element) 'note)  (note  (get-note-name (+ (pitch-of element) (if (eq? property-type 'pitch) change 0)))
                                               (get-octave (+ (pitch-of element) (if (eq? property-type 'pitch) change 0)))
                                               (get-seconds (* (duration-of element) (if (eq? property-type 'duration) change 1)))
                                               (if (eq? property-type 'instrument) change (get-instrument-from (instrument-of element)))))
        ((eq? (type-of element) 'pause) (pause (get-seconds (* (duration-of element) (if (eq? property-type 'duration) change 1)))))))
;; Transpose a music element
(define (transpose element pitch-change)
  (change element change-element-property 'pitch pitch-change))
;; Scale the duration of a music element
(define (multiply-duration element duration-multiplier)
  (change element change-element-property 'duration duration-multiplier))
;; Re-instrument a music element
(define (change-instrument element new-instrument)
  (change element change-element-property 'instrument new-instrument))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; checking functions
;; Get duration of a music element
;; Make new list with add value at an index
(define (list-with lst index value)
  (if (null? lst)
    (if (eq? index 0) (list value) lst)
    (cons
      (if (zero? index)
        (if (empty? (first lst)) value (+ (first lst) value))
        (first lst))
      (list-with (rest lst) (- index 1) value))))
;; Check list containing durations for all parallel elements
(define check-list '())
;; Adds parallel elements
(define (make-parallel-indexes elements number index)
  (cond ((empty? elements) '())
        ((pair?  elements) (set! check-list (append check-list (list number)))
                           (make-parallel-indexes (rest elements) number (+ index 1))
                           (count-duration (first elements) (+ index 1)))))
;; Goes through the elements of a music element
(define (go-through-elements elements index is-parallel)
  (cond ((empty? elements) '())
        ((pair?  elements) (if is-parallel (make-parallel-indexes elements (list-ref check-list index) index)
                                           (begin (count-duration (first elements) index)
                                                  (go-through-elements (rest elements) index is-parallel))))))
;; Calls funtions based on type of music element, adds duration to check list at specified index
(define (count-duration element index)
  (let ((type (type-of element)))
  (cond ((eq? type 'parallel) (go-through-elements (music-element-elements element) index #t))
        ((eq? type 'sequentiel) (go-through-elements (music-element-elements element) index #f))
        ((or (eq? type 'note)
             (eq? type 'pause)) (set! check-list (list-with check-list (+ index 0) (duration-of element)))))))
;; Get duration of music element
(define (get-music-element-duration element)
  (set! check-list '(0)) (count-duration element 0) check-list (apply max check-list))
;; Get degree of polyphony
(define (degree-of-polyphony music-element) 1)
;; Is monophonic?
(define (monophonic? music-element)
  (if (eq? (degree-of-polyphony music-element) 1) #t #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Transform music element to list of absolute timed notes
(define (get-absolute-timed-notes-of element)
  (if (eq? (type-of element) 'note) 'DoSomething 'DoSomethingElse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Test cases
(define i (sequentiel (list (note 'C 4 2 'piano)
                            (pause 20)
                            (note 'F# 7 3/4 'violin)
                            (parallel (list (note 'C 4 2 'piano)
                                            (pause 20)
                                            (note 'F# 7 3/4 'violin)
                                            (sequentiel (list (note 'C 4 2 'piano)
                                                        (pause 20)
                                                        (note 'F# 7 3/4 'violin))))))))
(define j (sequentiel (list (note 'C 4 2 'piano)
                            (pause 20)
                            (note 'F# 7 3/4 'violin)
                            (parallel (list (note 'C 4 2 'piano)
                                            (pause 20)
                                            (note 'F# 7 3/4 'violin)
                                            (parallel (list (note 'C 4 2 'piano)
                                                            (pause 20)
                                                            (note 'F# 7 3/4 'violin))))))))
(define k (parallel (list (note 'C 4 2 'piano)
                          (pause 20)
                          (note 'F# 7 3/4 'violin))))
(define m (sequentiel (list (note 'C 4 2 'piano)
                            (pause 20)
                            (note 'F# 7 3/4 'violin))))
(define n (note 'C 4 2 'piano))
(define p (pause 20))

(pitch-of n)
(pitch-of (transpose n 20))
(pitch-of (first (rest (rest (music-element-elements m)))))
(pitch-of (first (rest (rest (music-element-elements (transpose m 37))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Input format
;;  | | parallel
;;  [ ] sequentiel
;;  , , note
;;  . . pause
;; e.g.
;; | [ , 'C# 8 2 'piano , . 2 . , 'C 2 2 'violin , ] [ , 'C 2 2 'violin , . 2 . , 'C# 8 2 'piano , ] |
