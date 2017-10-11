#lang racket
;;; Music elements
;;; accessor functions, construction functions, and a recognizing predicate needs to be made for all music elements
(struct music-element (type elements properties))
;; Note
(define (note note-name octave duration instrument)
  (music-element 'note '() (list (get-pitch note-name octave)
                                 (get-time-ticks duration)
                                 (get-instrument instrument))))
;; Pause
(define (pause duration)
  (music-element 'pause '() (list (get-time-ticks duration))))
;; Parallel music element
(define (parallel elements)
  (music-element 'parallel (check-all-elements elements) '()))
;; Sequentiel music element
(define (sequentiel elements)
  (music-element 'sequentiel (check-all-elements elements) '()))
;;; Helper functions
(define (check-all-elements elements)
  (if (check-list elements) elements (error elements "contains non-music-elements")))

(define (check-list elements)
  (cond ((empty? elements) #t)
        ((pair?  elements) (and (music-element? (first elements)) (check-all-elements (rest elements))))
        (else (error "Not a list in music element"))))

;; Seconds in time ticks
(define (get-time-ticks amount)
  (cond ((integer? (* 2 amount)) (exact-floor (* 960 amount)))
        ((integer? (* 4 amount)) (exact-floor (* 960 amount)))
        ((integer? (* 8 amount)) (exact-floor (* 960 amount)))
        (else (error "Unsupported duration"))))
;; Pitch
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
;; Instrument
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