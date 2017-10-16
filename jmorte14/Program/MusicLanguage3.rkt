#lang racket

(require "music-base.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Music elements
;; Structs
(struct music-element (type elements properties))
(struct absolute-note (start pitch duration instrument))
(struct note-properties (type pitch duration instrument))
;; absolute-note
(define (an-absolute-note start pitch duration instrument)
  (absolute-note start pitch duration instrument))
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
    (if (or (< p 0) (> p 127)) (error "Unsupported pitch") p)))

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
;;;; Transform music element to list of absolute timed notes
(define (sort-by-start absolute-note-list)
  (sort absolute-note-list < #:key absolute-note-start))

(define (sort-by-end-of-note absolute-note-list)
  (sort absolute-note-list < #:key (lambda (e) (+ (absolute-note-start e) (absolute-note-duration e)))))

(define (get-sub-list elements current-time is-parallel)
  (sort-by-end-of-note (cond ((empty? elements) '())
        ((pair? elements) (let ((first-list (get-absolute-list-from (first elements) current-time)))
                            (if is-parallel (if (list? first-list) (append  first-list (get-sub-list (rest elements) current-time is-parallel))
                                                                   (get-sub-list (rest elements) current-time is-parallel))
                                            (if (list? first-list) (append  first-list (get-sub-list (rest elements) (+ (absolute-note-start (last first-list)) (absolute-note-duration (last first-list))) is-parallel))
                                                                   (get-sub-list (rest elements) first-list is-parallel))))))))

(define (get-absolute-list-from element current-time)
  (cond ((eq? (type-of element) 'parallel) (get-sub-list (music-element-elements element) current-time #t))
        ((eq? (type-of element) 'sequentiel) (get-sub-list (music-element-elements element) current-time #f))
        ((eq? (type-of element) 'note) (list (absolute-note current-time (pitch-of element) (duration-of element) (instrument-of element))))
        ((eq? (type-of element) 'pause) (+ current-time (duration-of element)))))

(define (get-absolute-timed-notes-of element)
  (let ((output (get-absolute-list-from element 0)))
  (if (list? output) (sort-by-start output) '()))) 
;; Transform to list of note-abs-time-with-duration
(define (ready-for-midi abs-list)
  (cond ((empty? abs-list) '())
        ((pair? abs-list) (let ((s (absolute-note-start (first abs-list)))
                                (i (absolute-note-instrument (first abs-list)))
                                (p (absolute-note-pitch (first abs-list)))
                                (v 80)
                                (d (absolute-note-duration (first abs-list)))
                                (r (rest abs-list)))
                            (append (list (note-abs-time-with-duration s i p v d)) (ready-for-midi r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; checking functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get duration of a music element
(define (get-music-element-duration element)
  (let ((last-note (last (sort-by-end-of-note (get-absolute-timed-notes-of element)))))
    (+ (absolute-note-start last-note) (absolute-note-duration last-note))))
;; Get degree of polyphony
(define (degree-of-polyphony element)
   (apply max (make-polyphony-list (make-start-end-list (get-absolute-timed-notes-of element)) 0)))

(define (sort-start-end-list start-end-list)
  (sort start-end-list < #:key (lambda (e) (first e))))

(define (make-start-end-list abs-list)
  (sort-start-end-list (cond ((empty? abs-list) '())
                             ((pair? abs-list) (let ((s (absolute-note-start (first abs-list)))
                                                     (d (absolute-note-duration (first abs-list))))
                                                 (append (list (list s 'start) (list (+ s d) 'end)) (make-start-end-list (rest abs-list))))))))

(define (make-polyphony-list start-end-list current-polyphony)
  (cond ((empty? start-end-list) '())
        ((pair? start-end-list) (let ((s-e (last (first start-end-list))))
                                      (let ((new-polyphony (if (eq? s-e 'start) (+ current-polyphony 1) (- current-polyphony 1))))
                                  (append (list new-polyphony) (make-polyphony-list (rest start-end-list) new-polyphony)))))))
;; Is monophonic?
(define (monophonic? music-element)
  (if (eq? (degree-of-polyphony music-element) 1) #t #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test cases ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define instrument-1 'guitar)
(define instrument-2 'piano)
(define instrument-3 'flute)
(define mester-jacob (sequentiel (list (note 'F 5 1/2 instrument-2)
                                       (note 'G 5 1/2 instrument-2)
                                       (note 'A 5 1/2 instrument-2)
                                       (note 'F 5 1/2 instrument-2)
                                       (note 'F 5 1/2 instrument-2)
                                       (note 'G 5 1/2 instrument-2)
                                       (note 'A 5 1/2 instrument-2)
                                       (note 'F 5 1/2 instrument-2)
                                       (note 'A 5 1/2 instrument-2)
                                       (note 'A# 5 1/2 instrument-2)
                                       (note 'C 6 1/1 instrument-2)
                                       (note 'A 5 1/2 instrument-2)
                                       (note 'A# 5 1/2 instrument-2)
                                       (note 'C 6 1/1 instrument-2)
                                       (note 'C 6 1/4 instrument-2)
                                       (note 'D 6 1/4 instrument-2)
                                       (note 'C 6 1/4 instrument-2)
                                       (note 'A# 5 1/4 instrument-2)
                                       (note 'A 5 1/2 instrument-2)
                                       (note 'F 5 1/2 instrument-2)
                                       (note 'C 6 1/4 instrument-2)
                                       (note 'D 6 1/4 instrument-2)
                                       (note 'C 6 1/4 instrument-2)
                                       (note 'A# 5 1/4 instrument-2)
                                       (note 'A 5 1/2 instrument-2)
                                       (note 'F 5 1/2 instrument-2)
                                       (note 'F 5 1/2 instrument-2)
                                       (note 'C 5 1/2 instrument-2)
                                       (note 'F 5 1/1 instrument-2)
                                       (note 'F 5 1/2 instrument-2)
                                       (note 'C 5 1/2 instrument-2)
                                       (note 'F 5 1/1 instrument-2))))
 
(define mester-jacob-canon (parallel (list mester-jacob
                                           (sequentiel (list (pause 4)
                                                             (change-instrument (transpose mester-jacob 12) instrument-3)))
                                           (sequentiel (list (pause 8)
                                                             (change-instrument (transpose mester-jacob -12) instrument-1))))))

(define part-1 (list (note 'C 6 3/8 instrument-1)
                     (note 'A# 5 1/8 instrument-1)
                     (note 'A 5 1/4 instrument-1)
                     (note 'G 5 1/8 instrument-1)
                     (note 'F 5 1/8 instrument-1)
                     (note 'D 5 1/4 instrument-1)
                     (note 'F 5 1/4 instrument-1)
                     (note 'D 5 1/2 instrument-1)))
(define part-2 (list (note 'A 5 3/8 instrument-2)
                     (note 'A# 5 1/8 instrument-2)
                     (note 'C 6 1/4 instrument-2)
                     (note 'A# 5 1/8 instrument-2)
                     (note 'A 5 1/8 instrument-2)
                     (note 'G 5 1/4 instrument-2)
                     (note 'F 5 1/4 instrument-2)
                     (note 'G 5 1/2 instrument-2)))
(define part-3 (list (note 'F 5 1/4 instrument-3)
                     (note 'F 5 1/2 instrument-3)
                     (note 'A# 4 1/4 instrument-3)
                     (note 'C 5 1/4 instrument-3)
                     (note 'D 5 1/4 instrument-3)
                     (note 'C 5 1/2 instrument-3)))
(define (pause-) (pause 2))
(define part-list (list part-1 part-2 part-3))
(define (make-canon parts iterations)
  (if (> iterations 0) (append parts (make-canon parts (- iterations 1))) '()))  
(define (combine-parts parts)
  (cond ((empty? parts) '())
        ((pair? parts) (append (first parts) (combine-parts (rest parts))))))
(define singer-1 (combine-parts (make-canon part-list 2)))
(define singer-2 (append (list (pause-)) (combine-parts (make-canon part-list 2))))
(define singer-3 (append (list (pause-)) (append (list (pause-)) (combine-parts (make-canon part-list 2)))))
(define viva-la-musica (parallel (list (change-instrument (sequentiel singer-1) instrument-1)
                                       (change-instrument (sequentiel singer-2) instrument-2)
                                       (change-instrument (sequentiel singer-3) instrument-3))))

(define (continue-song iterations)
  (if (eq? iterations 0) '() (append part-1 part-2 part-3 (continue-song (- iterations 1)))))

(define (viva-la-musica-setup-2 iterations)
  (sequentiel (append part-1 (list (parallel (list (sequentiel (append part-2 part-3 (continue-song iterations)))
                                                                                             (sequentiel (append part-1 (list (parallel (list (sequentiel (append part-2 part-3 (continue-song iterations)))
                                                                                                                                              (sequentiel (append part-1 part-2 part-3 (continue-song iterations))))))))))))))

(define viva-not-canon (sequentiel singer-1))
(define seq-with-para-para (sequentiel (list (parallel (list (note 'C 4 9 'telephone)
                                                             (note 'C 4 1 'telephone)))
                                             (parallel (list (note 'C 4 1 'organ)
                                                             (note 'C 4 1 'organ))))))
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

;(pitch-of n)
;(pitch-of (transpose n 20))
;(pitch-of (first (rest (rest (music-element-elements m)))))
;(pitch-of (first (rest (rest (music-element-elements (transpose m 37))))))

;(let ((s (absolute-note-start (first (get-absolute-timed-notes-of n))))
;      (i (absolute-note-instrument (first (get-absolute-timed-notes-of n))))
;      (p (absolute-note-pitch (first (get-absolute-timed-notes-of n))))
;      (v 80)
;      (d (absolute-note-duration (first (get-absolute-timed-notes-of n)))))
;  (note-abs-time-with-duration s i p v d)
;  (transform-to-midi-file-and-write-to-file! (list (note-abs-time-with-duration s i p v d)) "The-best")
;  )

;(define i- (ready-for-midi (get-absolute-timed-notes-of (multiply-duration (viva-la-musica-setup-2 1) 3))))
;(define i- (ready-for-midi (get-absolute-timed-notes-of (multiply-duration viva-la-musica 3))))
;(define i- (ready-for-midi (get-absolute-timed-notes-of seq-with-para-para)))
(define i- (ready-for-midi (get-absolute-timed-notes-of (multiply-duration mester-jacob-canon 1.5))))
(define t (get-absolute-timed-notes-of seq-with-para-para))
(absolute-note-start (first t))
(absolute-note-start (second t))
(absolute-note-start (third t))
(absolute-note-start (fourth t))
(absolute-note-duration (first t))
(absolute-note-duration (second t))
(absolute-note-duration (third t))
(absolute-note-duration (fourth t))
(transform-to-midi-file-and-write-to-file! i- "that")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Input format
;;  | | parallel
;;  [ ] sequentiel
;;  , , note
;;  . . pause
;; e.g.
;; | [ , 'C# 8 2 'piano , . 2 . , 'C 2 2 'violin , ] [ , 'C 2 2 'violin , . 2 . , 'C# 8 2 'piano , ] |
