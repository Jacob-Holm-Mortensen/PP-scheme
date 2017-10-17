;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Made by ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jacob Holm Mortensen
;; jmorte14@student.aau.dk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "music-language.rkt")
(require "song-library.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Description ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                                                                      ;;;; Provided in this module
;; mester-jacob                                                         ;; A song with 4 parts
;; viva-la-musica)                                                      ;; A song with 3 parts
;;                                                                      ;;;; Provided in this module
;; note pause parallel sequentiel                                       ;; Constructors for the four music elements
;; type-of elements-of properties-of pitch-of duration-of instrument-of ;; Accessors for music elements and note/pause properties (duration-of can be used on all music elements)
;; get-instrument-from                                                  ;; Converts integer to instrument (e.g. 'piano)
;; transpose multiply-duration change-instrument                        ;; Changes the a parameter of all notes/pauses in a music element
;; get-absolute-timed-notes-of                                          ;; Converts a music element to a list of absolute-note (a struct)
;; transform-to-list-of-absolute-time-with-duration                     ;; Converts a list of absolute-note to a list of note-abs-time-with-duration
;; degree-of-polyphony                                                  ;; Returns the degree of polyphony for a music element
;; monophonic?                                                          ;; Returns if an element is monophonic
;;                                                                      ;;;; 
;; make-canon                                                           ;; Returns a canon only using parallel, sequentiel and note
;; make-canon-using-pause                                               ;; Returns a canon using all music elements
;; make-non-canon                                                       ;; Returns non parallel song
;;                                                                      ;;;; Provided by music-base
;; transform-to-midi-file-and-write-to-file!)                           ;; Make a midi file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Making a song ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make song
(define song (make-canon viva-la-musica 1 (list 0 12 -12) (list 'organ 'flute 'guitar)))
(define song-using-pause (make-canon-using-pause mester-jacob 1 (list 0 12 -12 24) (list 'organ 'flute 'guitar 'piano)))

;; modify song
(define modified-song (multiply-duration song 3))
(define modified-song-using-pause (multiply-duration song-using-pause 1.5))

;; convert to list of note-abs-time-using-duration
(define abs-note-list (transform-to-list-of-absolute-time-with-duration modified-song))
(define abs-note-list-using-pause (transform-to-list-of-absolute-time-with-duration modified-song-using-pause))

;; convert to midi file
(transform-to-midi-file-and-write-to-file! abs-note-list "Test-song")
(transform-to-midi-file-and-write-to-file! abs-note-list-using-pause "Test-song-using-pause")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test other functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test note
(define n (note 'C 4 2 'piano))

;; Liniarize a music element
"n transformed to list of absolute time with duration"
(transform-to-list-of-absolute-time-with-duration n)

;; Degree of polyphony 
(newline)"Degree of polyphony for Viva la Musica"
(degree-of-polyphony song)

;; Is monophonic?
(newline)"Is Viva la Musica monophonic?"
(monophonic? song)
(newline)"Is n monophonic?"
(monophonic? n)

;; Duration of
(newline)"Duration of Viva la Musica"
(duration-of song)

;; Transpose
(newline)"Pitch of n"
(pitch-of n)
(newline)"Pitch of n after being transposed by 10"
(pitch-of (transpose n 10))

;; Scale
(newline)"Duration of n"
(duration-of n)
(newline)"Duration of n after duration is extended by a factor of 10"
(duration-of (multiply-duration n 10))

;; Re-instrument
(newline)"Instrument of n"
(get-instrument-from (instrument-of n))
(newline)"Instrument of n after changing it to helicopter"
(get-instrument-from (instrument-of (change-instrument n 'helicopter)))

;; Type of
(newline)"Type of the root element of Viva la Musica"
(type-of song)
(newline)"Type of the root element of Mester Jacob"
(type-of song-using-pause)

;; Elements of
(newline)"Elements of the root element of Viva la Musica"
(elements-of song)
(newline)"Elements of the root element of Mester Jacob"
(elements-of song-using-pause)

;; Properties of
(newline)"Properties of the root element of Viva la Musica"
(properties-of song)
(newline)"Properties of n"
(properties-of n)

;; Pitch property of
(newline)"Pitch of n"
(pitch-of n)

;; Duration property of
(newline)"Duration of n"
(duration-of n)

;; Instrument property of
(newline)"Instrument of n (converted from number to english)"
(get-instrument-from (instrument-of n))

;; Constructors for music elements
(newline)"Constructor for note"
(note 'C 2 1/4 'piano)
(newline)"Constructor for pause"
(pause 2)
(newline)"Constructor for parallel"
(parallel (list n))
(newline)"Constructor for sequentiel"
(sequentiel (list n))
