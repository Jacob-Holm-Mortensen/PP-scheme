;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Made by ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jacob Holm Mortensen
;; jmorte14@student.aau.dk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module music-language racket
  (require "music-base.rkt")
                                                                                ;;;; Provided in this module
  (provide note pause parallel sequentiel                                       ;; Constructors for the four music elements
           type-of elements-of properties-of pitch-of duration-of instrument-of ;; Accessors for music elements and note/pause properties (duration-of can be used on all music elements)
           get-instrument-from                                                  ;; Converts integer to instrument (e.g. 'piano)
           transpose multiply-duration change-instrument                        ;; Changes the a parameter of all notes/pauses in a music element
           get-absolute-timed-notes-of                                          ;; Converts a music element to a list of absolute-note (a struct)
           transform-to-list-of-absolute-time-with-duration                     ;; Converts a list of absolute-note to a list of note-abs-time-with-duration
           degree-of-polyphony                                                  ;; Returns the degree of polyphony for a music element
           monophonic?                                                          ;; Returns if an element is monophonic
           make-canon                                                           ;; Returns a canon only using parallel, sequentiel and note
           make-canon-using-pause                                               ;; Returns a canon using all music elements
           make-non-canon                                                       ;; Returns non parallel song
                                                                                ;;;; Provided by music-base
           transform-to-midi-file-and-write-to-file!)                           ;; Make a midi file
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;; Structs
  (struct music-element (type elements properties))
  (struct absolute-note (start pitch duration instrument))
  (struct note-properties (type pitch duration instrument))
  
  ;;;; Absolute note
  (define (an-absolute-note start pitch duration instrument)
    (absolute-note start pitch duration instrument))
  
  ;;;; Music elements
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Accessors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;; Music element accessors
  ;; Get elements of music element
  (define (elements-of element)
    (if (music-element? element) (music-element-elements element)
                                 (error "Not a music element")))
  
  ;; Get properties of music element
  (define (properties-of element)
    (if (music-element? element) (music-element-properties element)
                                 (error "Not a music element")))
  
  ;;;; Note or pause property accessors
  ;; Get pitch of note
  (define (pitch-of note)
    (if (note? note) (note-properties-pitch (properties-of note))
                     (error "Not a note")))
  
  ;; Get duration of music element
  (define (duration-of element)
    (cond ((or (note? element)
               (pause? element)) (note-properties-duration (properties-of element)))
          ((or (parallel? element)
               (sequentiel? element)) (get-music-element-duration element))
          (else (error "Not a music element"))))
  
  ;; Get instrument of note
  (define (instrument-of note)
    (if (note? note) (note-properties-instrument (properties-of note))
                     (error "Not a note")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Type check ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;; Get type of music element
  (define (type-of element)
    (cond ((note-properties? element) 'note-properties)
          ((music-element? element) (music-element-type element))
          (else (error "Not a music element or note property list"))))

  ;;;; Check for a specific music element type
  ;; Check if element is of type note
  (define (note? element)
    (if (eq? (type-of element) 'note) #t #f))

  ;; Check if element is of type pause
  (define (pause? element)
    (if (eq? (type-of element) 'pause) #t #f))

  ;; Check if element is of type parallel
  (define (parallel? element)
    (if (eq? (type-of element) 'parallel) #t #f))

  ;; Check if element is of type sequentiel
  (define (sequentiel? element)
    (if (eq? (type-of element) 'sequentiel) #t #f))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Property handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
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
  
  ;; Convert a note-name to an integer
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
  
  ;; Convert pitch to note-name
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
  
  ;; Convert pitch to octave
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
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Sorting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Sort list of absolute-note by their start time
  (define (sort-by-start absolute-note-list)
    (sort absolute-note-list < #:key absolute-note-start))

  ;; Sort list of absolute-note by their end time
  (define (sort-by-end-of-note absolute-note-list)
    (sort absolute-note-list < #:key (lambda (e) (+ (absolute-note-start e) (absolute-note-duration e)))))

  ;; Sort a list of (time is-start/end) by the time
  (define (sort-start-end-list start-end-list)
    (sort start-end-list < #:key (lambda (e) (first e))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Change music element ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Go through elements of a parallel or sequentiel music element, returning a modified version
  (define (make-new elements funtion property-type parameter)
    (cond ((empty? elements) '())
          ((pair?  elements) (cons (change (first elements) funtion property-type parameter) (make-new (rest elements) funtion property-type parameter)))
          (else (error "Not a list in music element"))))
  
  ;; Creates new music elements from modified versions
  (define (change element funtion property-type parameter)
    (cond ((parallel? element) (parallel (make-new (music-element-elements element) funtion property-type parameter)))
          ((sequentiel? element) (sequentiel (make-new (music-element-elements element) funtion property-type parameter)))
          ((or (note? element)
               (pause? element)) (funtion element property-type parameter))))
  
  ;; Modifies a note or pause
  (define (change-element-property element property-type change)
    (cond ((note? element)  (note  (get-note-name (+ (pitch-of element) (if (eq? property-type 'pitch) change 0)))
                                   (get-octave (+ (pitch-of element) (if (eq? property-type 'pitch) change 0)))
                                   (get-seconds (* (duration-of element) (if (eq? property-type 'duration) change 1)))
                                   (if (eq? property-type 'instrument) change (get-instrument-from (instrument-of element)))))
          ((pause? element) (pause (get-seconds (* (duration-of element) (if (eq? property-type 'duration) change 1)))))))
  
  ;; Transpose a music element
  (define (transpose element pitch-change)
    (cond ((music-element? element) (change element change-element-property 'pitch pitch-change))
          ((list? element) (change-pitch-for-list-of-elements element pitch-change))))

  ;; Transpose list of music elements
  (define (change-pitch-for-list-of-elements elements new-pitch)
    (cond ((empty? elements) '())
          ((pair? elements) (append (list (transpose (first elements) new-pitch)) (change-pitch-for-list-of-elements (rest elements) new-pitch)))))
  
  ;; Scale the duration of a music element
  (define (multiply-duration element duration-multiplier)
    (change element change-element-property 'duration duration-multiplier))
  
  ;; Re-instrument a music element
  (define (change-instrument element new-instrument)
    (cond ((music-element? element) (change element change-element-property 'instrument new-instrument))
          ((list? element) (change-instrument-for-list-of-elements element new-instrument))))

  ;; Re-instrument a list of music elements
  (define (change-instrument-for-list-of-elements elements new-instrument)
    (cond ((empty? elements) '())
          ((pair? elements) (append (list (change-instrument (first elements) new-instrument)) (change-instrument-for-list-of-elements (rest elements) new-instrument)))))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Convertion to MIDI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Finds a sublist of absolute-note from a music element
  (define (get-sub-list elements current-time is-parallel)
    (sort-by-end-of-note (cond ((empty? elements) '())
                               ((pair? elements) (let ((first-list (get-absolute-list-from (first elements) current-time)))
                                                   (if is-parallel (if (list? first-list) (append  first-list (get-sub-list (rest elements) current-time is-parallel))
                                                                                          (get-sub-list (rest elements) current-time is-parallel))
                                                                   (if (list? first-list) (append  first-list (get-sub-list (rest elements) (+ (absolute-note-start (last first-list))
                                                                                                                                               (absolute-note-duration (last first-list))) is-parallel))
                                                                                          (get-sub-list (rest elements) first-list is-parallel))))))))
  ;; Calls the correct function based on the music element type
  (define (get-absolute-list-from element current-time)
    (cond ((parallel? element) (get-sub-list (music-element-elements element) current-time #t))
          ((sequentiel? element) (get-sub-list (music-element-elements element) current-time #f))
          ((note? element) (list (absolute-note current-time (pitch-of element) (duration-of element) (instrument-of element))))
          ((pause? element) (+ current-time (duration-of element)))))

  ;; Start function for making a list of absolute-note from a music element
  (define (get-absolute-timed-notes-of element)
    (let ((output (get-absolute-list-from element 0)))
      (if (list? output) (sort-by-start output) '())))
  
  ;; Transform a music element to a list of note-abs-time-with-duration
  (define (transform-to-list-of-absolute-time-with-duration element)
    (let ((abs-list (get-absolute-timed-notes-of element)))
      (ready-for-midi abs-list)))

  ;; Transforms a list of absolute-note to a list of note-abs-time-with-duration
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Song generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Flattens a list, used for song-parts (list of list of notes)
  (define (flatten lst)
    (cond ((null? lst) '())
          ((pair? lst) (append (flatten (first lst)) (flatten (rest lst))))
          (else (list lst))))
  
  ;; Make a list of song parts that continue for a certain amount of iterations
  (define (continue-song iterations song)
    (if (eq? iterations 0) '() (append (flatten song) (continue-song (- iterations 1) song))))
  
  ;; Make canon of specific length from song parts
  (define (make-canon song-part-list iterations pitch-change-list new-instrument-list)
    (let ((spc (length song-part-list)) (it iterations))
      (let ((p (if (eq? (length pitch-change-list) spc) pitch-change-list (error "List of pitches dont match amount of song parts")))
            (i (if (eq? (length new-instrument-list) spc) new-instrument-list (error "List of instruments dont match amount of song parts"))))
        (make-canon-setup song-part-list spc (- it 1) p i))))

  ;; Sets up the song with offsets
  (define (make-canon-setup song-part-list song-part-counter iterations pitch-change-list new-instrument-list)
    (let ((spl song-part-list) (spc song-part-counter) (it iterations) (p pitch-change-list) (i new-instrument-list))
      (if (> spc 1) (sequentiel (append (modify (first spl) p i spc) (list (parallel (list (modify (sequentiel (append (flatten (rest spl)) (continue-song it spl))) p i spc)
                                                                                           (make-canon-setup spl (- spc 1) it p i))))))
                    (modify (sequentiel (append (flatten spl) (continue-song it spl))) p i spc))))

  ;; modify a song parts instrument and pitch
  (define (modify song-part pitch-change-list new-instrument-list index)
    (let ((song-part-t (transpose song-part (list-ref pitch-change-list (- index 1)))))
      (change-instrument song-part-t (list-ref new-instrument-list (- index 1)))))
  
  ;; Make canon of specific length from song parts using pauses
  (define (make-canon-using-pause song-part-list iterations pitch-change-list new-instrument-list)
    (let ((spc (length song-part-list)) (it iterations))
      (let ((p (if (eq? (length pitch-change-list) spc) pitch-change-list (error "List of pitches dont match amount of song parts")))
            (i (if (eq? (length new-instrument-list) spc) new-instrument-list (error "List of instruments dont match amount of song parts")))
            (pause-duration (get-seconds (get-music-element-duration (sequentiel (first song-part-list))))))
      (setup-using-pause song-part-list spc it p i pause-duration))))

  ;; Setup for canon using pause
  (define (setup-using-pause song-part-list song-part-counter iterations pitch-change-list new-instrument-list pause-duration)
    (let ((spl song-part-list) (spc song-part-counter) (it iterations) (p pitch-change-list) (i new-instrument-list))
      (if (> spc 1) (parallel (list (modify (sequentiel (append (list (pause (* pause-duration (- (length spl) spc)))) (flatten spl) (continue-song (- it 1) spl))) p i spc)
                                    (setup-using-pause spl (- spc 1) it p i pause-duration)))
                    (modify (sequentiel (append (list (pause (* pause-duration (- (length spl) spc)))) (flatten spl) (continue-song (- it 1) spl))) p i spc))))
     
  ;; Make non canon song from song parts with a certain amount of iterations
  (define (make-non-canon song-part-list iterations)
    (change-instrument (sequentiel (append (flatten song-part-list) (continue-song (- iterations 1) song-part-list))) 'piano))
)
