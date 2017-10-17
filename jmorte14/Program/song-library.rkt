;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Made by ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jacob Holm Mortensen
;; jmorte14@student.aau.dk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module song-library racket
  (require "music-language.rkt")
                                                                                ;;;; Provided in this module
  (provide mester-jacob                                                         ;; A song with 4 parts
           viva-la-musica)                                                      ;; A song with 3 parts

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test songs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Mester Jacob
  (define mester-jacob-1 (list (note 'F 5 1/2 'piano)
                               (note 'G 5 1/2 'piano)
                               (note 'A 5 1/2 'piano)
                               (note 'F 5 1/2 'piano)
                               (note 'F 5 1/2 'piano)
                               (note 'G 5 1/2 'piano)
                               (note 'A 5 1/2 'piano)
                               (note 'F 5 1/2 'piano)))
                                         
  (define mester-jacob-2 (list (note 'A 5 1/2 'piano)
                               (note 'A# 5 1/2 'piano)
                               (note 'C 6 1/1 'piano)
                               (note 'A 5 1/2 'piano)
                               (note 'A# 5 1/2 'piano)
                               (note 'C 6 1/1 'piano)))
                                         
  (define mester-jacob-3 (list (note 'C 6 1/4 'piano)
                               (note 'D 6 1/4 'piano)
                               (note 'C 6 1/4 'piano)
                               (note 'A# 5 1/4 'piano)
                               (note 'A 5 1/2 'piano)
                               (note 'F 5 1/2 'piano)
                               (note 'C 6 1/4 'piano)
                               (note 'D 6 1/4 'piano)
                               (note 'C 6 1/4 'piano)
                               (note 'A# 5 1/4 'piano)
                               (note 'A 5 1/2 'piano)
                               (note 'F 5 1/2 'piano)))
                                         
  (define mester-jacob-4 (list (note 'F 5 1/2 'piano)
                               (note 'C 5 1/2 'piano)
                               (note 'F 5 1/1 'piano)
                               (note 'F 5 1/2 'piano)
                               (note 'C 5 1/2 'piano)
                               (note 'F 5 1/1 'piano)))
  
  (define mester-jacob (list mester-jacob-1 mester-jacob-2 mester-jacob-3 mester-jacob-4))
    
  ;; Viva la Musica
  (define viva-la-musica-1 (list (note 'C 6 3/8 'piano)
                                 (note 'A# 5 1/8 'piano)
                                 (note 'A 5 1/4 'piano)
                                 (note 'G 5 1/8 'piano)
                                 (note 'F 5 1/8 'piano)
                                 (note 'D 5 1/4 'piano)
                                 (note 'F 5 1/4 'piano)
                                 (note 'D 5 1/2 'piano)))
  
  (define viva-la-musica-2 (list (note 'A 5 3/8 'piano)
                                 (note 'A# 5 1/8 'piano)
                                 (note 'C 6 1/4 'piano)
                                 (note 'A# 5 1/8 'piano)
                                 (note 'A 5 1/8 'piano)
                                 (note 'G 5 1/4 'piano)
                                 (note 'F 5 1/4 'piano)
                                 (note 'G 5 1/2 'piano)))
  
  (define viva-la-musica-3 (list (note 'F 5 1/4 'piano)
                                 (note 'F 5 1/2 'piano)
                                 (note 'A# 4 1/4 'piano)
                                 (note 'C 5 1/4 'piano)
                                 (note 'D 5 1/4 'piano)
                                 (note 'C 5 1/2 'piano)))

  (define viva-la-musica (list viva-la-musica-1 viva-la-musica-2 viva-la-musica-3))
)