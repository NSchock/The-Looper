;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname loopersndALT) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")))))
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)
(require rsound/piano-tones)

; C major scale notes on 4th octave
(define c4 (rs-scale 1 (piano-tone 60)))
(define d4 (rs-scale 1 (piano-tone 62)))
(define e4 (rs-scale 1 (piano-tone 64)))
(define f4 (rs-scale 1 (piano-tone 65)))
(define g4 (rs-scale 1 (piano-tone 67)))
(define a4 (rs-scale 1 (piano-tone 69)))
(define b4 (rs-scale 1 (piano-tone 71)))
(define c5 (rs-scale 1 (piano-tone 72)))


;a loop is (make-loop sound list-of-posns key)
;list-of-posns is all positions in loop where sound occurs
(define-struct loop (snd posns key))

(define (s x)
  (* 44100 x))

(define LOOP-TIME (s 5))
(define LEAD-FRAMES (s 1/28))

; world is (make-world list-of-loops pstream frames)
(define-struct world (loops))


;world doesn't really matter for this test
(define (draw-world w)
  (empty-scene 400 400))

;world key->world
(define (key-handler w ke)
  (make-world 
                    (construct-loop-list (world-loops w) (pstream-current-frame PST) ke)
              ;(play-all-snds (world-loops w) ke)
              #;(world-frames w) 
              ))

;pstream ke loop -> pstream
(define (playsnd ke loopA)
  (cond [(key=? ke (loop-key loopA)) (pstream-queue PST (loop-snd loopA) (pstream-current-frame PST))]
        [else PST]))

;list-of-loops pstream key -> pstream
(define (play-all-snds lol ke)
  (cond [(empty? lol) PST]
        [else (both (playsnd ke (first lol))
                    (play-all-snds (rest lol) ke))]))

;list-of-positions position->list-of-positions
;returns a list of positions with one more position added
(define (add-posn lop posn)
  (cond [(empty? lop) (cons posn empty)]
        [else
         (cons posn (cons (first lop)
                          (rest lop)))]))

;list-of-loops posn key -> list-of-loops
;constructs a list of loops with an added position for the corresponding sound to the keypress
(define (construct-loop-list lol posn ke)
  (cond [(empty? lol) empty]
        [else
         (if (maybe-add-posn ke (first lol))
             (cons (make-loop (loop-snd (first lol))
                          (add-posn (loop-posns (first lol)) posn)
                          (loop-key (first lol)))
               (construct-loop-list (rest lol) posn ke))
             (cons (make-loop (loop-snd (first lol))
                              (loop-posns (first lol))
                              (loop-key (first lol))) 
              (construct-loop-list (rest lol) posn ke)))
         ]))

(define (maybe-add-posn ke loopA)
  (if (key=? ke (loop-key loopA)) true false))

(check-expect (add-posn empty 12) (cons 12 empty))
(check-expect (add-posn (cons 12 empty) 20) (cons 20 (cons 12 empty)))
(check-expect (add-posn (cons 20 (cons 12 empty)) 70) (cons 70 (cons 20 (cons 12 empty))))

#;(check-expect (construct-loop-list (cons (make-loop kick (cons 70 empty) "a") (cons (make-loop c-hi-hat-1 (cons 420 empty) "b") empty)) 42 "b")
              (cons (make-loop kick (cons 70 empty) "a") (cons (make-loop c-hi-hat-1 (cons 420 (cons 42 empty)) "b") empty)))


;----------------------
;LOOPING
;----------------------

;if frames == any loop posn, play at current time
;world->world
(define (tock w)
  (make-world (queue-all-sounds (world-loops w))
             
              #;(modulo (add1 (world-frames w)) (* 28 LOOP-TIME))))


;list-of-loops pstream-> list-of-loops
;calls queue-sounds for every sound to see if the sound should be played
(define (queue-all-sounds lol)
  (cond [(empty? lol) empty]
        [else (cons (make-loop 
                               (loop-snd (first lol))
                               (queue-sounds (loop-posns (first lol)) (loop-snd (first lol)))
                               (loop-key (first lol)))
                    (queue-all-sounds (rest lol)))]))


; list-of-posns snd pstream -> list-of-posns
;calls maybe-play for all sounds to check if the sound should be played
(define (queue-sounds posns snd)
 (cond [(empty? posns) empty]
        [else                      
         (cons 
          (maybe-play (first posns) snd (pstream-current-frame PST))
          (queue-sounds (rest posns) snd))                               
         ]))

;position pstream sound -> posn(queue-sounds (loop-posns (first lol)) (loop-snd (first lol)) ps)
;if position is less than lead-frames away from current pstream frame,queue up sound at given position
;queues one sound at a time based on how close it is to the current frame
(define (maybe-play posn snd cur-frames)
  (cond [(< (- posn LEAD-FRAMES)  cur-frames)
              (both (pstream-queue PST snd posn)
                    (+ posn LOOP-TIME)
                    )
         ]
        [else posn]))

;(check-expect (maybe-play 20000 kick 19950) 64100)

(define (both a b)
  b)

(define PST (make-pstream))

(big-bang (make-world 
           (cons (make-loop c4 empty "z")
                            (cons (make-loop d4 empty "x") 
                            (cons (make-loop e4 empty "c")
                            (cons (make-loop f4 empty "v")
                            (cons (make-loop g4 empty "b")
                            (cons (make-loop a4 empty "n") 
                            (cons (make-loop b4 empty "m")
                            (cons (make-loop c5 empty ",")  
                                  
                            (cons (make-loop c-hi-hat-1 empty "l") 
                            (cons (make-loop kick empty "k")
                            (cons (make-loop o-hi-hat empty "i") 
                            (cons (make-loop snare empty "p")
                            (cons (make-loop bassdrum empty "o") 
                            (cons (make-loop ding empty "d") empty)))))))))))))) 
           )
          [to-draw draw-world]
          [on-key key-handler]
          [on-tick tock])

;converts ticks to frames
#;(define (tick-frames posn)
  (cond [(= posn 0) posn]
        [else (round (* 44100 (/ 28 posn)))]))

;list->number
;returns min posn of list
#;(define (min-posn l)
  (cond [(empty? l) 0]
        [else
         (min-of-posns (first l)
                       (min-posn (rest l)))]))

#;(define (min-of-posns p1 p2)
  (if (< p1 p2) p1 p2))
