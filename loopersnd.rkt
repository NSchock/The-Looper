;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname loopersnd) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")))))
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)

;a loop is (make-loop sound list-of-posns key)
;list-of-posns is all positions in loop where sound occurs
(define-struct loop (snd posns key))

(define-struct posin (pos queued))

(define (s x)
  (* 44100 x))

(define LOOP-TIME 5)
(define LEAD-FRAMES (s 1/28))

; world is (make-world list-of-loops pstream frames)
(define-struct world (loops ps frames))


;world doesn't really matter for this test
(define (draw-world w)
  (empty-scene 400 400))

;world key->world
(define (key-handler w ke)
  (make-world (construct-loop-list (world-loops w) (make-posin (pstream-current-frame (world-ps w)) 0 #;?) ke)
              (play-all-snds (world-loops w) (world-ps w) ke)
              (world-frames w)
              ))

;pstream ke loop -> pstream
(define (playsnd ps ke loopA)
  (cond [(key=? ke (loop-key loopA)) (pstream-queue ps (loop-snd loopA) (pstream-current-frame ps))]
        [else ps]))

;list-of-loops pstream key -> pstream
(define (play-all-snds lol ps ke)
  (cond [(empty? lol) ps]
        [else (both (playsnd ps ke (first lol))
                    (play-all-snds (rest lol) ps ke))]))

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

#;(check-expect (add-posn empty 12) (cons 12 empty))
#;(check-expect (add-posn (cons 12 empty) 20) (cons 20 (cons 12 empty)))
#;(check-expect (add-posn (cons 20 (cons 12 empty)) 70) (cons 70 (cons 20 (cons 12 empty))))

#;(check-expect (construct-loop-list (cons (make-loop kick (cons 70 empty) "a") (cons (make-loop c-hi-hat-1 (cons 420 empty) "b") empty)) 42 "b")
              (cons (make-loop kick (cons 70 empty) "a") (cons (make-loop c-hi-hat-1 (cons 420 (cons 42 empty)) "b") empty)))


;----------------------
;LOOPING
;----------------------

;if frames == any loop posn, play at current time
;world->world
(define (tock w)
  (make-world (world-loops w)
              (cond [(= (modulo (world-frames w) (* 28 LOOP-TIME)) 0) (make-pstream)]
                    [else (queue-all w)])
              (modulo (add1 (world-frames w)) (* 28 LOOP-TIME)))
  
  )

;list-of-loops pstream->pstream
;queues all sounds for all loops in list-of-loops
(define (queue-all w)
  (cond [(empty? (world-loops w)) (world-ps w)]
        [else (both (queue-sounds (first (world-loops w)) (pstream-current-frame (world-ps w)))
                    (queue-all (make-world (rest (world-loops w)) (world-ps w) (world-frames w))))]))


; loopA pstream -> loopA
;queues up all sounds for one loop
;adds one to queue if ready to play NOT
;adds to counter for loop position if ready to play
(define (queue-sounds loopA cur-frame)
 (cond [(empty? (loop-posns loopA)) loopA]
        [else             
         (if (maybe-play (first (loop-posns loopA)) (loop-snd loopA) cur-frame)
                       #;(queue-sounds (make-loop (loop-snd loopA) (rest (loop-posns loopA)) (loop-key loopA))                                   
                                            
                                                #;(pstream-queue ps (loop-snd loopA) (first (loop-posns loopA)))
                                                ps)
                       ;(both 
                       (make-loop (loop-snd loopA) (change-first (loop-posns loopA)) (loop-key loopA))
                             #;(queue-sounds (make-loop (loop-snd loopA) (change-first (loop-posns loopA)) (loop-key loopA))                                   
                                            
                                                #;(pstream-queue ps (loop-snd loopA) (first (loop-posns loopA)))
                                                ps);)

                       (queue-sounds (make-loop (loop-snd loopA) (rest (loop-posns loopA)) (loop-key loopA))                                   
                                            
                                                #;(pstream-queue ps (loop-snd loopA) (first (loop-posns loopA)))
                                                cur-frame)
                       )
                       
         ]))

(check-expect (queue-sounds (make-loop kick (list (make-posin 42000 0) (make-posin 10000 0)) "a") 41500) (make-loop kick (list (make-posin 42000 1) (make-posin 10000 0)) "a"))

#;(define (play-snd loopA ps)
  (cond [(empty? (loop-posns loopA)) ps]
        [else (first loopA)
              ((rest loopA))]))

;change first in list-of-posins
;list-of-posins->list-of-posins
(define (change-first lop)
  (cond [(empty? lop) lop]
        [else (cons (make-posin (posin-pos (first lop)) (add1 (posin-queued (first lop))))
                    (rest lop))]))

(define (valid-play snd posn ps)
  (cond [(<= (posin-queued posn) 1) (pstream-queue ps snd (posin-pos posn))]
        [else ps]))

;loops through for multiple ticks and queues within those ticks
;position sound pstream -> pstream
;if position is less than lead-frames away from current pstream frame,queue up sound at given position
;queues one sound at a time based on how close it is to the current frame
(define (maybe-play p snd cur-frames)
  #;(cond [(< (- (posin-pos p) LEAD-FRAMES) cur-frames)
         (pstream-queue ps snd (posin-pos p))]
        [else ps])
  (< (- (posin-pos p) LEAD-FRAMES) cur-frames))

(define (both a b)
  b)

(big-bang (make-world (cons (make-loop kick empty "a") (cons (make-loop c-hi-hat-1 empty "b") empty)) (make-pstream) 0)
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
