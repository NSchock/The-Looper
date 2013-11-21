;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname loopersnd) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")))))
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)

;a loop is (make-loop sound list-of-posns key)
;list-of-posns is all positions in loop where sound occurs
(define-struct loop (snd posns key))

(define-struct play-time (play? posn))

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
  (make-world (construct-loop-list (world-loops w) (pstream-current-frame (world-ps w)) ke)
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
  #;(cond [(play-time-play? (all-time-to-play? (world-loops w) (pstream-current-frame (world-ps w))))
         (make-world (world-loops w)
                     (cond [(= (modulo (world-frames w) (* 28 LOOP-TIME)) 0) (make-pstream)]
                           [else (queue-all (world-loops w) (world-ps w))])
                     (modulo (add1 (world-frames w)) (* 28 LOOP-TIME)))]
        [else w])
  (check-plays (check-if-play (all-time-to-play? (world-loops w) (pstream-current-frame (world-ps w)))) w)
  )
;list-of-play-times->list-of-booleans
(define (check-if-play lop)
  (cond [(empty? lop) empty]
        [else (cons (play-time-play? (first lop))
                    (check-if-play (rest lop)))]))

(check-expect (check-if-play (list (make-play-time true 420) (make-play-time false 400) (make-play-time true 200))) (list true false true))

;list-of-booleans->world
(define (check-plays lob w)
  (cond [(empty? lob) w]
        [else (both (possibly-play (first lob) w)
                    (check-plays (rest lob) w))]))

(define (possibly-play bool w)
  (cond [bool (make-world (world-loops w)
                     (cond [(= (modulo (world-frames w) (* 28 LOOP-TIME)) 0) (make-pstream)]
                           [else (queue-all (world-loops w) (world-ps w))])
                     (modulo (add1 (world-frames w)) (* 28 LOOP-TIME)))]
        [else w]))

;list-of-loops pstream->pstream
;queues all sounds for all loops in list-of-loops
(define (queue-all lol ps)
  (cond [(empty? lol) ps]
        [else (both (queue-sounds (first lol) ps)
                    (queue-all (rest lol) ps))]))


; loopA pstream -> pstream
;queues up all sounds for one loop
(define (queue-sounds loopA ps)
 (cond [(empty? (loop-posns loopA)) ps]
        [else
                    (queue-sounds (make-loop (loop-snd loopA) (rest (loop-posns loopA)) (loop-key loopA)) 
                              ;(maybe-play (first (loop-posns loopA)) (loop-snd loopA) ps)
                                  (pstream-queue ps (loop-snd loopA) (first (loop-posns loopA)))
                              )
         ]))

;loop frames -> (make-play-time boolean frames)
(define (time-to-play? loop cur-time)
  (cond [(empty? (loop-posns loop)) (make-play-time false cur-time)]
        [else (cond [(< (- (first (loop-posns loop)) LEAD-FRAMES) cur-time) 
                  (make-play-time true (first (loop-posns loop)))]
                  [else (time-to-play? (make-loop (loop-snd loop) (rest (loop-posns loop)) (loop-key loop)) cur-time)])]))

;;only takes first val of each posns-list
;list-of-loops frames -> list-of-play-times
(define (all-time-to-play? lol cur-time)
  (cond [(empty? lol) empty]
        [else (cons (time-to-play? (first lol) cur-time)
                    (all-time-to-play? (rest lol) cur-time))]))

(check-expect (all-time-to-play? (list (make-loop kick (list 1000 12000 42000) "a") (make-loop ding (list 11600 22500 33200 69000) "b")) 11500) 
              (list (make-play-time false 11500) (make-play-time true 12000) (make-play-time false 11500) (make-play-time true 11600) (make-play-time false 11500) (make-play-time false 11500) (make-play-time false 11500)))


;loops through for multiple ticks and queues within those ticks
;position sound pstream -> pstream
;if position is less than lead-frames away from current pstream frame,queue up sound at given position
;queues one sound at a time based on how close it is to the current frame
(define (maybe-play posn snd ps)
  (cond [(< (- posn LEAD-FRAMES) (pstream-current-frame ps))
         (pstream-queue ps snd posn)]
        [else ps]))

(define (both a b)
  b)

;converts ticks to frames
(define (tick-frames posn)
  (cond [(= posn 0) posn]
        [else (round (* 44100 (/ 28 posn)))]))

(big-bang (make-world (cons (make-loop kick empty "a") (cons (make-loop c-hi-hat-1 empty "b") empty)) (make-pstream) 0)
          [to-draw draw-world]
          [on-key key-handler]
          [on-tick tock])

;list->number
;returns min posn of list
(define (min-posn l)
  (cond [(empty? l) 0]
        [else
         (min-of-posns (first l)
                       (min-posn (rest l)))]))

(define (min-of-posns p1 p2)
  (if (< p1 p2) p1 p2))