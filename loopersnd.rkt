;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname loopersnd) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")))))
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)

;a loop is (make-loop sound list-of-posns key)
;list-of-posns is all positions in loop where sound occurs
(define-struct loop (snd posns key))

(define (s x)
  (* 44100 x))

(define LOOP-TIME 5)
(define LEAD-FRAMES (s 1/28))

;s world is (make-world list-of-loops pstream frames)
(define-struct world (loops ps frames))


;world doesn't really matter for this test
(define (draw-world w)
  (empty-scene 420 69))

;world key->world
(define (key-handler w ke)
  (make-world (construct-loop-list (world-loops w) (world-frames w) ke)
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
                          (add-posn (loop-posns (first lol)) (tick-frames posn))
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
(check-expect (add-posn (cons 20 (cons 12 empty)) 69) (cons 69 (cons 20 (cons 12 empty))))

#;(check-expect (construct-loop-list (cons (make-loop kick (cons 69 empty) "a") (cons (make-loop c-hi-hat-1 (cons 420 empty) "b") empty)) 42 "b")
              (cons (make-loop kick (cons 69 empty) "a") (cons (make-loop c-hi-hat-1 (cons 420 (cons 42 empty)) "b") empty)))

;if frames == any loop posn, play at current time
(define (tock w)
  (make-world (world-loops w)
              (cond [(= (modulo (world-frames w) (* 28 LOOP-TIME)) 0) (make-pstream)]
                    [else (queue-all (world-loops w) (world-ps w))])
              (modulo (add1 (world-frames w)) (* 28 LOOP-TIME))))

;list-of-loops pstream->pstream
(define (queue-all lol ps)
  (cond [(empty? lol) ps]
        [else (both (queue-sounds2 (first lol) ps)
                    (queue-all (rest lol) ps))]))

;loop pstream -> pstream
;convert this to list all
#;(define (queue-sound ls ps)
  (cond [(empty? (loop-posns ls)) ps]
        [else (pstream-queue ps (loop-snd ls) (first (loop-posns ls)))]))

;loop pstream -> pstream
;creates queue of sounds at 
(define (queue-sounds ls ps)
  (cond [(empty? (loop-posns ls)) ps]
        [else
         (queue-sounds (make-loop (loop-snd ls) (rest (loop-posns ls))) 
                      (pstream-queue ps (loop-snd ls) (first (loop-posns ls))))]))

; loopA pstream -> pstream
(define (queue-sounds2 loopA ps)
 (cond [(empty? (loop-posns loopA)) ps]
        [else
         (queue-sounds2 (make-loop (loop-snd loopA) (rest (loop-posns loopA)) (loop-key loopA)) 
                        (pstream-queue ps (loop-snd loopA) (tick-frames (first (loop-posns loopA)))))
        ]))

;plays for full tick? convert to frames?
;current-frames position sound pstream -> pstream
(define (maybe-play posn snd ps)
  (cond [(< (- (tick-frames posn) (pstream-current-frame ps)) LEAD-FRAMES)
         (pstream-queue ps snd (tick-frames posn))]
        [else ps]))

(define (both a b)
  b)

;converts ticks to frames
(define (tick-frames posn)
  (cond [(= posn 0) posn]
        [else (round (* 44100 (/ 28 posn)))]))

(big-bang (make-world (cons (make-loop c-hi-hat-1 empty "a") (cons (make-loop kick empty "b") empty)) (make-pstream) 0)
          [to-draw draw-world]
          [on-key key-handler]
          [on-tick tock])