;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |LooperD3 (click it, yo) |) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  THE LOOPER   ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A project by:
;; Alex Maestre 
;; Joey Gross
;; Juan Ramirez
;; Logan Williams 
;; and Nolan Schock.

;required libs
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)
(require rsound/piano-tones)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
;-----------;            DEFINITIONS           ;-----------;;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

; C major scale notes on 4th octave
(define c4 (rs-scale 1 (piano-tone 60)))
(define d4 (rs-scale 1 (piano-tone 62)))
(define e4 (rs-scale 1 (piano-tone 64)))
(define f4 (rs-scale 1 (piano-tone 65)))
(define g4 (rs-scale 1 (piano-tone 67)))
(define a4 (rs-scale 1 (piano-tone 69)))
(define b4 (rs-scale 1 (piano-tone 71)))
(define c5 (rs-scale 1 (piano-tone 72)))

;Image call. 
;calls local files
(define C (bitmap/file "z.png"))
(define CHit (bitmap/file "zHit.png"))
(define D (bitmap/file "x.png"))
(define DHit (bitmap/file "xHit.png"))
(define E (bitmap/file "c.png"))
(define EHit (bitmap/file "cHit.png"))
(define F (bitmap/file "v.png"))
(define FHit (bitmap/file "vHit.png"))
(define G (bitmap/file "b.png"))
(define GHit (bitmap/file "bHit.png"))
(define A (bitmap/file "n.png"))
(define AHit (bitmap/file "nHit.png"))
(define B (bitmap/file "m.png"))
(define BHit (bitmap/file "mHit.png"))
(define C5 (bitmap/file ",.png"))
(define C5Hit (bitmap/file ",Hit.png"))
(define I (bitmap/file "i.png"))
(define IHit (bitmap/file "iHit.png"))
(define K (bitmap/file "k.png"))
(define KHit (bitmap/file "kHit.png"))
(define L (bitmap/file "l.png"))
(define LHit (bitmap/file "lHit.png"))
(define O (bitmap/file "o.png"))
(define OHit (bitmap/file "oHit.png"))
(define P (bitmap/file "p.png"))
(define PHit (bitmap/file "pHit.png"))
(define back (bitmap/file "back.png"))
(define clearbox (bitmap/file "clearbox.png"))
(define record (bitmap/file "record2.png"))
(define recordhit (bitmap/file "record2Hit.png"))
(define reset (bitmap/file "reset.png"))
(define save (bitmap/file "savebutton.png"))
(define THELOOPER (bitmap/file "LOOPER.png"))

;graphics
(define WIDTH 500)
(define HEIGHT 800)
(define LEFT-PAD 40)
(define RIGHT-PAD 100)
(define LINE-HEIGHT 25)
(define NUM-SLIDERS 8)
(define VERT-PAD 100)

;a loop is (make-loop sound list-of-posns key boolean boolean)
;list-of-posns is all positions in loop where sound occurs
(define-struct loop (snd posns key hit col))

(define-struct posin (pos active))

(define (s x)
  (* 44100 x))

(define LOOP-TIME (s 5))
(define LEAD-FRAMES (s 1/28))


; world is (make-world list-of-loops boolean boolean)
(define-struct world (loops paused recording start-time loop-time))

(define PST (make-pstream))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
;-----------;              TO-DRAW             ;-----------;;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
(define (draw-world w) 
  (overlay
  (above
   (above
   (above 
    THELOOPER 
    (beside 
     (overlay/xy
      (if (loop-hit (list-ref (world-loops w) 12)) LHit L) -350 -100
      (overlay/xy
       (if (loop-hit (list-ref (world-loops w) 9)) KHit K) -172 -165
       (overlay/xy
        (if (loop-hit (list-ref (world-loops w) 11)) IHit I) -10 -100 
        (overlay/xy
         (if (loop-hit (list-ref (world-loops w) 10)) PHit P) -300 -25
         (overlay/xy
          (if (loop-hit (list-ref (world-loops w) 8)) OHit O) -120 -20
          empty-image)))))
     clearbox
     (if (loop-hit (list-ref (world-loops w) 0)) CHit C) 
     (if (loop-hit (list-ref (world-loops w) 1)) DHit D)
     (if (loop-hit (list-ref (world-loops w) 2)) EHit E)
     (if (loop-hit (list-ref (world-loops w) 3)) FHit F)
     (if (loop-hit (list-ref (world-loops w) 4)) GHit G)
     (if (loop-hit (list-ref (world-loops w) 5)) AHit A)
     (if (loop-hit (list-ref (world-loops w) 6)) BHit B)
     (if (loop-hit (list-ref (world-loops w) 7)) C5Hit C5)))
  
     clearbox)            
   
   (beside
    (if (world-recording w) recordhit record)
    (beside
    (place-loops (world-loops w) (place-prog bar (world-paused w) (world-loop-time w)) (world-loop-time w))
    (beside
     clearbox 
     (beside reset save)))))
  back))

;;---------------------------------------------------------------------;;
;;                              TRACKER                                ;;
;;---------------------------------------------------------------------;;

            ;~~~~~~~~~~~~~~~~~;
            ;  GUI Constants  ;
            ;~~~~~~~~~~~~~~~~~;

;; bar
(define B-HEIGHT 30)
(define B-LENGTH 700)
(define bar (rectangle B-LENGTH B-HEIGHT "solid" "cornflowerblue"))

;; width of the tracker
(define T-WIDTH 3)

;; tracker (to show instrument positions)
;string->image
(define (tracker clr) 
  (rectangle T-WIDTH B-HEIGHT "solid" clr))

;; prog tracker (to show current position)
(define prog-tracker (rectangle T-WIDTH B-HEIGHT "solid" "black"))

   ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

            ;~~~~~~~~~~~~~~~~~;
            ;  GUI Variables  ;
            ;~~~~~~~~~~~~~~~~~;

;; convert frame position to relative pixel position on the bar
;; posin -> num
 (define (pos-pixel posin ltime) 
   (round (* B-LENGTH (/ (modulo (posin-pos posin) ltime) ltime))))
 
;; if value is a num, draw. if value is false, don't draw.
;; posin image -> image
(define (maybe-draw posin cur-bar col ltime)
  (cond [(equal? true (posin-active posin)) (overlay/xy (tracker col) (- 0 (pos-pixel posin ltime)) 0 cur-bar)]
        [(equal? false (posin-active posin)) cur-bar]
        )) 

;places current position in loop on the bar as the tracker
;number->image
(define (place-prog cur-bar paused ltime)
(if paused
    cur-bar
    (overlay/xy prog-tracker (- 0 (round  (* B-LENGTH (/ (modulo (pstream-current-frame PST) ltime) ltime)))) 0 cur-bar)
    ))

;; draw first set
;; list-of-posins image color -> image
(define (maybe-draw-all posns cur-bar col ltime)
  (cond [(empty? posns) cur-bar]
        [else (maybe-draw (first posns) (maybe-draw-all (rest posns) cur-bar col ltime) col ltime)]
        )) 

;list-of-loops image->image
;takes all loops in the world and calls place-posns to place positions for each loop
(define (place-loops lol cur-bar ltime)
  (cond [(empty? lol) cur-bar]
        [else (place-loops (rest lol) (maybe-draw-all (loop-posns (first lol)) cur-bar (loop-col (first lol)) ltime) ltime)]))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
;-----------;              ON-MOUSE            ;-----------;;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

;; mouse-handler
;; manages mouse events on the three buttons
;; world x-pos y-pos mouse-event -> world

(define (mouse-handler w x y evt)
  (cond [(mouse=? evt "button-up") 
         (cond [(< 550 y 630) 
                (cond [(< 60 x 185) 
                       
                       ;taken from " " keypress
                       (if (world-recording w) ;means space bar has been hit to STOP recording -> end loop, make that the time
                           
                           (make-world #;(construct-loop-list 
                                        (world-loops w) 
                                        (make-posin (pstream-current-frame PST) true) ke false (not (world-recording w)))
                                       (world-loops w)
                                       (world-paused w)
                                       (not (world-recording w))
                                       (pstream-current-frame PST)
                                       (world-loop-time w))
                           (make-world (world-loops w)
                                       (world-paused w)
                                       (not (world-recording w))
                                       (pstream-current-frame PST)
                                       (world-loop-time w))
                           )]
                      ; taken from construct-loop-list code for "escape" keypress
                      [(< 918 x 987) (make-world (construct-loop-list
                                                  (world-loops w)
                                                  (make-posin (pstream-current-frame PST) true) "escape" true (world-recording w))
                                                 (world-paused w)
                                                 (world-recording w)
                                                 (world-start-time w)
                                                 (world-loop-time w))]
                      
                      ; taken from saving function 
                      [else (cond [(< 555 y 625) 
                                    (cond [(< 1001 x 1055) (both (save-sound (conv-list (world-loops w) (world-loop-time w))) w)]
                                          [else w])]
                                  [else w])])]
               [else w])]
        [else w]))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
;-----------;              ON-KEY              ;-----------;;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;



;world key -> world
(define (key-handler w ke)
  
  (cond [(key=? ke "\r")
         (make-world (construct-loop-list 
                      (world-loops w) 
                      (make-posin (pstream-current-frame PST) true) ke false (world-recording w))
                     (not (world-paused w))
                     (world-recording w)
                     0
                     0)]
        [(key=? ke " ")
         (if (world-recording w) ;means space bar has been hit to STOP recording -> end loop, make that the time
             
             (make-world (construct-loop-list 
                          (world-loops w) 
                          (make-posin (pstream-current-frame PST) true) ke false (not (world-recording w)))
                         (world-paused w)
                         (not (world-recording w))
                         (pstream-current-frame PST)
                         (world-loop-time w))
             (make-world (construct-loop-list 
                          (world-loops w) 
                          (make-posin (pstream-current-frame PST) true) ke false (not (world-recording w)))
                         (world-paused w)
                         (not (world-recording w))
                         (pstream-current-frame PST)
                         (world-loop-time w))
         )
         ]
        [(key=? ke "w") 
         (both (save-sound (conv-list (world-loops w) (world-loop-time w)))
               w)]
        [else (make-world (construct-loop-list
                           (world-loops w)
                           (make-posin (pstream-current-frame PST) true) ke false (world-recording w))
                          (world-paused w)
                          (world-recording w)
                          (world-start-time w)
                          (world-loop-time w))])
  )

(define (play-no-rec-all lol ke rec)
  (if rec
      (cond [(empty? lol) PST]
            [else (both (play-no-rec (first lol) ke)
                        (play-no-rec-all (rest lol) ke rec))])
      lol))

(define (play-no-rec loopA ke)
  (cond [(key=? ke (loop-key loopA)) (pstream-queue PST (loop-snd loopA) (pstream-current-frame PST))]
        [else PST]))

;world key -> world
(define (release w ke)
  (make-world (construct-loop-list
               (world-loops w)
               (make-posin (pstream-current-frame PST) true) ke true (world-recording w))
             (world-paused w)
             (world-recording w)
             (world-start-time w)
             (world-loop-time w)))

;list-of-positions position->list-of-positions
;returns a list of positions with one more position added
(define (add-posn lop posn rec)
  (if rec 
      (cond [(empty? lop) (cons posn empty)]
        [else
         (cons posn (cons (first lop)
                          (rest lop)))])
  lop))

;list-of-loops posn key boolean -> list-of-loops
;constructs a list of loops with an added position for the corresponding sound to the keypress
(define (construct-loop-list lol posn ke up rec)
  (cond [(empty? lol) empty]
        [(key=? ke "escape") 
         (cons (make-loop (loop-snd (first lol))
                          empty
                          (loop-key (first lol))
                          false
                          (loop-col (first lol)))
               (construct-loop-list (rest lol) posn ke up rec))]
        [else
        (if rec 
             (if (and (not up) (valid-add ke (first lol)))
                 (cons (make-loop (loop-snd (first lol))                              
                                  (add-posn (loop-posns (first lol)) posn rec)
                                  (loop-key (first lol))
                                  (check-if-hit (loop-posns (first lol)) (pstream-current-frame PST))
                                  (loop-col (first lol)))
                       (construct-loop-list (rest lol) posn ke up rec))
                 (cons (make-loop (loop-snd (first lol))
                                  (loop-posns (first lol))
                                  (loop-key (first lol))
                                  (check-if-hit (loop-posns (first lol)) (pstream-current-frame PST))
                                  (loop-col (first lol)))
                       (construct-loop-list (rest lol) posn ke up rec)))
             (if (and (not up) (valid-add ke (first lol)))
                 (both (pstream-queue PST (loop-snd (first lol)) (pstream-current-frame PST))
                       (cons (make-loop (loop-snd (first lol))
                                  (loop-posns (first lol))
                                  (loop-key (first lol))
                                  true
                                  (loop-col (first lol)))
                       (construct-loop-list (rest lol) posn ke up rec)))
                 (cons (make-loop (loop-snd (first lol))
                                  (loop-posns (first lol))
                                  (loop-key (first lol))
                                  (check-if-hit (loop-posns (first lol)) (pstream-current-frame PST))
                                  (loop-col (first lol)))
                       (construct-loop-list (rest lol) posn ke up rec))))
         ]))

;key loop -> boolean
;returns true if key for sound was hit AND in recording mode, else false
(define (valid-add ke loopA)
  (if (key=? ke (loop-key loopA)) true false))

#;(check-expect (add-posn empty 12) (cons 12 empty))
#;(check-expect (add-posn (cons 12 empty) 20) (cons 20 (cons 12 empty)))
#;(check-expect (add-posn (cons 20 (cons 12 empty)) 70) (cons 70 (cons 20 (cons 12 empty))))

#;(check-expect (construct-loop-list (cons (make-loop kick (cons 70 empty) "a") (cons (make-loop c-hi-hat-1 (cons 420 empty) "b") empty)) 42 "b")
                (cons (make-loop kick (cons 70 empty) "a") (cons (make-loop c-hi-hat-1 (cons 420 (cons 42 empty)) "b") empty)))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

                   ;-----------------------;
                   ;        LOOPING        ;
                   ;-----------------------;

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
;-----------;              ON-TICK             ;-----------;;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

;if frames == any loop posn, play at current time
;world->world
(define (tock w)
  (if (not (world-paused w))
      (make-world (queue-all-sounds (world-loops w) (world-loop-time w))
              (world-paused w)
              (world-recording w)
              (world-start-time w)
              (world-loop-time w))
      (make-world (world-loops w)
                  (world-paused w)
                  (world-recording w)
                  (world-start-time w)
                  (world-loop-time w))))

;list-of-loops -> list-of-loops
(define (queue-all-sounds lol ltime)
  (cond [(empty? lol) empty]
        [else (cons (make-loop 
                               (loop-snd (first lol))
                               (queue-sounds (loop-posns (first lol)) (loop-snd (first lol)) ltime)
                               (loop-key (first lol))
                               (check-if-hit (loop-posns (first lol)) (pstream-current-frame PST))
                               (loop-col (first lol)))
                    (queue-all-sounds (rest lol) ltime))]))


; list-of-posns snd  -> list-of-posns
;calls maybe-play for all sounds to check if the sound should be played
(define (queue-sounds posns snd ltime)
 (cond [(empty? posns) empty]
        [else                      
         (cons 
          (maybe-play (first posns) snd (pstream-current-frame PST) ltime)
          (queue-sounds (rest posns) snd ltime))                               
         ]))

;position pstream sound frames-> posn
;if position is less than lead-frames away from current pstream frame,queue up sound at given position
;queues one sound at a time based on how close it is to the current frame
(define (maybe-play posn snd cur-frames ltime)
  (cond [(< (- (posin-pos posn) LEAD-FRAMES)  cur-frames)
             (if (posin-active posn)
                  (both (pstream-queue PST snd (posin-pos posn))
                    (make-posin (+ (posin-pos posn) ltime) (posin-active posn)))
                 (make-posin (+ (posin-pos posn) ltime) (posin-active posn)))
         ]
        [else posn]))

#;(define (is-hit posn cur-frames)
  (< (- posn LEAD-FRAMES) cur-frames))

(define (check-if-hit posns cur-frames)
  (cond [(empty? posns) false]
        [else (if (< (- (posin-pos (first posns)) LEAD-FRAMES) cur-frames)
                  true
                  (check-if-hit (rest posns) cur-frames))
              ]))

(define (both a b)
  b)

;;---------------------------------------------------------------------;;
;;                              SAVING                                 ;;
;;---------------------------------------------------------------------;;

;list-of-loops->list-of-lists
;converts a list of loops to a list of lists
;each list inside the main list contains a sound and the position of that sound.
(define (conv-list lol ltime)
  (cond [(empty? lol) empty]
        [else 
         (if (empty? (loop-posns (first lol)))
             (conv-list (rest lol) ltime)
             (cons 
              (if (posin-active (first (loop-posns (first lol))))
                  (cons (loop-snd (first lol)) (cons (modulo (posin-pos (first (loop-posns (first lol)))) ltime) empty))
                  (conv-list (rest lol) ltime))
              (conv-list (rest lol) ltime)))]))


;list->void
;takes the previously defined list of rsounds and positions and converts it to one single rsound
;then saves that rsound to loop1.wav
(define (save-sound l)
  (rs-write (assemble l) "loop1.wav"))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;


;;make-tone simplifier
(define (make-note x)
  (make-tone x 0.1 20000))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                          ;
;                     THE BIG-BANG                         ;
;                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(big-bang (make-world (cons (make-loop c4 empty "z" false "firebrick")
                            (cons (make-loop d4 empty "x" false "crimson") 
                            (cons (make-loop e4 empty "c" false "red")
                            (cons (make-loop f4 empty "v" false "tomato")
                            (cons (make-loop g4 empty "b" false "indianred")
                            (cons (make-loop a4 empty "n" false "lightcoral") 
                            (cons (make-loop b4 empty "m" false "lightpink")
                            (cons (make-loop c5 empty "," false "lavenderblush")  
                                  
                            (cons (make-loop (rs-scale 0.5 c-hi-hat-1) empty "o" false "yellow") 
                            (cons (make-loop (rs-scale 0.5 kick) empty "k" false "midnightblue")
                            (cons (make-loop (rs-scale 0.5 o-hi-hat) empty "p" false "gold") 
                            (cons (make-loop (rs-scale 0.5 snare) empty "i" false "royalblue")
                            (cons (make-loop (rs-scale 0.5 bassdrum) empty "l" false "darkslategray")
                                  
                            
                            
                            (cons (make-loop (make-note 261.63) empty "a" false "brown") 
                            (cons (make-loop (make-note 293.66) empty "s" false "sienna") 
                            (cons (make-loop (make-note 329.63) empty "d" false "chocolate")
                            (cons (make-loop (make-note 349.23) empty "f" false "darkorange")
                            (cons (make-loop (make-note 392.00) empty "g" false "coral")
                            (cons (make-loop (make-note 440.00) empty "h" false "darksalmon") 
                            (cons (make-loop (make-note 493.88) empty "j" false "lightsalmon")   
                                  
                             empty))))))))))))))))))))
                            false
                            false
                            0
                            (s 1))
          [to-draw draw-world]
          [on-mouse mouse-handler]
          [on-key key-handler]
          [on-tick tock]
          [on-release release])