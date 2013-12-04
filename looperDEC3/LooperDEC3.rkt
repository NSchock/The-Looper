;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname LooperDEC3) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")))))
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
(define-struct world (loops paused recording start-time loop-time ps))

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
    (place-loops (world-loops w) (place-prog bar (world-paused w) (world-loop-time w) (world-ps w)) (world-loop-time w))
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
   (round (* B-LENGTH (/ (modulo (posin-pos posin) (if (= ltime 0) 1 ltime)) (if (= ltime 0) 1 ltime)))))
 
;; if value is a num, draw. if value is false, don't draw.
;; posin image -> image
(define (maybe-draw posin cur-bar col ltime)
  (cond [(equal? true (posin-active posin)) (overlay/xy (tracker col) (- 0 (pos-pixel posin ltime)) 0 cur-bar)]
        [(equal? false (posin-active posin)) cur-bar]
        )) 

;places current position in loop on the bar as the tracker
;number->image
(define (place-prog cur-bar paused ltime ps)
(if paused
    cur-bar
    (overlay/xy prog-tracker (- 0 (round  (* B-LENGTH (/ (modulo (pstream-current-frame ps) (if (= ltime 0) 1 ltime)) (if (= ltime 0) 1 ltime))))) 0 cur-bar)
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
             
                           (make-world
                            (world-loops w)
                            (world-paused w)
                            false
                            (smallest-loop (world-loops w) (world-ps w))
                            (- (pstream-current-frame (world-ps w)) (smallest-loop (world-loops w) (world-ps w)))
                            (world-ps w)) 
                           (make-world 
                            (world-loops w)
                            (world-paused w)
                            true
                            (world-start-time w)
                            (world-loop-time w)
                            (make-pstream))
                           )]
                      ; taken from construct-loop-list code for "escape" keypress
                      [(< 918 x 987) (make-world (construct-loop-list
                                                  (world-loops w)
                                                  (make-posin (pstream-current-frame (world-ps w)) true) "escape" true (world-recording w))
                                                 (world-paused w)
                                                 (world-recording w)
                                                 (world-start-time w)
                                                 (world-loop-time w)
                                                 (make-pstream))]
                      
                      ; taken from saving function 
                      [else (cond [(< 555 y 625) 
                                    (cond [(< 1001 x 1055) 
                                           (if (has-posns (world-loops w))
                                               (both (save-sound (conv-list (world-loops w) (world-loop-time w)))
                                                     w)
                                               w)]
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
         (make-world (world-loops w)
                     (not (world-paused w))
                     (world-recording w)
                     (world-start-time w)
                     (world-loop-time w)
                     (world-ps w))]
        [(key=? ke " ")
         (if (world-recording w) ;means space bar has been hit to STOP recording -> end loop, make that the time
             
             (make-world #;(world-loops w)
                         #;(remake-loop-list (world-loops w) (world-loop-time w))
                         (out-of-loop (remake-loop-list (world-loops w) (world-loop-time w)) (pstream-current-frame (world-ps w)))
                         (world-paused w)
                         false
                         (smallest-loop (remake-loop-list (world-loops w) (world-loop-time w)) (world-ps w))
                         #;(- (greatest-loop (remake-loop-list (world-loops w) (world-loop-time w))) (smallest-loop (remake-loop-list (world-loops w) (world-loop-time w)) (world-ps w)))
                         #;(abs (- (modulo (smallest-loop (world-loops w) (world-ps w)) (world-loop-time w)) (pstream-current-frame (world-ps w))))
                         #;(world-loop-time w)
                         #;(if (< (pstream-current-frame (world-ps w)) (smallest-loop (remake-loop-list (world-loops w) (world-loop-time w)) (world-ps w)))
                             (world-loop-time w)
                             (- (pstream-current-frame (world-ps w)) (smallest-loop (remake-loop-list (world-loops w) (world-loop-time w)) (world-ps w)))
                             )
                         #;(make-pstream)
                         (pstream-current-frame (world-ps w))
                         (world-ps w))
             (make-world #;(world-loops w)
                         (remake-loop-list (world-loops w) (world-loop-time w))
                         (world-paused w)
                         true
                         (world-start-time w)
                         (world-loop-time w)
                         (make-pstream)))
         
         ]
        [(key=? ke "w") 
         (if (has-posns (world-loops w))
             (both (save-sound (conv-list (world-loops w) (world-loop-time w)))
               w)
             w)]
        [else (make-world (construct-loop-list
                           (world-loops w)
                           (make-posin (pstream-current-frame (world-ps w)) true) ke false (world-recording w) (world-ps w))
                          (world-paused w)
                          (world-recording w)
                          (world-start-time w)
                          (world-loop-time w)
                          (world-ps w))])
  )

;list-of-loops->list-of-loops
;removes posn from loop if it is out of loop range
(define (out-of-loop lol ltime)
  (cond [(empty? lol) empty]
        [else (cons (make-loop (loop-snd (first lol))
                               (out-of-posns (loop-posns (first lol)) ltime)
                               (loop-key (first lol))
                               (loop-hit (first lol))
                               (loop-col (first lol)))
                    (out-of-loop (rest lol) ltime))]))

;removes position from loop if it is outside the bounds of ltime
(define (out-of-posns lop ltime)
  (cond [(empty? lop) empty]
        [else (if (< (posin-pos (first lop)) ltime)
                  (cons (first lop)
                        (out-of-posns (rest lop) ltime))
                  (out-of-posns (rest lop) ltime))]))

(check-expect (out-of-loop (list (make-loop ding (list (make-posin 69 true) (make-posin 20 true)) "q" false "blue")
                                   (make-loop kick (list (make-posin 15 true) (make-posin 420 true)) "e" false "red")) 50)
              (list (make-loop ding (list (make-posin 20 true)) "q" false "blue")
                                   (make-loop kick (list (make-posin 15 true)) "e" false "red")))

;world key -> world
(define (release w ke)
  (make-world (construct-loop-list
               (world-loops w)
               (make-posin (pstream-current-frame (world-ps w)) true) ke true (world-recording w) (world-ps w))
             (world-paused w)
             (world-recording w)
             (world-start-time w)
             (world-loop-time w)
             (world-ps w)))

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
(define (construct-loop-list lol posn ke up rec ps)
  (cond [(empty? lol) empty]
        [(key=? ke "escape") 
         (cons (make-loop (loop-snd (first lol))
                          empty
                          (loop-key (first lol))
                          false
                          (loop-col (first lol)))
               (construct-loop-list (rest lol) posn ke up rec ps))]
        [else
        (if rec 
             (if (and (not up) (valid-add ke (first lol)))
                 (cons (make-loop (loop-snd (first lol))                              
                                  (add-posn (loop-posns (first lol)) posn rec)
                                  (loop-key (first lol))
                                  (check-if-hit (loop-posns (first lol)) (pstream-current-frame ps))
                                  (loop-col (first lol)))
                       (construct-loop-list (rest lol) posn ke up rec ps))
                 (cons (make-loop (loop-snd (first lol))
                                  (loop-posns (first lol))
                                  (loop-key (first lol))
                                  (check-if-hit (loop-posns (first lol)) (pstream-current-frame ps))
                                  (loop-col (first lol)))
                       (construct-loop-list (rest lol) posn ke up rec ps)))
             (if (and (not up) (valid-add ke (first lol)))
                 (both (pstream-queue ps (loop-snd (first lol)) (pstream-current-frame ps))
                       (cons (make-loop (loop-snd (first lol))
                                  (loop-posns (first lol))
                                  (loop-key (first lol))
                                  true
                                  (loop-col (first lol)))
                       (construct-loop-list (rest lol) posn ke up rec ps)))
                 (cons (make-loop (loop-snd (first lol))
                                  (loop-posns (first lol))
                                  (loop-key (first lol))
                                  (check-if-hit (loop-posns (first lol)) (pstream-current-frame ps))
                                  (loop-col (first lol)))
                       (construct-loop-list (rest lol) posn ke up rec ps))))
         ]))

(define (remake-loop-list lol ltime)
  (cond [(empty? lol) empty]
        [else (cons (make-loop (loop-snd (first lol))
                               (remake-posns (loop-posns (first lol)) ltime)
                               (loop-key (first lol))
                               (loop-hit (first lol))
                               (loop-col (first lol)))
                    (remake-loop-list (rest lol) ltime))]))

(define (remake-posns lop ltime)
 (cond [(empty? lop) empty]
       [else (cons (make-posin (modulo (posin-pos (first lop)) ltime) (posin-active (first lop)))
                   (remake-posns (rest lop) ltime))])
  )
;key loop -> boolean
;returns true if key for sound was hit AND in recording mode, else false
(define (valid-add ke loopA)
  (if (key=? ke (loop-key loopA)) true false))

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
      (make-world (queue-all-sounds (world-loops w) (world-loop-time w) (world-ps w))
              (world-paused w)
              (world-recording w)
              (world-start-time w)
              (world-loop-time w)
              (world-ps w))
      (make-world (world-loops w)
                  (world-paused w)
                  (world-recording w)
                  (world-start-time w)
                  (world-loop-time w)
                  (world-ps w))))

;list-of-loops -> list-of-loops
(define (queue-all-sounds lol ltime ps)
  (cond [(empty? lol) empty]
        [else (cons (make-loop 
                               (loop-snd (first lol))
                               (queue-sounds (loop-posns (first lol)) (loop-snd (first lol)) ltime ps)
                               (loop-key (first lol))
                               (check-if-hit (loop-posns (first lol)) (pstream-current-frame ps))
                               (loop-col (first lol)))
                    (queue-all-sounds (rest lol) ltime ps))]))


; list-of-posns snd  -> list-of-posns
;calls maybe-play for all sounds to check if the sound should be played
(define (queue-sounds posns snd ltime ps)
 (cond [(empty? posns) empty]
        [else                      
         (cons 
          (maybe-play (first posns) snd ps ltime)
          (queue-sounds (rest posns) snd ltime ps))                               
         ]))

;position pstream sound frames-> posn
;if position is less than lead-frames away from current pstream frame,queue up sound at given position
;queues one sound at a time based on how close it is to the current frame
(define (maybe-play posn snd ps ltime)
  (cond [(< (- (posin-pos posn) LEAD-FRAMES)  (pstream-current-frame ps))
             (if (posin-active posn)
                  (both (pstream-queue ps snd (posin-pos posn))
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

;list-of-loops->boolean
;returns true if there are any positions in any loop, else false
(define (has-posns lol)
  (cond [(empty? lol) false]
        [else (if (not (empty? (loop-posns (first lol))))
                  true
                  (has-posns (rest lol)))]))


;list->void
;takes the previously defined list of rsounds and positions and converts it to one single rsound
;then saves that rsound to loop1.wav
(define (save-sound l)
  (rs-write (assemble l) "loop1.wav"))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;


;;make-tone simplifier
(define (make-note x)
  (make-tone x 0.1 20000))

;min function
;returns minimum of a list of positions
;list-of-positions->number
(define (minum lop smallest)
  (cond [(empty? lop) smallest]
        [else (minum  (rest lop) (lesser (posin-pos (first lop))
                      smallest))]))

(define (smallest lop)
  (minum lop (posin-pos (first lop))))

;returns lesser value of the two numbers
;number number -> number
(define (lesser n1 n2)
  (if (< n1 n2) n1 n2))

(define (maxum lop largest)
  (cond [(empty? lop) largest]
        [else (maxum (rest lop) (greater (posin-pos (first lop))
                      largest))]))

(define (greatest lop)
  (maxum lop (posin-pos (first lop))))

(define (greater n1 n2)
  (if (> n1 n2) n1 n2))

(check-expect (greatest (list (make-posin 69 true) (make-posin 420 true) (make-posin 20 true))) 420)

(check-expect (smallest (list (make-posin 69 true) (make-posin 420 true) (make-posin 20 true))) 20)

;returns the greatest posn value in the list of loops
;list-of-loops->number
(define (greatest-loop lol)
  (cond [(empty? lol) 0]
        [else (maxum (loop-posns (first lol))
                     (greatest-loop (rest lol)))]))

(check-expect (greatest-loop (list (make-loop ding (list (make-posin 69 true) (make-posin 20 true)) "q" false "blue")
                                   (make-loop kick (list (make-posin 15 true) (make-posin 420 true)) "e" false "red")))
              420)

;returns the smallest posn value in the list of loops
;list-of-loops number->number
(define (smallest-almost lol smallest)
  (cond [(empty? lol) smallest]
        [else (minum (loop-posns (first lol))
                     (smallest-almost (rest lol) smallest))]))

;returns the smallest posn value in the list of loops
;list-of-loops->number
(define (smallest-loop lol ps)
  (cond [(empty? (loop-posns (first lol))) (pstream-current-frame ps)]
        [else (smallest-almost lol (posin-pos (first (loop-posns (first lol)))))]))

(check-expect (smallest-loop (list (make-loop ding (list (make-posin 15 true) (make-posin 20 true)) "q" false "blue")
                                   (make-loop kick (list (make-posin 420 true) (make-posin 69 true)) "e" false "red")) (make-pstream))
              15)

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
                            (s 5)
                            (make-pstream))
          [to-draw draw-world]
          [on-key key-handler]
          [on-tick tock]
          [on-release release]
          [on-mouse mouse-handler])
