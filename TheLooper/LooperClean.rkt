;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname LooperClean) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  THE LOOPER   ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A project by:
;; Alex Maestre 
;; Joey Gross
;; Juan Ramirez
;; Logan Williams 
;; Nolan Schock

;~~~~~~~~~~~~~~~~~~~~~~~~~~;

;required libs
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)
(require rsound/piano-tones)
(require "put-file.rkt")
(require "get-file.rkt")


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
;-----------;      STRUCTURES        ;-----------;;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

;a loop is (make-loop sound list-of-posins key boolean boolean)
;list-of-posins is all positions in loop where sound occurs
(define-struct loop (snd posns key hit col))

; a posin is (make-posin number boolean)
(define-struct posin (pos active))

; world is (make-world list-of-loops boolean boolean frames pstream)
(define-struct world (loops paused recording loop-time ps))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
;-----------;      DEFINITIONS      ;-----------;;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

; C major scale notes on 4th octave and C from the 5th octave
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

;num -> num
;converts x to frames
(define (s x)
  (* 44100 x))

(define LEAD-FRAMES (s 1/28))

(define start-loop-time (s 60))

;pitch -> tone
;make-tone simplifier
(define (make-note x)
  (make-tone x 0.1 20000))

;a b -> b
;helper funtion both
(define (both a b) b)


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
;-----------;              TO-DRAW             ;-----------;;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

;; big-bang's draw function
;; world -> image
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
;; posin loop-time -> pixel
 (define (pos-pixel posin ltime) 
   (round (* B-LENGTH (/ (modulo (posin-pos posin) (if (= ltime 0) 1 ltime)) (if (= ltime 0) 1 ltime)))))
 
;; if position is active, draw it
;; posin image color loop-time -> image
(define (maybe-draw posin cur-bar col ltime)
  (cond [(equal? true (posin-active posin)) (overlay/xy (tracker col) (- 0 (pos-pixel posin ltime)) 0 cur-bar)]
        [(equal? false (posin-active posin)) cur-bar]
        )) 

;places current position in loop on the bar as the tracker
;image boolean loop-time pstream->image
(define (place-prog cur-bar paused ltime ps)
(if paused
    cur-bar
    (overlay/xy prog-tracker (- 0 (round  (* B-LENGTH (/ (modulo (pstream-current-frame ps) (if (= ltime 0) 1 ltime)) (if (= ltime 0) 1 ltime))))) 0 cur-bar)
    ))

;; draw positions from the first loop
;; list-of-posins image color -> image
(define (maybe-draw-all posns cur-bar col ltime)
  (cond [(empty? posns) cur-bar]
        [else (maybe-draw (first posns) (maybe-draw-all (rest posns) cur-bar col ltime) col ltime)]
        )) 

;list-of-loops image->image
;takes all loops in the world and calls maybe-draw-all to place positions for each loop
(define (place-loops lol cur-bar ltime)
  (cond [(empty? lol) cur-bar]
        [else (place-loops (rest lol) (maybe-draw-all (loop-posns (first lol)) cur-bar (loop-col (first lol)) ltime) ltime)]))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
;-----------;              ON-MOUSE            ;-----------;;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

;; mouse-handler
;; manages mouse events on the three buttons (record, reset, & save)
;; world x-pos y-pos mouse-event -> world
(define (mouse-handler w x y evt)
  (cond [(mouse=? evt "button-up") 
         (cond [(< 550 y 630) 
                (cond [(< 60 x 185)                      
                       ;taken from " " keypress
                       (if (world-recording w) 
                           ;means space bar has been hit to STOP recording -> end loop, make pstream-current-frame the loop-time
                           (make-world 
                            (out-of-loop (remake-loop-list (world-loops w) (world-loop-time w)) (pstream-current-frame (world-ps w)))
                            (world-paused w)
                            false
                            (pstream-current-frame (world-ps w))
                            (world-ps w))
                           ;else
                           (make-world
                            (remake-loop-list (world-loops w) (world-loop-time w))
                            (world-paused w)
                            true
                            (if (no-loop (world-loops w))
                                start-loop-time
                                (world-loop-time w))
                            (make-pstream)))]
                      
                      ; taken from "escape" keypress                                                                                                                        
                      [(< 918 x 987) (make-world (clear-list
                                                  (if (valid-add-loop (world-loops w))
                                                      (world-loops w)
                                                      (rem-last (world-loops w) (- (len (world-loops w)) 1))))
                                                 (world-paused w)
                                                 false
                                                 (world-loop-time w)
                                                 (world-ps w))]
                      
                      ; taken from "w" keypress saving function 
                      [else (cond [(< 555 y 625) 
                                   (cond [(< 1001 x 1055) 
                                          (make-world (save-sound (world-loops w) (put-file) (world-loop-time w))
                                                      (world-paused w)
                                                      (world-recording w)
                                                      (world-loop-time w)
                                                      (world-ps w))]
                                         [else w])]
                                  [else w])])]
               [else w])]
        [else w]))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
;-----------;              ON-KEY              ;-----------;;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

; big-bang's key-handler
; world key -> world
(define (key-handler w ke)
  (cond [(key=? ke "\r") ;hardly used feature to mute the sounds when return key is hit
         (make-world (world-loops w)
                     (not (world-paused w))
                     (world-recording w)
                     (world-loop-time w)
                     (world-ps w))]
        
        [(key=? ke " ") ;begin or end recording on space key
         (cond [(equal? (world-loop-time w) start-loop-time) 
                (if (world-recording w) 
                    ;means space bar has been hit to STOP recording -> end loop, make that the time
                    (make-world 
                     (out-of-loop (remake-loop-list (world-loops w) (world-loop-time w)) (pstream-current-frame (world-ps w)))
                     (world-paused w)
                     false
                     (pstream-current-frame (world-ps w))
                     (world-ps w))
                    
                    (make-world
                     (remake-loop-list (world-loops w) (world-loop-time w))
                     (world-paused w)
                     true
                     (if (no-loop (world-loops w))
                         start-loop-time
                         (world-loop-time w))
                     (make-pstream)))]
               
               [else (if (world-recording w) 
                         
                         (make-world 
                          (out-of-loop (remake-loop-list (world-loops w) (world-loop-time w)) (world-loop-time w))
                          (world-paused w)
                          false
                          (world-loop-time w)
                          (world-ps w))
                         
                         (make-world
                          (remake-loop-list (world-loops w) (world-loop-time w))
                          (world-paused w)
                          true
                          
                          (if (no-loop (world-loops w))
                              start-loop-time
                              (world-loop-time w))
                          (make-pstream)))])]
        
        [(key=? ke "w") ;save sound on w key
                 (make-world (save-sound (world-loops w) (put-file) (world-loop-time w))
                             (world-paused w)
                             (world-recording w)
                             (world-loop-time w)
                             (world-ps w))]
        
        [(key=? ke "e") ;load a sound file on e key
         (make-world (load-file (world-loops w) (get-file))
                     (world-paused w)
                     (world-recording w)
                     (world-loop-time w)
                     (world-ps w))]
        
        [(key=? ke "escape") ;reset on escape key
         (make-world (clear-list
                           (if (valid-add-loop (world-loops w))
                               (world-loops w)
                               (rem-last (world-loops w) (- (len (world-loops w)) 1))))
                          (world-paused w)
                          false
                          (world-loop-time w)
                          (world-ps w))]
        
        [else (make-world (if (world-recording w) 
                            (add-loop-pos (world-loops w) (make-posin (pstream-current-frame (world-ps w)) true) ke (world-ps w))
                            (play-loop (world-loops w) ke (world-ps w)))
                          (world-paused w)
                          (world-recording w)
                          (world-loop-time w)
                          (world-ps w))])
  )

;reads in a file the user chooses and maps it to "q"
;list-of-loops file -> list-of-loops
(define (load-file l file)
  (if (not (equal? file false))       
      (add-loop l (make-loop (rs-read file) empty "q" false "blue"))          
      l))

;removes posn from loop if it is out of loop range
;list-of-loops loop-time->list-of-loops
(define (out-of-loop lol ltime)
  (cond [(empty? lol) empty]
        [else (cons (make-loop (loop-snd (first lol))
                               (out-of-posns (loop-posns (first lol)) ltime)
                               (loop-key (first lol))
                               (loop-hit (first lol))
                               (loop-col (first lol)))
                    (out-of-loop (rest lol) ltime))]))

;removes position from loop if it is outside the bounds of ltime
;list-of-posins loop-time->list-of-posins
(define (out-of-posns lop ltime)
  (cond [(empty? lop) empty]
        [else (if (< (posin-pos (first lop)) ltime)
                  (cons (first lop)
                        (out-of-posns (rest lop) ltime))
                  (out-of-posns (rest lop) ltime))]))

;list-of-loops->boolean
;true if no positions in any loop, false otherwise
(define (no-loop lol)
  (cond [(empty? lol) true]
        [else (if (not (empty? (loop-posns (first lol))))
                  false
                  (no-loop(rest lol)))]))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
;-----------;           ON-RELEASE             ;-----------;;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

;big-bang's on-release function
;;calls up-loop to change image but keep loop same
;world key -> world
(define (release w ke)
  (make-world (up-loop (world-loops w) (world-ps w))
             (world-paused w)
             (world-recording w)
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

;list-of-loops loop->list-of-loops
;appends the input loop to the end of the input list of loops
(define (add-loop lol loopA)
  (cond [(empty? lol) (cons loopA empty)]
        [else
         (if (valid-add-loop lol)
             (append lol (list loopA))
             (maybe-add-loop lol loopA))]))

;list-of-loops loop -> list-of-loops
;if there is a loop in the list-of-loops with key q, replace that loop with input loop
;otherwise returns same list-of-loops
(define (maybe-add-loop lol loopA)
  (cond [(empty? lol) empty]
        [else (cons (if (string=? (loop-key (first lol)) "q")
                        loopA
                        (first lol))
                    (maybe-add-loop (rest lol) loopA))]))
 
;list-of-loops -> boolean
;checks a list-of-loops for any "q" loops, if there is a "q"-return false
;checks whether a loop should be added
(define (valid-add-loop lol)
  (cond [(empty? lol) true]
        [else (if (string=? (loop-key (first lol)) "q")
                  false
                  (valid-add-loop (rest lol)))]))

;list number->list
;the second argument must be the length of the list - 1
;returns list without the last element of the input list
(define (rem-last l n)
  (cond [(= n 0) empty]
        [else (cons (first l)
                    (rem-last (rest l) (- n 1)))]))

;list -> number
;returns length of a list
(define (len l)
  (cond [(empty? l) 0]
        [else (+ 1 (len (rest l)))]))

;list-of-loops->list-of-loops
;returns a list-of-loops with no positions
(define (clear-list lol) 
  (cond [(empty? lol) empty]
        [else
         (cons (make-loop (loop-snd (first lol))
                          empty
                          (loop-key (first lol))
                          false
                          (loop-col (first lol)))
               (clear-list (rest lol)))]))
          
;list-of-loops key pstream->list-of-loops
;adds position to the loop with the corresponding key 
;called when key is pushed down and recording mode is active
(define (add-loop-pos lol posn ke ps)
  (cond [(empty? lol) empty]
        [else 
                 (if (valid-add ke (first lol))
                     (cons (make-loop (loop-snd (first lol))                              
                                  (add-posn (loop-posns (first lol)) posn true)
                                  (loop-key (first lol))
                                  (check-if-hit (loop-posns (first lol)) (pstream-current-frame ps))
                                  (loop-col (first lol)))
                       (add-loop-pos (rest lol) posn ke ps))
                     (cons (make-loop (loop-snd (first lol))
                                  (loop-posns (first lol))
                                  (loop-key (first lol))
                                  (check-if-hit (loop-posns (first lol)) (pstream-current-frame ps))
                                  (loop-col (first lol)))
                       (add-loop-pos (rest lol) posn ke ps)))
          ]))

;list-of-loops key pstream->list-of-loops
;plays a sound but does not add it to the list of positions for the loop
;called when key is pushed down and recording mode is NOT active
(define (play-loop lol ke ps)
  (cond [(empty? lol) empty]
        [else 
                 (if (valid-add ke (first lol))
                     (both (pstream-queue ps (loop-snd (first lol)) (pstream-current-frame ps))
                       (cons (make-loop (loop-snd (first lol))
                                  (loop-posns (first lol))
                                  (loop-key (first lol))
                                  true
                                  (loop-col (first lol)))
                       (play-loop (rest lol) ke ps)))
                     (cons (make-loop (loop-snd (first lol))
                                  (loop-posns (first lol))
                                  (loop-key (first lol))
                                  (check-if-hit (loop-posns (first lol)) (pstream-current-frame ps))
                                  (loop-col (first lol)))
                       (play-loop (rest lol) ke ps)))]))

;list-of-loops ps->list-of-loops
;returns the same list but with the boolean for hit changed to change the key image
;called on key-up
(define (up-loop lol ps)
  (cond [(empty? lol) empty]
        [else 
                 (cons (make-loop (loop-snd (first lol))
                                  (loop-posns (first lol))
                                  (loop-key (first lol))
                                  (check-if-hit (loop-posns (first lol)) (pstream-current-frame ps))
                                  (loop-col (first lol)))
                       (up-loop (rest lol) ps))]))

;list-of-loops loop-time -> list-of-loops
;changes all posns in all loops to fit new loop-time 'ltime'
(define (remake-loop-list lol ltime)
  (cond [(empty? lol) empty]
        [else (cons (make-loop (loop-snd (first lol))
                               (remake-posns (loop-posns (first lol)) ltime)
                               (loop-key (first lol))
                               (loop-hit (first lol))
                               (loop-col (first lol)))
                    (remake-loop-list (rest lol) ltime))]))

;list-of-posins loop-time -> list-of-posins
;changes posns in lop to fit new loop-time 'ltime'
(define (remake-posns lop ltime)
 (cond [(empty? lop) empty]
       [else (cons (make-posin (modulo (posin-pos (first lop)) ltime) (posin-active (first lop)))
                   (remake-posns (rest lop) ltime))]))

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

;world->world
;if frames == any loop posn, play at current time
(define (tock w)
  (if (not (world-paused w))
      (make-world (queue-all-sounds (world-loops w) (world-loop-time w) (world-ps w))
              (world-paused w)
              (world-recording w)
              
              (world-loop-time w)
              (world-ps w))
      (make-world (world-loops w)
                  (world-paused w)
                  (world-recording w)
                  
                  (world-loop-time w)
                  (world-ps w))))

;list-of-loops loop-time pstream -> list-of-loops
;calls queue-sounds for all sounds in list-of-loops to check if sounds should be played
(define (queue-all-sounds lol ltime ps)
  (cond [(empty? lol) empty]
        [else (cons (make-loop 
                     (loop-snd (first lol))
                     (queue-sounds (loop-posns (first lol)) (loop-snd (first lol)) ltime ps)
                     (loop-key (first lol))
                     (check-if-hit (loop-posns (first lol)) (pstream-current-frame ps))
                     (loop-col (first lol)))
                    (queue-all-sounds (rest lol) ltime ps))]))


;list-of-posns snd  loop-time pstream -> list-of-posns
;calls maybe-play for all sounds to check if the sound should be played
(define (queue-sounds posns snd ltime ps)
  (cond [(empty? posns) empty]
        [else                      
         (cons 
          (maybe-play (first posns) snd ps ltime)
          (queue-sounds (rest posns) snd ltime ps))]))

; position sound pstream loop-time -> posn
; if position is less than lead-frames away from current pstream frame,queue up sound at given position
; queues one sound at a time based on how close it is to the current frame
(define (maybe-play posn snd ps ltime)
  (cond [(< (- (posin-pos posn) LEAD-FRAMES)  (pstream-current-frame ps))
         (if (posin-active posn)
             (both (pstream-queue ps snd (posin-pos posn))
                   (make-posin (+ (posin-pos posn) ltime) (posin-active posn)))
             (make-posin (+ (posin-pos posn) ltime) (posin-active posn)))]
        [else posn]))

; positions frame -> boolean
; compares all posns to cur-frame until it is true
(define (check-if-hit posns cur-frames)
  (cond [(empty? posns) false]
        [else (if (< (- (posin-pos (first posns)) LEAD-FRAMES) cur-frames)
                  true
                  (check-if-hit (rest posns) cur-frames))]))


;;---------------------------------------------------------------------;;
;;                              SAVING                                 ;;
;;---------------------------------------------------------------------;;

; sound list-of-positions loop-time -> listception (list inside list...)
; converts parameters into a list of lists of sounds with their relative positions
(define (list-snd snd lop ltime)
  (cond [(empty? lop) empty]
        [else (cons (cons snd (cons (modulo (posin-pos (first lop)) ltime) empty))
                    (list-snd snd (rest lop) ltime))]))

;list-of-loops loop-time->list-of-lists
;converts a list of loops to a list of lists
;each list inside the main list contains a sound and the position of that sound.
(define (conv-list lol ltime)
  (cond [(empty? lol) empty]
        [else 
         (if (empty? (loop-posns (first lol)))
             (conv-list (rest lol) ltime)            
             (append (list-snd (loop-snd (first lol)) (loop-posns (first lol)) ltime) (conv-list (rest lol) ltime))                 
              )]))

;list-of-loops->boolean
;returns true if there are any positions in any loop, else false
(define (has-posns lol)
  (cond [(empty? lol) false]
        [else (if (not (empty? (loop-posns (first lol))))
                  true
                  (has-posns (rest lol)))]))

;list file loop-time->list
;takes the previously defined list of rsounds and positions and converts it to one single rsound
;then saves that rsound to loop1.wav
(define (save-sound l file ltime)
  (if (equal? file false) 
      l
      (if (equal? (has-posns l) false)
          l
          (both (rs-write (assemble (conv-list l ltime)) file) l))))


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
                            start-loop-time
                            (make-pstream))
          [to-draw draw-world]
          [on-key key-handler]
          [on-tick tock]
          [on-release release]
          [on-mouse mouse-handler])
