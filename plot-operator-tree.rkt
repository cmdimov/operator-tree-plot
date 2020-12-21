#lang racket
(require pict/tree-layout)
(require 2htdp/image)
(require pict-abbrevs)

(define STARTING-VALUE 'COOP)
(define FONT-SIZE 16)

;; operator -> symbol
;; extract operator name
(define (operator-name op)
  (first op))

;; operator -> operator-body
;; extract operator body
(define (operator-body op)
  (rest op))

;; operator-body slot -> symbol
;; finds value slot
(define (operator-value op-body slot)
  (cond
    ((empty? op-body) '())
    ((equal? (first op-body) slot) (first (rest op-body)))
    (else (operator-value (rest (rest op-body)) slot))))

;; operator-body -> symbol
;; finds value of any post slot (i.e., post, success, fail)
(define (operator-post op-body)
  (cond
    ((member 'post op-body) (list (operator-value op-body 'post)))
    ((member 'success op-body) (list (operator-value op-body 'success)
                                     (operator-value op-body 'fail)))))

;; Operator-body -> Image
;; Draws the body of an operator
(define (draw-operator-body opb)
  (let ((first-two-image
         (beside/align "bottom"
                       (text/font (symbol->string (first opb)) FONT-SIZE "black" #f 'default 'italic 'normal #f)
                       (text " " FONT-SIZE "black")
                       (text (symbol->string (second opb)) FONT-SIZE "black"))))
    (if (= (length opb) 2)
        first-two-image
        (above/align "left"
                     first-two-image
                     (draw-operator-body (rest (rest opb)))))))

;; Operator -> Image
;; draws an operator
(define (draw-operator op)
  (let ((operator-image
         (above/align "left"
               (text/font (symbol->string (operator-name op)) FONT-SIZE "black" #f 'default 'normal 'bold #f)
               (draw-operator-body (operator-body op)))))
    (overlay operator-image
             (rectangle (image-width operator-image)
                        (image-height operator-image)
                        "solid"
                        "white"))))

;; List-of-operators start-operator-> Operator-tree
;; Nests operators into a tree representation
(define (loo->tree loo start-op)
  (let* ((start-pre (operator-value (operator-body start-op) 'pre))
         (start-post (operator-post (operator-body start-op)))
         (matching-ops (filter (lambda (op) (member (operator-value (operator-body op) 'pre) start-post)) loo))
         (matching-ops2 (filter (lambda (op) (not (equal? STARTING-VALUE (operator-value (operator-body op) 'pre)))) matching-ops)))
    (if (empty? matching-ops)
        (list start-op)
        (append (list start-op) (map (lambda (op) (loo->tree loo op)) matching-ops)))))

;; A predicate that checks if list is not nested
(define (non-nested-list? lst)
  (and (list? lst) (not (list? (car lst)))))

;; Operator-tree -> Image
;; Draws operator tree
(define (draw tree)
  (define (viz tree)
    (cond
      ((null? tree) #f)
      ((non-nested-list? tree)
       (tree-layout #:pict (draw-operator tree)))
      ((non-nested-list? (car tree))
       (apply tree-layout (map viz (cdr tree)) #:pict (draw-operator (car tree))))))
  (if (null? tree)
      #f
      (naive-layered #:x-spacing 15 (viz tree))))

;; List of operators from model
(define operators
  '((starter
     isa operator
     pre COOP
     action test-object
     arg1 fortress-alive
     success fortress
     fail nofortress)

    (fortress-present
     isa operator
     pre fortress
     action  test-location
     arg1 inside-hex
     success inside
     fail not-inside)

    (fortress-abscent
     isa operator
     pre nofortress
     action  test-location
     arg1 outside-hex
     success slowspeed?
     fail self-not-outside)

  (speed-slow?
     isa operator
     pre slowspeed?
     action test-dimension
     arg1 slowspeed
     success slow-down
     fail COOP!)

    (inside-hex
     isa operator
     pre inside
     action  test-object
     arg1 fortress-target-self
     success self-target
     fail other-target)

    ;; this shouldn't be in a separate operator
    (not-inside-hex
     isa operator
     pre not-inside
     action  test-future-location
     arg1 inside-hex
     success fortress!
     fail move-inside)

    ;;this shouldn't be in a separate operator
    (not-outside-hex
     isa operator
     pre self-not-outside
     action  test-future-location
     arg1 outside-hex
     success nofortress!
     fail move-outside)

    ;; this shouldn't be an object
    (bait-me
     isa operator
     pre self-target
     action test-object
     arg1 slowspeed-bait
     success speed-slow-bait
     fail speed-fast-bait
     )

    ;; this shouldn't be in a separate operator
    (slow-bait
     isa operator
     pre speed-slow-bait
     action  test-future-location
     arg1 outside-hex
     success slow-down
     fail COOP!)

    (shooter
     isa operator
     pre other-target
     action test-location
     arg1 behind-fortress
     success fortress-vulnerable?
     fail not-behind-fortress)

    (behind-fortress
     isa operator
     pre fortress-vulnerable?
     action test-object
     arg1 fortress-vulnerable
     success shooting
     fail not-behind-fortress)

    ;; shouldn't be a separate operator
    (will-be-behind-fortress
     isa operator
     pre not-behind-fortress
     action test-future-location
     arg1 behind-fortress
     success will-be-behind
     fail move-behind)

    (anticipatory-aim
     isa operator
     pre will-be-behind
     action turn-to-point
     arg1 future-aim-angle
     post COOP!)
     

    (not-behind-fortress
     isa operator
     pre move-behind
     action test-dimension
     arg1 behind-angle
     success ready-to-thrust
     fail turn-behind)

    (turn-to-face-behind
     isa operator
     pre turn-behind
     action calculate-turn-angle
     arg1 behind-angle
     post move-behind!)

    (not-heading-inside
     isa operator
     pre  move-inside
     action test-dimension
     arg1 inside-angle
     success ready-to-thrust
     fail not-facing-inside)
    
    (turn-to-face-inside
     isa operator
     pre  not-facing-inside
     action calculate-turn-angle
     arg1 inside-angle
     post move-inside!)
    
    (not-heading-outside
     isa operator
     pre move-outside
     action test-dimension
     arg1 outside-angle
     success ready-to-thrust
     fail not-facing-outside)

    (turn-to-face-outside
     isa operator
     pre not-facing-outside
     action calculate-turn-angle
     arg1 outside-angle
     post move-outside!)

    (thrust-toward
     isa operator
     pre ready-to-thrust
     action thrust-to-move
     post done)

    (slowdown-bait
     isa operator
     pre speed-fast-bait
     action test-dimension
     arg1 bait-angle
     success make-slow-turn
     fail not-bait-angle)

    (turn-to-bait-trajectory
     isa operator
     pre not-bait-angle
     action calculate-turn-angle-slow
     arg1 bait-angle
     post self-target!)

    (thrust-gently-bait
     isa operator
     pre make-slow-turn
     action thrust-gently
     post done)

    (slowdown-angle
     isa operator
     pre slow-down
     action test-angle
     arg1 opposite-angle
     success thrust-to-stop
     fail not-facing-opposite)

    (slowdown
     isa operator
     pre not-facing-opposite
     action turn-to-point
     arg1 opposite-angle
     post slow-down!)

    (stop
     isa operator
     pre thrust-to-stop
     action thrust-stop
     post done)

   (thrustaction
     isa operator
     pre tapthrust
     action tap-key
     arg1 thrust
     post done)

    (aim
     isa operator
     pre shooting
     action turn-to-point
     arg1 aim-angle
     post fire)

    (shoot
     isa operator
     pre fire
     action tap-key
     arg1 fire
     post done)))

;(save-pict "Operator-tree.png" (draw (loo->tree operators (first operators))) 'png)

;; List of operators from model
(define operators2
  '((starter
     isa operator
     pre COOP
     action test-object
     arg1 fortress-alive
     success fortress
     fail nofortress)

    (fortress-present
     isa operator
     pre fortress
     action  move-to
     arg1 inside-hex
     post inside)

    (fortress-abscent
     isa operator
     pre nofortress
     action  move-to
     arg1 outside-hex
     post outside)

  (speed-slow?
     isa operator
     pre slowspeed?
     action test-dimension
     arg1 slowspeed
     success slow-down
     fail COOP!)

    (inside-hex
     isa operator
     pre inside
     action  test-object
     arg1 fortress-target-self
     success self-target
     fail other-target)

    ;; this shouldn't be an object
    (bait-me
     isa operator
     pre self-target
     action test-object
     arg1 slowspeed-bait
     success speed-slow-bait
     fail speed-fast-bait
     )

    ;; this shouldn't be in a separate operator
    (slow-bait
     isa operator
     pre speed-slow-bait
     action  test-future-location
     arg1 outside-hex
     success slow-down
     fail COOP!)

    (shooter
     isa operator
     pre other-target
     action move-to
     arg1 behind-fortress
     post behind-fortress)

    (behind-fortress
     isa operator
     pre behind-fortress
     action test-object
     arg1 fortress-vulnerable
     success shooting
     fail not-behind-fortress)

    (thrust-toward
     isa operator
     pre ready-to-thrust
     action thrust-to-move
     post done)

    (slowdown-bait
     isa operator
     pre speed-fast-bait
     action test-dimension
     arg1 bait-angle
     success make-slow-turn
     fail not-bait-angle)

    (turn-to-bait-trajectory
     isa operator
     pre not-bait-angle
     action calculate-turn-angle-slow
     arg1 bait-angle
     post self-target!)

    (thrust-gently-bait
     isa operator
     pre make-slow-turn
     action thrust-gently
     post done)

    (slowdown-angle
     isa operator
     pre slow-down
     action test-angle
     arg1 opposite-angle
     success thrust-to-stop
     fail not-facing-opposite)

    (slowdown
     isa operator
     pre not-facing-opposite
     action turn-to-point
     arg1 opposite-angle
     post slow-down!)

    (stop
     isa operator
     pre thrust-to-stop
     action thrust-stop
     post done)

   (thrustaction
     isa operator
     pre tapthrust
     action tap-key
     arg1 thrust
     post done)

    (aim
     isa operator
     pre shooting
     action turn-to-point
     arg1 aim-angle
     post fire)

    (shoot
     isa operator
     pre fire
     action tap-key
     arg1 fire
     post done)))

;(save-pict "Operator-tree2.png" (draw (loo->tree operators2 (first operators2))) 'png)

;; To do:
;; - come up with a general turning mechanism, instead of repeating operators for each target angle
;; - come up with a general anticipatory mechanism, instead of repeating mechanism for each location

(define operators-autoturn
'((starter
     isa operator
     pre  SPACEFORTRESS
     action test-object
     arg1 fortress-alive
     success fortress
     fail nofortress)
    (spacefortress
     isa operator
     pre  fortress
     action  test-dimension
     arg1 time-to-outer
     success delay?
     fail tapthrust)
    (spacefortress1
     isa operator
     pre  nofortress
     action  test-dimension
     arg1 time-to-outer
     success delay?
     fail tapthrust)
    (delay
     isa operator
     pre  delay?
     action  check-delay
     success vulnerability?)
    (vulnerability
     isa operator
     pre  vulnerability?
     action  test-dimension
     arg1 vulnerability
     success kill-it
     fail shoot)
    (kill-it
     isa operator
     pre kill-it
     action double-spacebar
     post done)
    (shoot
     isa operator
     pre shoot
     action spacebar
     post done)
    (thrustaction
     isa operator
     pre tapthrust
     action tap-key
     arg1 thrust
     post done)))

;(save-pict "Operator-tree-autoturn.png" (draw (loo->tree operators-autoturn (first operators-autoturn))) 'png)

(define operators-youturn
'((starter
     isa OPERATOR
     pre  SPACEFORTRESS
     action test-object
     arg1 fortress-alive
     success fortress
     fail nofortress)
    (spacefortress
     isa OPERATOR
     pre  fortress
     action  test-dimension
     arg1 time-to-outer
     success shooting
     fail thrusting)
    (spacefortress1
     isa OPERATOR
     pre  nofortress
     action  test-dimension
     arg1 time-to-outer
     success shooting
     fail thrusting)
    (aim
     isa OPERATOR
     pre  shooting
     action  turn-to-point
     arg1 fortress-angle
     post delay?)
    (delay
     isa OPERATOR
     pre  delay?
     action  check-delay
     success vulnerability?)
    (vulnerability
     isa OPERATOR
     pre  vulnerability?
     action  test-dimension
     arg1 vulnerability
     success kill-it
     fail shoot)
    (kill-it
     isa operator
     pre kill-it
     action double-spacebar
     post done)
    (shoot
     isa operator
     pre shoot
     action spacebar
     post done)
    (judge-speed
     isa OPERATOR
     pre  thrusting
     action  test-dimension
     arg1 speed
     fail speed-up
     success slow-down)
    (speed-up
     isa OPERATOR
     pre  speed-up
     action  increment-angle
     arg1 thrust-angle
     post tapthrust)
    (slow-down
     isa OPERATOR
     pre  slow-down
     action  decrement-angle
     arg1 thrust-angle
     post tapthrust)
    (thrustaction
     isa operator
     pre tapthrust
     action tap-key
     arg1 thrust
     post done)))


;(save-pict "Operator-tree-youturn.png" (draw (loo->tree operators-youturn (first operators-youturn))) 'png)

(define operators-spacetrack
 '((spacetrack
     isa operator
     pre  spacetrack
     action  test-dimension
     arg1 offaim
     success  adjust
     fail  test-speed)
    (turn-to-target
     isa operator
     pre  adjust
     action calculate-turn-angle
     arg1 current-angle
     post tapthrust)
    (speedtest
     isa operator
     pre  test-speed
     action test-dimension
     arg1 ship-speed
     success turn
     fail speedup)
    (speedup
     isa operator
     pre speedup
     action turn-to-point
     arg1 current-angle
     post tapthrust)
    (turn
     isa operator
     pre  turn
     action calculate-turn-angle
     arg1 future-angle
     post maketurn2)
    (thrust-turn
     isa operator
     pre  maketurn2
     action thrust-to-turn
     post done)
    (thrustaction
     isa operator
     pre tapthrust
     action tap-key
     arg1 thrust
     post done)))


(save-pict "Operator-tree-spacetrack.png" (draw (loo->tree operators-spacetrack (first operators-spacetrack))) 'png)