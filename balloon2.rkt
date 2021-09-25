#lang racket/base
(require pict
         racket/draw
         racket/class
         racket/math)

(provide pin-balloon
         balloon-note
         current-balloon-color
         current-balloon-x-margin
         current-balloon-y-margin
         spike->dx
         spike->dy)

(define current-balloon-color (make-parameter (make-color 255 255 170)))
(define current-balloon-x-margin (make-parameter 10))
(define current-balloon-y-margin (make-parameter 10))

(define (pin-balloon p
                     at1 at2
                     content
                     #:spike spike
                     #:margin [margin #f]
                     #:x-margin [x-margin (or margin
                                              (current-balloon-x-margin))]
                     #:y-margin [y-margin (or margin
                                              (current-balloon-y-margin))]
                     #:corner-radius [corner-radius 10]
                     #:spike-radius [spike-radius corner-radius]
                     #:dx [dx (spike->dx spike)]
                     #:dy [dy (spike->dy spike)]
                     #:sprout [sprout 0.5] ; applies to n, s, e, w
                     #:thought? [thought? #f]
                     #:color [color (current-balloon-color)]
                     #:line-color [line-color #f]
                     #:line-width [line-width 0]
                     #:result [return (lambda (p balloon) p)])
  (define w (+ (pict-width content) (* 2 x-margin)))
  (define h (+ (pict-height content) (* 2 y-margin)))
  (define rx corner-radius)
  (define ry corner-radius)
  (define srx spike-radius)
  (define sry spike-radius)
  (define (pict*? p) (or (pict? p) (and (list? p) (andmap pict? p))))
  (define-values (x y)
    (cond
      [(and (pict*? at1) (procedure? at2))
       (at2 p at1)]
      [(and (pict*? at2) (procedure? at1))
       (at1 p at2)]
      [(and (real? at1) (real? at2))
       (values at1 at2)]
      [else
       (raise-arguments-error 'pin-balloon
                              "invalid locator combination"
                              "first locator arg" at1
                              "second locator arg" at2)]))
  (define sx (case spike
               [(s n) (* w sprout)]
               [else (/ w 2)]))
  (define sy (case spike
               [(e w) (* h sprout)]
               [else (/ h 2)]))
  (define bx
    (- (case spike
         [(w nw sw) x]
         [(s n) (- x sx)]
         [else (- x w)])
       dx))
  (define by
    (- (case spike
         [(n nw ne) y]
         [(e w) (- y sy)]
         [else (- y h)])
       dy))
  (define pen (if line-color
                  (make-pen #:color line-color #:width line-width)
                  (make-pen #:style 'transparent)))
  (define brush (if color
                    (make-brush #:color color)
                    (make-brush #:style 'transparent)))
  (define nw-rx (case spike [(nw) srx] [else rx]))
  (define nw-ry (case spike [(nw) sry] [else ry]))
  (define ne-rx (case spike [(ne) srx] [else rx]))
  (define se-ry (case spike [(se) sry] [else ry]))
  (define sw-rx (case spike [(sw) srx] [else rx]))
  (define balloon-path
    (cond
      [(not thought?)
       (let ([p (new dc-path%)])
         (send p move-to nw-rx 0)
         (case spike
           [(n) (send p line-to (- sx srx) 0)
                (send p line-to (+ sx dx) dy)
                (send p line-to (+ sx srx) 0)
                (send p line-to (- w ne-rx) 0)]
           [else (send p line-to (- w ne-rx) 0)])
         (case spike
           [(ne) (send p line-to (+ w dx) dy)
                 (send p line-to w sry)]
           [else (send p arc (- w (* 2 rx)) 0 (* 2 rx) (* 2 ry) (* 1/2 pi) 0 #f)])
         (case spike
           [(e) (send p line-to w (- sy sry))
                (send p line-to (+ w dx) (+ sy dy))
                (send p line-to w (+ sy sry))
                (send p line-to w (- h se-ry))]
           [else (send p line-to w (- h se-ry))])
         (case spike
           [(se) (send p line-to (+ w dx) (+ h dy))
                 (send p line-to (- w srx) h)]
           [else (send p arc (- w (* 2 rx)) (- h (* 2 ry)) (* 2 rx) (* 2 ry) 0 (* -1/2 pi) #f)])
         (case spike
           [(s) (send p line-to (+ sx srx) h)
                (send p line-to (+ sx dx) (+ h dy))
                (send p line-to (- sx srx) h)
                (send p line-to sw-rx h)]
           [else (send p line-to sw-rx h)])
         (case spike
           [(sw) (send p line-to dx (+ h dy))
                 (send p line-to 0 (- h sry))]
           [else (send p arc 0 (- h (* 2 ry)) (* 2 rx) (* 2 ry) (* -1/2 pi) (- pi) #f)])
         (case spike
           [(w) (send p line-to 0 (+ sy sry))
                (send p line-to dx (+ sy dy))
                (send p line-to 0 (- sy sry))
                (send p line-to 0 nw-ry)]
           [else (send p line-to 0 nw-ry)])
         (case spike
           [(nw) (send p line-to dx dy)]
           [else (send p arc 0 0 (* 2 rx) (* 2 ry) (- pi) (* 1/2 pi) #f)])
         (send p close)
         p)]
      [else
       (let ([p (new dc-path%)])
         (send p move-to (* 2/3 rx) (* 1/3 h))
         (send p curve-to
               0 0
               (* 1/8 w) 0
               (* 1/4 w) (* 2/3 ry))
         (send p curve-to
               (* 3/10 w) (* -1/2 ry)
               (* 3/8 w) (* -1/2 ry)
               (* 11/20 w) (* 2/3 ry))
         (send p curve-to
               (* 5/8 w) (* -1/2 ry)
               (* 11/16 w) (* -1/2 ry)
               (* 3/4 w) (* 2/3 ry))
         (send p curve-to
               (* 8/10 w) 0
               w 0
               (- w (* 2/3 rx)) (* 1/4 h))
         (send p curve-to
               (+ w (* 1/2 rx)) (* 3/8 h)
               (+ w (* 1/2 rx)) (* 1/2 h)
               (- w (* 2/3 rx)) (* 5/8 h))
         (send p curve-to
               (+ w (* 1/2 rx)) (* 2/3 h)
               (+ w (* 1/2 rx)) (* 7/10 h)
               (- w (* 2/3 rx)) (* 4/5 h))
         (send p curve-to
               w (* 9/10 h)
               w (+ h (* 1/2 ry))
               (* 3/4 w) (- h (* 2/3 ry)))
         (send p curve-to
               (* 4/10 w) (+ h (* 1/2 ry))
               (* 1/2 w) (+ h (* 1/2 ry))
               (* 1/4 w) (- h (* 2/3 ry)))
         (send p curve-to
               (* 1/8 w) (+ h (* 1/2 ry))
               0 (+ h (* 1/2 ry))
               (* 2/3 rx) (* 5/6 h))
         (send p curve-to
               (* -1/2 rx) (* 3/4 h)
               (* -1/2 rx) (* 3/4 h)
               (* 2/3 rx) (* 1/2 h))
         (send p curve-to
               (* -1/2 rx) (* 7/16 h)
               (* -1/2 rx) (* 7/16 h)
               (* 2/3 rx) (* 1/3 h))
         (send p close)
         (case spike
           [(s sw se)
            (define sy (/ dy 3))
            (define sx (max sy srx))
            (case spike
              [(s)
               (send p ellipse (/ (- w sx) 2) (+ h (/ sy 3)) sx sy)
               (send p ellipse (/ (- w (/ sx 2)) 2) (+ h (* sy 7/4)) (/ sx 2) (/ sy 2))]
              [(sw)
               (send p ellipse (* dx 1/4) (+ h (/ sy 3)) sx sy)
               (send p ellipse (* dx 3/4) (+ h (* sy 7/4)) (/ sx 2) (/ sy 2))]
              [(se)
               (send p ellipse (+ (- w sx) (* dx 1/4)) (+ h (/ sy 3)) sx sy)
               (send p ellipse (+ (- w (/ sx 2)) (* dx 3/4)) (+ h (* sy 7/4)) (/ sx 2) (/ sy 2))])]
           [else (error "not ready")])
         p)]))
  (define balloon
    (dc (lambda (dc x y)
          (define p (send dc get-pen))
          (define b (send dc get-brush))
          (send dc set-pen pen)
          (send dc set-brush brush)
          (send dc draw-path balloon-path x y)
          (send dc set-pen p)
          (send dc set-brush b))
        w h))
  (define balloon+content
    (pin-over balloon
              x-margin y-margin
              content))
  (return (pin-over p
                    bx by
                    balloon+content)
          balloon+content))

(define balloon-note
  (make-keyword-procedure
   (lambda (kws kw-args content)
     (define-values (more-kws more-kw-args)
       (let loop ([kws kws] [kw-args kw-args])
         (cond
           [(null? kws) (values '(#:spike) '(#f))]
           [(eq? (car kws) '#:spike) (values kws kw-args)]
           [(keyword<? '#:spike (car kws))
            (values (cons '#:spike kws)
                    (cons #f kw-args))]
           [else
            (define-values (new-kws new-kw-args) (loop (cdr kws) (cdr kw-args)))
            (values (cons (car kws) new-kws) (cons (car kw-args) new-kw-args))])))
     (define p (blank))
     (keyword-apply pin-balloon
                    more-kws more-kw-args
                    p p cc-find content
                    null
                    #:result (lambda (p balloon) balloon)))))

(define (spike->dx spike)
  (case spike
    [(sw w nw) -20]
    [(se e ne) 20]
    [else 0]))

(define (spike->dy spike)
  (case spike
    [(sw s se) 20]
    [(nw n ne) -20]
    [else 0]))

(module+ main
  (require slideshow
           slideshow/code)
  (define square (filled-rectangle 100 100))
  (slide
   (pin-balloon (code (+ 1 #,square))
                square cb-find
                #:spike 'n
                #:sprout 0.2 ; 0 to 1 to determine x-position
                (para #:fill? #f "There's the square")
                #:color "gold"))
  (slide
   (pin-balloon square
                square ct-find
                (vc-append
                 (current-line-sep)
                 (t "To be or not to be —")
                 (t "that is the question"))
                #:margin 32
                #:dy 50
                #:spike 's
                #:color "gainsboro"
                #:thought? #t)))
