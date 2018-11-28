#lang racket
(require racket/draw)
(require racket/gui/base)
(require racket/cmdline)
(require "samples-buffer.rkt")

; ------------------------------------ APP GLOBALS ------------------------------------

; CONSTANTS
(define (APP-NAME)      "stdinoscope")
(define (SYMBOL-STDIN)  "-")
(define COLOR-BG (send the-color-database find-color "white"))
(define PM-DEBUG #f)
(define TIMER-REFRESH-MSEC 100)

; CONTROL MODIFIERS
(define THREAD-FILECHANGE #f)
(define TIMER-REFRESH     #f)

(define PM-X-DIV     (make-parameter 7))
(define PM-Y-DIV     (make-parameter 5))
(define PM-LOG-SCALE (make-parameter #f))
(define PM-AXES-FONT (make-parameter #f))

#|
Features
  X Sample Count
  X X-Divisions
  X Y-Divisions
  X Log Scale
  X Axes Font
  X Tuples Input
  X Terminal Color Legend
  X History / Samples Scroller
  ? Ctrl-C Handler
  - Keep collecting on pause?
  - Save/Load Samples
  - Regexp Separator?
  - Horizontal Dotted Scale
  - remember x, y, width, height from registry/conf file
                      [label "Example"]
                      [width 1024]
                      [height 700]
                      [x -1000]
                      [y 100]))
  X Button Bar
     X Pause
     X Increase/Reduce Samples
     X Save as PDF
     X pause/resume input on print
     - Print
     X about screen / licensing
  - log scale
  - Mouse-over (x, y) reporting
  - interpolate values for smooth animation?
  - pre-create pens  

NOTES:
   - Use `cmd` instead of power shell in Windows to preserve UTF-8 (instead of UTF-16)
   - Output buffering of input command matters (unix util / sed options to disable)
   - If post-processing output with `sed`, use the `-u` (unbuffered) flag to prevent output buffering

|#

; ------------------------------------ PSEUDO-PARAMETERS ------------------------------------

(define PM-PIPE
  (let ([PIPE-NAME #f])
    (lambda PARAM
      (cond
        ; return pipe name
        [(null? PARAM) PIPE-NAME]
        ; change pipe name
        [else
         (let ([pipename-new (first PARAM)])
           (set! PIPE-NAME pipename-new)
           (printf "     PIPE: ~a~n" pipename-new)
           )])
      )))

(define SCOPE-BUFFER (new samples-buf%))

(define PM-SAMPLES
  (let ([SAMPLE-COUNT 0])
    (lambda PARAM
      (cond
        ; return sample count
        [(null? PARAM) SAMPLE-COUNT]
        ; set sample display count
        [(positive? (first PARAM))
         (let ([n-samp (max 2 (first PARAM))])
           (set! SAMPLE-COUNT n-samp)
           ;(printf "  SAMPLES: ~a~n" n-samp)
           )])
      )))

; ------------------------------------ COLOR UTILITIES ------------------------------------

(define (csv-lower-split-trim txt-csv)
    (for/list ([ITM (string-split (string-downcase txt-csv) ",")]) (string-trim ITM)))

; create color% object from parsed hex or color-name string
(define (to-color txt-color)
  (let ([lst-match (regexp-match #px"#([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})" txt-color)]
        [db-color (send the-color-database find-color txt-color)])
    (cond
      [(and (list? lst-match) (= 4 (length lst-match)))
       (make-object color%
         (string->number (list-ref lst-match 1) 16)
         (string->number (list-ref lst-match 2) 16)
         (string->number (list-ref lst-match 3) 16))]
      [(not (false? db-color)) db-color]
      [else #f])))

; transform command-line list of colors into sanitized list of color% objects
(define (csv-to-colors txt-csv)
  ;(print (csv-lower-split-trim txt-csv))
  (for/list ([txt-color (map to-color (csv-lower-split-trim txt-csv))]
             #:when (not (false? txt-color)))
    txt-color))

; @function: validates positive whole numbers for x/y divisions
(define (int-param val fn)
  (let ([num (cond [(string? val) (string->number val)]
                   [(integer? val) val]
                   [else #f])])
    (cond [(and (integer? num) (> num 0)) (fn num)])))

(define (TERM-COLOR-INFO color-name)
  (let* ([obj-col (send the-color-database find-color color-name)]
         [red (send obj-col red)]
         [grn (send obj-col green)]
         [blu (send obj-col blue)]
         [blocks (build-string 7 (lambda (i) #\u2588))])

    (printf "\e[~a;2;~a;~a;~am~a\e[0m ~a\n" 38 red grn blu blocks (string-upcase color-name))
            ))

(define (HELP-TERM-COLORS)
  (for ([color-name (in-list (send the-color-database get-names))])
    (TERM-COLOR-INFO color-name)))

; ------------------------------------ COMMAND LINE PARSING + HELP ------------------------------------

(define TEMPPARAM-COLORS  (make-parameter ""))
(define TEMPPARAM-SAMPLES (make-parameter 200))

(define CLO
  (command-line
   #:program (APP-NAME)
   #:once-each
   [("-C") datum-colors
           "\n\tcomma-separated list of colors, one for each sample\n\t(color name or RGB hex, in order of specification)"
           (TEMPPARAM-COLORS datum-colors)]
   [("--color-names")
    "\n\tlist named colors, then exit\n\t(your terminal needs true-color escape codes for this to work)"
    ((lambda ()
      (HELP-TERM-COLORS)
      (exit)))]
   [("-I") input-device
           "\n\tSTDIN if set to '-' or left unspecified"
           (PM-PIPE input-device)]
   [("-S") sample-count
           "\n\tnumber of samples to display"
           (int-param sample-count TEMPPARAM-SAMPLES)]
   [("--xdiv") x-div
               "\n\tnumber of x-divisions"
               (int-param x-div PM-X-DIV)]
   [("--ydiv") y-div
               "\n\tnumber of y-divisions"
               (int-param y-div PM-Y-DIV)]
   [("--font") font-face
               "\n\taxes font"
               (PM-AXES-FONT font-face)]
   [("-L" "--log-scale")
    "\n\tuse logarithmic y-scale (not yet implemented)"
    (PM-LOG-SCALE #t)]
   #:args
   () (void)))

; initialize pipe defaults
(cond
  [(false? (PM-PIPE))
   (PM-PIPE
    (cond
      ; use testing pipes when in debug mode
      [PM-DEBUG
       (printf "PIPE DEBUG MODE~n")
       (if (eq? 'windows (system-type 'os)) "\\\\.\\pipe\\vm-dev" "/home/jstewart/inoscope")]
      ; otherwise, default to STDIN
      [else
       (SYMBOL-STDIN)])
    )])

; DEFINE COLORS
(define PM-COLORS
  (let ([lst-colors   '()]
        [lst-pen-line '()]
        [lst-pen-pt   '()])
    (lambda PARAM
      (cond
        ; no-parameter: get color object lists
        [(null? PARAM) (values lst-colors lst-pen-line lst-pen-pt)]
        [else
         (let ([opc (car PARAM)])
           ; list?: set colors list
           (cond [(list? opc)
                  (set! lst-colors opc)
                  (set! lst-pen-line
                        (for/list ([col (in-list opc)])
                          (new pen% [color col] [width 2.0] [style 'solid])))
                  (set! lst-pen-pt
                        (for/list ([col (in-list opc)])
                          (new pen% [color col] [width 5.0] [style 'solid])))]
                 ))])
      )))

(let* ([lst-colors (csv-to-colors (TEMPPARAM-COLORS))]
       [lst-len (length lst-colors)])
  (cond
    [(> lst-len 0) (PM-COLORS lst-colors)]
    [else          (PM-COLORS (csv-to-colors "crimson, purple, blue"))]))

; SET SAMPLE RING-BUFFER SIZE
; NOTE: COLORS MUST BE DEFINED FIRST
(cond
  [(> (TEMPPARAM-SAMPLES) 2) (PM-SAMPLES (TEMPPARAM-SAMPLES))]
  [else (PM-SAMPLES 200)])

(printf "    X-DIV: ~a~n" (PM-X-DIV))
(printf "    Y-DIV: ~a~n" (PM-Y-DIV))
(printf "LOG SCALE: ~a~n" (PM-LOG-SCALE))
(printf "AXES FONT: ~a~n" (PM-AXES-FONT))

; AXES FONT
(define AXES-FONT
  (cond
    [(and (string? (PM-AXES-FONT)) (> (string-length (PM-AXES-FONT)) 0))
     (make-object font% 9 (PM-AXES-FONT) 'default 'normal)]
    [else (make-object font% 9 'modern)]))

#|
 @function: calculate chart metrics from samples buffer and client-area dimensions
    n-samp: number of samples
  samp-min: lowest sample
  samp-max: highest sample
samp-delta: difference between highest and lowest sample
        sx: sample-to-device-pixels x-scale
        sy: sample-to-device-pixels y-scale
(lambda (n-samp samp) ...): converts sample and sample-index into device coordinates
|#
(define (chart-metrics canvas-chart dx dy)
  (let*-values ([(n-samp n-samp-log)  (values (PM-SAMPLES) (send SCOPE-BUFFER size?))]
                [(scroll-pos)         (send canvas-chart chart-scrollbar-update)]
                [(ix-first)           (if (< scroll-pos 0) (max (- n-samp-log n-samp) 0) scroll-pos)]
                [(samp-min samp-max)  (send SCOPE-BUFFER minmax? ix-first n-samp)]
                [(fx fy)              (values (->my-fl dx) (->my-fl dy))]
                [(ONE)                (->my-fl 1)])
    ; expand y-axis to [-1, 1] if sample delta is zero
    (cond [(= samp-min samp-max)
           (set! samp-min (- samp-min ONE))
           (set! samp-max (+ samp-max ONE))])
    ; continue calculation
    (let* ([samp-delta (- samp-max samp-min)]
           [sx (/ fx (->my-fl (- n-samp 1)))]
           [sy (/ fy samp-delta)])
      (list n-samp samp-min samp-max samp-delta sx sy ix-first
            (lambda (ix sample)
              (values (* sx (->my-fl ix)) (* sy (- samp-max sample))))
            )
      )))


; @function: draw chart axes
(define (draw-axes dc dx dy lstMetrics pad)
  (send dc set-pen "black" 1 'solid)
  (let*-values
      ([(nsamp smin smax sdelta sx sy ix-first fnTx) (apply values lstMetrics)]
       [(samp-last)    (- nsamp 1)]
       [(pad-1 pad-2)  (values (* pad -1.5) (* pad -0.5))]
       [(inc-x inc-y)  (values (/ samp-last (PM-X-DIV)) (/ sdelta (PM-Y-DIV)))])

    ; X-AXIS
    (for ([ix (in-range 0 (+ samp-last (* 0.5 inc-x)) inc-x)])
      (let-values ([(px py) (fnTx ix (->my-fl smin))])
        (send dc draw-line px (- dy pad-1) px (- dy pad-2))
        (send dc draw-text (real->decimal-string (+ ix ix-first) 2) (+ px (* 0.25 pad)) (+ dy (* 1.35 pad)))
        ))
    ; Y-AXIS
    (for ([iy (in-range smin (+ smax (* 0.5 inc-y)) inc-y)])
      (let-values ([(px py) (fnTx 0 (->my-fl iy))])
        (send dc draw-line (+ px pad-1) py (+ px pad-2) py)
        (send dc draw-text (real->decimal-string iy 2) (- px (* 1.75 pad)) (+ py (* 0.2 pad)))
        ))
    ))

; @function: draw chart samples
(define (draw-chart dc dx dy lstMetrics)
  (let*-values ([(nsamp smin smax sdelta sx sy ix-first fnTx) (apply values lstMetrics)]
                [(NO-SAMPLE) (my-nosample)]
                [(_ lst-pnline lst-pnpt) (PM-COLORS)]
                [(n-colors)  (length lst-pnline)]
                [(samp-val)   NO-SAMPLE]
                [(vec-xprev) (make-vector n-colors NO-SAMPLE)]
                [(vec-yprev) (make-vector n-colors NO-SAMPLE)]
                [(set-last-point!)
                 (lambda (ix x y)
                   (vector-set! vec-xprev ix x)
                   (vector-set! vec-yprev ix y))])

    ; sample iterator
    (define (fn-sample-iterate ix-screen ix-sample lst-tuple)

      ; walk tuples in sample
      (for ([pen-line      (in-list lst-pnline)]
            [pen-point     (in-list lst-pnpt)]
            [ix-tuple      (in-naturals)])

        ; handle no-sample blanks in tuple lists
        (cond
          [(null? lst-tuple)
           (set! samp-val NO-SAMPLE)]
          [else
           (set! samp-val  (car lst-tuple))
           (set! lst-tuple (cdr lst-tuple))])

        ; Transform sample index/value to screen coordinates
        (cond
          [(nan? samp-val) (set-last-point! ix-tuple NO-SAMPLE NO-SAMPLE)]
          [else
           (let-values ([(px py)   (fnTx ix-screen samp-val)]
                        [(x-prev)  (vector-ref vec-xprev ix-tuple)]
                        [(y-prev)  (vector-ref vec-yprev ix-tuple)])
             (cond
               ; plot current point if no previous point
               [(or (nan? x-prev) (nan? y-prev))
                (send dc set-pen pen-point)
                (send dc draw-point px py)]
               ; draw line from previous point
               [else
                (send dc set-pen pen-line)
                (send dc draw-line x-prev y-prev px py)])

             (set-last-point! ix-tuple px py))
           ])
        ))

    ; walk samples
    (send SCOPE-BUFFER walk ix-first nsamp fn-sample-iterate)
    ))


; @function: set scale/transform defaults, repaint screen
(define (draw-window canvas-chart dc)
  (let-values ([(dx dy) (send dc get-size)])

    (send dc set-brush COLOR-BG 'transparent)
    (send dc set-smoothing 'aligned)
    (send dc set-text-mode 'transparent)
    (send dc set-font AXES-FONT)
    (send dc set-initial-matrix (vector 1.0 0.0 0.0 1.0 0.0 0.0))
    (send dc set-pen "blue" 1.0 'solid)

    ;TODO: constant-ize border margin (30 px)
    (send
     SCOPE-BUFFER do-sem
     (lambda ()
       (let* ([pxo 30.0]
              [scl-x (/ (- dx (* 2.0 pxo)) dx)]
              [scl-y (/ (- dy (* 2.0 pxo)) dy)]
              [lstMetrics (chart-metrics canvas-chart dx dy)])
         (send dc translate pxo pxo)
         (send dc scale scl-x scl-y)
         (send dc draw-rectangle 0.0 0.0 dx dy)

         (send dc translate pxo pxo)
         (send dc scale scl-x scl-y)

         (draw-axes dc dx dy lstMetrics pxo)
         (draw-chart dc dx dy lstMetrics)
         )))
    ))

; OPEN INPUT PIPE
(define G-INPUT-PORT
  (with-handlers ([exn:fail? (lambda (exn) 'pipeFail)])
    (cond
      ; stdin mode
      [(equal? (PM-PIPE) (SYMBOL-STDIN))
       (current-input-port)]
      ; open specified file name if string
      [else
       (open-input-file (PM-PIPE) #:mode 'binary)])))

(define (APP-PAUSE pause)
  (cond
    [(eq? pause #t)
     (if (false? TIMER-REFRESH) (void) (send TIMER-REFRESH stop))
     (if (false? THREAD-FILECHANGE) (void) (thread-suspend THREAD-FILECHANGE))]
    [else
     (if (false? TIMER-REFRESH) (void) (send TIMER-REFRESH start TIMER-REFRESH-MSEC))
     (if (false? THREAD-FILECHANGE) (void) (thread-resume THREAD-FILECHANGE))]
    ))

(define (SAVE-PDF canvas-chart)
  (APP-PAUSE #t)
  (let ([dc-print (new pdf-dc%
                       [interactive #t]
                       [parent #f]
                       [use-paper-bbox #t])])
    (cond [(send dc-print ok?)
           (send dc-print start-doc "printing")
           (send dc-print start-page)
           (draw-window canvas-chart dc-print)
           (send dc-print end-page)
           (send dc-print end-doc)]))
  (APP-PAUSE #f))

; Derive a new canvas (a drawing window) class to handle events
(define ovr-canvas%
  (class canvas%
#|
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (println "Canvas mouse"))
|#
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (case (send event get-key-code)
        [(#\p) (SAVE-PDF this)]))

    (define ix-samples-start -1)
    
    (define/public (chart-scrollbar-update . params)
      (cond
        [(null? params) ix-samples-start]
        [else
         (let ([p-enable (car params)]) 
           (cond
             [(and (integer? p-enable) (>= p-enable 0) (not (= p-enable ix-samples-start)))
              ;(printf "S: ~a~n" p-enable)
              (set! ix-samples-start p-enable)
              (send this refresh-now)]
             [(eq? p-enable #f)
              (send this show-scrollbars #f #f)
              (set! ix-samples-start -1)]        
             [(eq? p-enable #t)
              (let* ([n-hist (send SCOPE-BUFFER size?)]
                     [h-len  (max (- n-hist (PM-SAMPLES)) 0)])
                (cond [(> h-len 0)
                       (send this init-manual-scrollbars
                             h-len #f (PM-SAMPLES) 1 h-len 1)
                       (send this show-scrollbars  #t #f)
                       (set! ix-samples-start h-len)
                       ])
                )])
           )]
        ))
    
    (define/override (on-scroll evt)
      (chart-scrollbar-update (send evt get-position)))
    
    ; Call the superclass init, passing on all init args
    (super-new)))


; @function: initialize file-reader thread and drawing timer
(define (canvas-init wnd-parent)

  ;(define THREAD-ID (make-parameter 1))

  ; @canvas: main window drawing surface
  (let* ([SEM-REDRAW  (make-semaphore 1)]
         [SEM-DIRTYCT (make-semaphore 1)]
         [DIRTY-DO    (lambda (fn-action) (call-with-semaphore SEM-DIRTYCT fn-action))]
         [SAMPLES-DIRTY 0]
         [oCanv (new ovr-canvas%
                     [parent wnd-parent]
                     ;[enabled #f]
                     ;[style (list 'no-autoclear)]
                     [style '(hscroll)]
                     [gl-config (new gl-config%)]
                     [paint-callback
                      (lambda (oCanvas oDc)
                        (draw-window oCanvas oDc)
                        (DIRTY-DO (lambda () (set! SAMPLES-DIRTY 0))))]
                     )])
    
    ; prime the semaphore for our usage pattern
    (semaphore-wait SEM-REDRAW)

    (define (MAKE-READER-HANDLER)
      (define (HDL-RECUR)
        (let* ([line-txt (read-line G-INPUT-PORT 'any)])
          (cond
            ; eof read
            [(eof-object? line-txt)
             (cond
               ; pipe eof handling (wait for change)
               [(not (equal? (SYMBOL-STDIN) (PM-PIPE)))
                (let ([evt-eof-end (filesystem-change-evt (PM-PIPE))])
                  (sync evt-eof-end)
                  (print 'eof-end))
                ]

               ; stdin eof handling
               [else (print 'rs)])
             ]
            ; valid input read
            [else

             ; wait for refresh when full display range is dirty
             ; otherwise, update dirty count
             (cond [(>= (+ SAMPLES-DIRTY 1) (PM-SAMPLES))
                    ; NOTE: this callback runs in the primary thread instead of this one
                    (queue-callback
                     (lambda ()
                       ; refresh screen, clear dirty count
                       (send oCanv refresh-now #:flush? #t)
                       (semaphore-post SEM-REDRAW)
                       ))
                    ; wait for screen refresh before enqueuing next sample
                    (semaphore-wait SEM-REDRAW)
                    ])

             ; push sample to buffer
             (send SCOPE-BUFFER push-string line-txt)

             ; increment dirty count
             (DIRTY-DO (lambda () (set! SAMPLES-DIRTY (+ SAMPLES-DIRTY 1))))
             ]))
        (HDL-RECUR)
        )
      HDL-RECUR
      )

    ; @thread: read text from input pipe, transform, notify redraw
    (set! THREAD-FILECHANGE (thread (MAKE-READER-HANDLER)))

    ; @timer/callback: re-paints screen at given interval when dirty
    (set! TIMER-REFRESH
          (new timer%
               [notify-callback
                (lambda ()
                  (if (> SAMPLES-DIRTY 0)
                      ((lambda ()
                         (send oCanv refresh-now #:flush? #t)))
                      #t)
                  )]
               [interval TIMER-REFRESH-MSEC]
               [just-once? #f]))

    (send oCanv set-canvas-background COLOR-BG)

    ; initialize scrollbars
    (send oCanv chart-scrollbar-update #f)
    
    oCanv
    ))

(cond
  ; error message if input file not opened
  [(eq? G-INPUT-PORT 'pipeFail)
    (message-box
     (APP-NAME)
     (format "Failed to open pipe ~s" (PM-PIPE)))
  ]
  ; otherwise, start app
  [else

   ; @event-handler: close input file and kill thread on exit
   (define (app-exit)

     ; stop refresh timer
     (if (false? TIMER-REFRESH) (void) (send TIMER-REFRESH stop))

     ; kill file reader thread
     (if (false? THREAD-FILECHANGE) (void) (kill-thread THREAD-FILECHANGE))

     ; @note: closing STDIN while running from DrRacket closes it
     ;        for the duration of the session--which kind of sucks
     ;(close-input-port G-INPUT-PORT)
     )

   ; Derive a new frame
   (define ovr-frame%
     (class frame%
       (super-new)
       (define/augment (on-close)
         (app-exit))))

   ; @frame: main window
   (define wnd-main-frame
     (new ovr-frame%
          [label (APP-NAME)]
          [width 1024]
          [height 700]
          [x -1000]
          [y 100]))

   (define wnd-panel-buttons
     (new horizontal-pane%
          [parent wnd-main-frame]
          [min-height 50]
          [stretchable-height #f]))

   ; @button: Print PDF
   (define btn-pdf
     (new button%
          [label "PDF"]
          [horiz-margin 10]
          [parent wnd-panel-buttons]
          [callback (lambda (b e) (SAVE-PDF canvas-chart))]))

   ; @text-field: Change Displayed Sample Count
   (define ctrl-samples
     (new text-field%
          [label "Samples:"]
          [parent wnd-panel-buttons]
          [init-value (number->string (PM-SAMPLES))]
          [min-width 150]
          [stretchable-width #f]
          [callback
           (lambda (ctl evt)
             (cond
               ; update sample count on enter
               [(eq? (send evt get-event-type) 'text-field-enter)
                (let* ([sz-samples (send ctrl-samples get-value)]
                       [n-samples (string->number sz-samples)])
                  (cond
                    [(and (integer? n-samples) (>= n-samples 2))
                     (PM-SAMPLES n-samples)]
                    [else
                     (send ctrl-samples set-value (number->string (PM-SAMPLES)))])
                  (send canvas-chart refresh-now #:flush? #t))
                ])                
             )]
          ))

   ; @dialog: about stdinoscope
   (define (dlg-about)

     (define (unsaved-warning-mixin %)
       (class %
         (super-new [min-height 400] [min-width 700])))

     (message-box
      "About"
      (string-join
       (list
        "Stdinoscope"
        "Copyright 2018 Jason Stewart"
        "Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the \"Software\"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:"
        "The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software."
        "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.")
       "\n\n")
      wnd-main-frame
      '(ok no-icon)
      #:dialog-mixin unsaved-warning-mixin)       
     )

   ; @button: Pause
   (define btn-pause
     (new button%
          [label "Pause"]
          [horiz-margin 10]
          [parent wnd-panel-buttons]
          [callback
           (lambda (btn evt)
             (cond
               [(string=? "Pause" (send btn get-label))
                (APP-PAUSE #t)
                (send canvas-chart chart-scrollbar-update #t)
                (send btn set-label "Resume")]
               [else
                (APP-PAUSE #f)
                (send canvas-chart chart-scrollbar-update #f)                
                (send btn set-label "Pause")])
             )]
          ))
   
   (define btn-about
     (new button%
          [label "About"]
          [horiz-margin 10]
          [parent wnd-panel-buttons]
          [callback (lambda (ctl-btn evt-btn) (dlg-about))]))
      
   (define wnd-panel-chart
     (new pane%
          [parent wnd-main-frame]))

   (define canvas-chart (canvas-init wnd-panel-chart))
      
   ; Show the frame by calling its show method
   (send wnd-main-frame show #t)
    
   ; handle Ctrl-C
#|
; TODO: this was blocking the repl, find a fix
   (with-handlers ([exn:break? (lambda (exn) (app-exit) (exit 0))])
     (yield 'wait)
     (void) ; to prevent echo of #t on exit
     )
|#
   
])
