#lang racket

(require net/http-client)
(require json)
(require racket/date)
(require racket/gui/base)

(date-display-format 'iso-8601)

(define (cmd x) (string-trim (with-output-to-string (lambda() (system x)))))

(define (node-a) (cmd "~/bin/terraform output node_a"))
(define (node-b) (cmd "~/bin/terraform output node_b"))
(define (beep) (play-sound "/System/Library/Sounds/Glass.aiff" #f))

(define (ping ip)
  (let*-values (((status headers in)
                 (http-sendrecv ip (format "/api/ping") #:port 9001 #:method "GET"))
                ((res)
                 (port->string in)))
    (unless (string=? res "ok")
      (error (format "node not up (~s)" res)))))

(define (setup-db ip n p bb)
  (http-sendrecv ip (format "/api/debug/setup_db?n=~a&p=~a&bb=~a" n p bb)
                 #:port 9001 #:method "POST")
  'ok)

(define (config-bloom ip bloom-false-probability)
  (http-sendrecv
   ip (format "/api/debug/config?ds=riak_set_a&bloom_false_probability=~a"
              (~r bloom-false-probability #:precision '(= 3)))
   #:port 9001 #:method "POST"))

(define (reconcile ip)
  (let-values (((status headers in)
                (http-sendrecv ip "/api/datasets/riak_set_a/recons" #:port 9001
                               #:method "POST")))
    (read-json in)))

(define (assemble-result bloom-false-probability response)
  (let* ((ds-size (hash-ref response 'ds_size))
         (bloom-size (hash-ref response 'bloom_size))
         (data-size (hash-ref response 'data_size))
         (its (hash-ref response 'its))
         (rx-cnt (hash-ref response 'rx_cnt))
         (tx-cnt (hash-ref response 'tx_cnt))
         (prep-time (hash-ref response 'prep_time))
         (rec-time (hash-ref response 'rec_time))
         (transfer-ratio (exact->inexact (/ (+ bloom-size data-size) ds-size))))
    (format "~a, ~a, ~a, ~a, ~a" (precision bloom-false-probability)
            (precision transfer-ratio) bloom-size its
            (precision (us->s rec-time)))))

(define (us->s us) (exact->inexact (/ us (* 1000 1000))))
(define (ms->s ms) (exact->inexact (/ ms 1000)))
(define (precision x) (~r x #:precision '(= 3)))

(define (test n p bulk (range #f) (samples 3))
  (printf "false-probability, transfer-ratio, bloom-size, its, time~n")
  (let ((a (node-a))
        (b (node-b)))
    (for ((bloom-false-probability (or range (in-range 0.001 0.9 0.1))))
      (for ((tries (in-range 0 samples)))
        (ping a)
        (ping b)
        (config-bloom a bloom-false-probability)
        (config-bloom b bloom-false-probability)
        (pmap (lambda(ip) (setup-db ip n p bulk)) (list a b))
        (sleep 1)
        (let ((res (reconcile a)))
          ;; FIXME: Add verification of result.
          (displayln (assemble-result bloom-false-probability res))
          (flush-output))))))

(define (test/log n p bulk (range #f) (samples 3))
  (let* ((file (file-name n p bulk))
         (dt (match/values
                 (time-apply (lambda ()
                               (with-output-to-file file
                                 (lambda() (test n p bulk range samples))))
                             '())
               ((_ _ dt _) dt))))
    (cmd (format "./plot.R ~s ~s" file
                 (format "n=~a, p=~a, bulk=~a, runtime=~a" n p bulk
                         (format-runtime dt))))
    (displayln "done")))

(define (file-name n p bulk)
  (let ((git-revision (cmd "git rev-parse --short HEAD"))
        (date (date->string (current-date) #t)))
    (format "testrun-~a-~a-~a-~a-~a.dat" git-revision n p bulk date)))

(define (format-runtime ms)
  (let ((s (ms->s ms)))
    (cond ((< s 60) (format "~a s" (~r s #:precision '(= 1))))
          ((< s 3600) (format "~a min" (~r (/ s 60) #:precision '(= 1))))
          (else (format "~a h" (~r (/ s 3600) #:precision '(= 1)))))))

(define (quick)  (test/log 400 0.1 16 (in-list '(0.001)) 1))
(define (quick2) (test/log 400 0.1 16 (in-range 0.001 0.9 0.1) 1))

(define (all)
  (for ((p (in-list (list 0.01 0.05 0.1 0.2 0.8 0.9))))
    (printf "running p=~a~n" p)
    (test/log 100000 p 4096))
  (beep))

(define (pmap f xs)
  (let ((cs (for/list ([x xs]) (make-channel))))
    (for ([x xs] [c cs]) (thread (thunk (channel-put c (f x)))))
    (for/list ([c cs]) (channel-get c))))
