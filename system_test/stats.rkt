#lang at-exp racket

(require net/http-client)
(require json)
(require racket/date)
(require racket/gui/base)
(require racket/engine)

(date-display-format 'iso-8601)

(define dbg (lambda (fmt . args)
              (date-display-format 'iso-8601)
              (let ((date (date->string (current-date) #t)))
                (apply printf (cons (string-append "~a: " fmt) (cons date args))))
              (flush-output (current-output-port))))

(define (cmd x) (string-trim (with-output-to-string (lambda() (system x)))))
(define (cmddbg x)
  (dbg "executing: ~a~n" x)
  (let ((res (cmd x)))
    (dbg "done~n")
    res))

(define (node-a) (cmd "~/bin/terraform output node_a"))
(define (node-b) (cmd "~/bin/terraform output node_b"))
(define (beep) (play-sound "/System/Library/Sounds/Glass.aiff" #f))

(define (configure-and-start-setconcile host a b)
  (let* ((f (lambda x (string-replace (apply string-append x) "\n" " ")))
         (c1 @f{ssh -o 'StrictHostKeyChecking no' -i ~/.ssh/pgw.pem
                ubuntu@"@"@host 'sed -i -e "s/NodeAIp = .*,/NodeAIp =
                \"@|a|\",/" /opt/setconcile/etc/config.txt'})
         (c2 @f{ssh -o 'StrictHostKeyChecking no' -i ~/.ssh/pgw.pem
                ubuntu@"@"@host 'sed -i -e "s/NodeBIp = .*,/NodeBIp =
                \"@|b|\",/" /opt/setconcile/etc/config.txt'})
         (c3 @f{ssh -o 'StrictHostKeyChecking no' -i ~/.ssh/pgw.pem
                ubuntu@"@"@host /opt/setconcile/_rel/setconcile/bin/setconcile
                start}))
    (cmddbg c1)
    (cmddbg c2)
    (cmddbg c3)))

(define (reinitialize-nodes-from-ami)

  (define (reinitialize-nodes)
    (cmddbg "~/bin/terraform taint -module=a aws_instance.replication_node")
    (cmddbg "~/bin/terraform taint -module=b aws_instance.replication_node")
    (cmddbg "~/bin/terraform apply --var-file=~/.aws/credentials.tf"))
  
  (reinitialize-nodes)
  (dbg "AWS initialization done.~n")
  (let ((a (node-a)) (b (node-b)))
    (dbg "Configuring and starting setconcile.~n")
    (configure-and-start-setconcile a a b)
    (configure-and-start-setconcile b a b)
    (dbg "Waiting for setconcile to start.~n")
    (sleep 120) ;; Give setconcile time to start.
    (dbg "Done starting setconcile.~n")))

(define (ping-retry ip)
  (let iter ((count 3))
      (cond ((= count 0) (error "ping failed three times"))
            (else
             (let* ((eng (engine (lambda (_)
                                   (with-handlers ((exn:fail? (lambda (_) #f)))
                                     (ping ip)))))
                    (_ (engine-run 5000 eng))
                    (res (engine-result eng)))
               (cond ((eq? res 'ok) 'ok)
                     (else
                      (dbg "retrying ping~n")
                      (sleep 5)
                      (iter (- count 1)))))))))

(define (ping ip)
  (let*-values (((status headers in)
                 (http-sendrecv ip (format "/api/ping") #:port 7363 #:method "GET"))
                ((res)
                 (port->string in)))
    (unless (string=? res "ok")
      (error (format "node not up (~s)" res))))
  'ok)

(define (setup-db ip n p bb)
  (http-sendrecv ip (format "/api/debug/setup_db?n=~a&p=~a&bb=~a" n p bb)
                 #:port 7363 #:method "POST")
  'ok)

(define (config-bloom ip bloom-false-probability)
  (http-sendrecv
   ip (format "/api/debug/config?ds=riak_set_a&bloom_false_probability=~a"
              (~r bloom-false-probability #:precision '(= 3)))
   #:port 7363 #:method "POST"))

(define (reconcile ip)
  (let-values (((status headers in)
                (http-sendrecv ip "/api/datasets/riak_set_a/recons" #:port 7363
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

(define (test outf n p bulk (range #f) (samples 3))
  (fprintf outf "false-probability, transfer-ratio, bloom-size, its, time~n")
  (for ((bloom-false-probability (or range (in-range 0.001 0.5 0.1))))
    (dbg " false-probability=~a~n" bloom-false-probability)
    (for ((tries (in-range 0 samples)))
      (dbg "  sample=~a~n" tries)

      ;; Setting up the db is slow, so reinitializing using a
      ;; preconfigured AMI is quicker. Assumes the AMI has already
      ;; been setup with the correct n,p,bulk parameters.
      ;;(pmap (lambda(ip) (setup-db ip n p bulk)) (list a b))
      (reinitialize-nodes-from-ami)
      
      (let ((a (node-a)) (b (node-b)))
        (dbg "Setting false probability.~n")
        (ping-retry a) (ping-retry b)
        (config-bloom a bloom-false-probability)
        (config-bloom b bloom-false-probability)

        (dbg "Starting reconciliation.~n")
        (let ((res (reconcile a)))
          ;; FIXME: Add verification of result.
          (displayln (assemble-result bloom-false-probability res) outf)
          (flush-output outf))))))

(define (test/log n p bulk (range #f) (samples 3))
  (let* ((file (file-name n p bulk))
         (dt (match/values
                 (time-apply (lambda ()
                               (call-with-output-file file
                                 (lambda (outf)
                                   (test outf n p bulk range samples))))
                             '())
               ((_ _ dt _) dt))))
    (cmd (format "./plot.R ~s ~s" file
                 (format "n=~a, p=~a, bulk=~a, runtime=~a" n p bulk
                         (format-runtime dt))))
    (dbg "done~n")))

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
  (for ((p (in-list (list 0.001))))
    (printf "Testing with p=~a~n" p)
    (sleep 1)
    (test/log 1000000 p 4096 (in-list '(0.001 0.1 0.15 0.2 0.25 0.3 0.4 0.5 0.6))))
  (beep))

(define (pmap f xs)
  (let ((cs (for/list ([x xs]) (make-channel))))
    (for ([x xs] [c cs]) (thread (thunk (channel-put c (f x)))))
    (for/list ([c cs]) (channel-get c))))
