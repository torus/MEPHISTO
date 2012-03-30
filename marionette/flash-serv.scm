;; XMLSocket server

;; EXAMPLE:
;; ;; Define a XML handler
;; (define (my-xml-handler xml end output-port)
;;   #?=xml
;;   )
;;
;; ;; Create server procedure
;; (define server (make-server my-xml-handler))
;;
;; ;; Invoke server
;; (server)

(define-module net.xmlsocket-serv
  (use srfi-1)
  (use srfi-43)
  (use sxml.ssax)
  (use sxml.serializer)
  (use gauche.net)
  (export make-server)
  )

(select-module net.xmlsocket-serv)

(define (send-message-raw mesg output-port)
  (display (srl:sxml->xml-noindent mesg) output-port)
  (display "\0" output-port))

(define (process-input xml end output-port handler)
  (if (eq? (caadr xml) 'policy-file-request)
      ;; <cross-domain-policy>
      ;; <allow-access-from domain="*" to-ports="*"/>
      ;; </cross-domain-policy>
      (begin
	(send-message-raw '(*TOP* (cross-domain-policy
				   (allow-access-from (@ (domain "*")
							 (to-ports "*")))))
			  output-port)
	(end 'continue))
      (handler (cadr xml) end output-port)))

(define (flash-listen-accept output-port)
  (let* ((listen-sock (make-server-socket 'inet 0))
	 (port (sockaddr-port (socket-address listen-sock))))
    (print #`"Flash XMLSocket server started; port# = ,port")
    (let loop ((final #f))
      (let ((sock (socket-accept listen-sock)))
	(when final (socket-close listen-sock))
	(values sock (if final #f loop))))))

(define (start-server iport oport input-handler)
  (call/cc
   (lambda (end)
     (let loop ()
       (let ((input (ssax:xml->sxml iport '())))
	 (process-input input end oport input-handler)
	 (let skip-whitespece ()
	   (let1 c (peek-char iport)
	     (if (eof-object? c)
		 (end 'done)
		 (when (or (char-whitespace? c)
			   (char=? c #\null))
		   (read-char iport)
		   (skip-whitespece)))))
	 (loop))))))

(define (make-server xml-handler)
  (lambda ()
    (let-values (((sock accept)
		  (flash-listen-accept (current-output-port))))
      (let loop ((sock sock)
		 (accept accept))
	(let* ((input-port (socket-input-port sock))
	       (output-port (socket-output-port sock)))
	  (set! (port-buffering output-port) :none)
	  (port-buffering output-port)
	  (if (eq? (start-server input-port output-port xml-handler) 'continue)
	      (let-values (((sock accept) (accept #t)))
		(loop sock accept))
	      'done))))))

(provide "net.xmlsocket-serv")
