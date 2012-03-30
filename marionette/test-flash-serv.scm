(load "./flash-serv")

(define (my-xml-handler xml end output-port)
  #?=xml
  )

;; Create server procedure
(define server
  ((with-module net.xmlsocket-serv make-server) my-xml-handler)
  )

;; Invoke server
(server)
