(define-library (gemini client)
  (export gemini-get)
  (import (scheme base) (gemini))
  (cond-expand
    (chicken
     (import (chicken condition) (openssl) (uri-generic))
     (begin
       (define-condition-type &invalid-uri &error
         make-invalid-uri-error invalid-uri-error?))))
  (begin

    (define (write-request to-server uri-string)
      (write-string (string-append uri-string "\r\n") to-server))

    (define (read-response from-server)
      (let ((line (read-cr-lf-terminated-line from-server)))
        (if (or (< (string-length line) 3)
                (not (char<=? #\0 (string-ref line 0) #\9))
                (not (char<=? #\0 (string-ref line 1) #\9))
                (not (char=? #\space (string-ref line 2))))
            (error "Malformed Gemini response line" line)
            (let ((code (string->number (string-copy line 0 2)))
                  (meta (string-copy line 3 (string-length line))))
              (make-gemini-response code meta from-server)))))

    (define (gemini-get uri handle-response)
      ;; Validate input types
      (unless (or (string? uri) (uri? uri))
        (error "gemini-get: URI must be a string or URI object" uri))
      (unless (procedure? handle-response)
        (error "gemini-get: handle-response must be a procedure" handle-response))
      
      (let* ((uri-object (if (string? uri) (uri-reference uri) uri))
             (uri-string (uri->string uri-object))
             (host (uri-host uri-object)))
        
        ;; Validate URI scheme
        (unless (eq? 'gemini (uri-scheme uri-object))
          (raise (make-invalid-uri-error
                 (string-append "Invalid scheme: "
                               (symbol->string (uri-scheme uri-object))
                               " (expected gemini)")))
        
        ;; Validate host
        (unless (and host (string? host) (> (string-length host) 0))
          (error "gemini-get: URI must have a valid host" uri-object))
        
        ;; Make connection and handle request
        (let-values (((from-server to-server)
                     (ssl-connect* hostname: host
                                   port: (or (uri-port uri-object) 1965)
                                   verify?: #f)))
          (dynamic-wind
            (lambda () #f)  ; no setup needed
            (lambda ()
              (write-request to-server uri-string)
              (handle-response (read-response from-server)))
            (lambda ()
              (close-input-port from-server)
              (close-output-port to-server))))))))
