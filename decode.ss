
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2014                              *
; *  Author: Liu Liu                          *
; *          Ulrich Kremer                    *
; *  April 5, 2014                            *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ctv", "vtc",and "reduce" definitions
(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
(load "test-dictionary.ss")

(load "dictionary.ss") ;; the real thing with 45,000 words


;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

;This function takes in an element and a list and returns the index of that element in that list
(define indexoflist
 (lambda (i list) 
  (if (null? list)
   -1
    (if (eq? (car list) i)
      0
      (if (= (indexoflist i (cdr list)) -1) 
       -1
       (+ 1 (indexoflist i (cdr list))))))))

;This function returns the max element from the list
(define listmax
    (lambda (list)
      (cond
        ((equal? '() list) -inf.0)
        (else (findmaxlist (cdr list) (car list))))))
 
;This function recursively goes through the list and finds the max. 
(define findmaxlist
    (lambda (list maxcurr)
      (cond
        ((equal? '() list) maxcurr)
        (else (findmaxlist (cdr list) (max (car list) maxcurr))))))

;This function  
(define correctwords
  (lambda (p)
    (map (lambda (p) (spell-checker p)) p)))
  
(define dawg
  (lambda (p)
    (append (list (length (remove #f (correctwords (paragraph p 0)))))
            (list (length (remove #f (correctwords (paragraph p 1)))))
            (list (length (remove #f (correctwords (paragraph p 2)))))
            (list (length (remove #f (correctwords (paragraph p 3)))))
            (list (length (remove #f (correctwords (paragraph p 4)))))
            (list (length (remove #f (correctwords (paragraph p 5)))))
            (list (length (remove #f (correctwords (paragraph p 6)))))
            (list (length (remove #f (correctwords (paragraph p 7)))))
            (list (length (remove #f (correctwords (paragraph p 8)))))
            (list (length (remove #f (correctwords (paragraph p 9)))))
            (list (length (remove #f (correctwords (paragraph p 10)))))
            (list (length (remove #f (correctwords (paragraph p 11)))))
            (list (length (remove #f (correctwords (paragraph p 12)))))
            (list (length (remove #f (correctwords (paragraph p 13)))))
            (list (length (remove #f (correctwords (paragraph p 14)))))
            (list (length (remove #f (correctwords (paragraph p 15)))))
            (list (length (remove #f (correctwords (paragraph p 16)))))
            (list (length (remove #f (correctwords (paragraph p 17)))))
            (list (length (remove #f (correctwords (paragraph p 18)))))
            (list (length (remove #f (correctwords (paragraph p 19)))))
            (list (length (remove #f (correctwords (paragraph p 20)))))
            (list (length (remove #f (correctwords (paragraph p 21)))))
            (list (length (remove #f (correctwords (paragraph p 22)))))
            (list (length (remove #f (correctwords (paragraph p 23)))))
            (list (length (remove #f (correctwords (paragraph p 24)))))
            (list (length (remove #f (correctwords (paragraph p 25))))))))
      

            

            
(define paragraph
  (lambda (p n)
    (map (lambda (x) ((encode-n n) x)) p)));Takes an encoded word (being hte first word of a paragraph and appends recursively the first element the list starting from the second element of htat list. 

(define (remove x ls)
  (if (null? ls)
      '()
      (let ((h (car ls)))
        ((if (eqv? x h)
            (lambda (y) y)
            (lambda (y) (cons h y)))
         (remove x (cdr ls))))))

;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker 
  (lambda (w)
   ;;'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING *** 
    (if (member w dictionary)
        #t
        #f
   )))

;; -----------------------------------------------------
;; ENCODING FUNCTIONS


;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input=a word, output=encoded word
(define encode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded
     
     (map (lambda (x) (vtc x)) ;;every value gets converted to its character
          (map (lambda (x) (modulo(+ x n) 26))  ;;the algorithm is applied to the values
               (map (lambda (x) (ctv x)) w))) ;;all of the characters are converted to their value
      
      )))



;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
     (map (lambda (arg) (map encoder arg)) d) 
    ))
    
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
  (lambda (p)
   (if (= 0 0)
    (encode-n (indexoflist (listmax (dawg p)) (dawg p)))
    (encode-n (indexoflist (listmax (dawg p)) (dawg p))))))
     
       

;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)
    'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
    ))

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
  (lambda (d decoder)
    (
     (map (lambda (arg) (map decoder arg)) d) 
     )))

;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;;(spell-checker '(h e l l o))
;;(define add5 (encode-n 5))
;;(encode-d document add5)
;;(define decoderSP1 (Gen-Decoder-A paragraph))
;;(define decoderFA1 (Gen-Decoder-B paragraph))
;;(Code-Breaker document decoderSP1)

