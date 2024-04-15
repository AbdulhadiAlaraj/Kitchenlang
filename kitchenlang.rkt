#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(require racket/hash)

; Define tokens for actions and ingredients
(define-tokens kitchen-tokens (INGREDIENT VAR))
(define-empty-tokens kitchen-actions (CHOP MIX PEEL WHISK POUR EOF))
(define-empty-tokens kitchen-conditional (TASTE-EQ TASTE-GT TASTE-LT))
(define-empty-tokens kitchen-loop (BAKE))

; Lexer definitions
(define-lex-abbrevs
  (letters (re-+ (re-range #\a #\z #\A #\Z)))
  (digits (re-+ (re-range #\0 #\9))))

(define kitchen-lexer
  (lexer
   [(re-+ (char-range "0" "9"))
    (begin
      (displayln (format "Next lexeme is ~a, Next token: INGREDIENT" lexeme))
      (token-INGREDIENT (string->number lexeme)))]
   ["chop" (begin
            (displayln (format "Next lexeme is ~a, Next token: CHOP" lexeme))
            (token-CHOP))]
   ["mix" (begin
           (displayln (format "Next lexeme is ~a, Next token: MIX" lexeme))
           (token-MIX))]
   ["bake" (begin
            (displayln (format "Next lexeme is ~a, Next token: BAKE" lexeme))
            (token-BAKE))]
   ["peel" (begin
            (displayln (format "Next lexeme is ~a, Next token: PEEL" lexeme))
            (token-PEEL))]
   ["whisk" (begin
             (displayln (format "Next lexeme is ~a, Next token: WHISK" lexeme))
             (token-WHISK))]
   ["taste-eq" (begin
                 (displayln (format "Next lexeme is ~a, Next token: TASTE-EQ" lexeme))
                 (token-TASTE-EQ))]
   ["taste-gt" (begin
                 (displayln (format "Next lexeme is ~a, Next token: TASTE-GT" lexeme))
                 (token-TASTE-GT))]
   ["taste-lt" (begin
                 (displayln (format "Next lexeme is ~a, Next token: TASTE-LT" lexeme))
                 (token-TASTE-LT))]
   ["pour" (begin
             (displayln (format "Next lexeme is ~a, Next token: POUR" lexeme))
             (token-POUR))]
   [(re-+ (re-or (char-range #\a #\z) (char-range #\A #\Z)))
    (begin
      (displayln (format "Next lexeme is ~a, Next token: VAR" lexeme))
      (token-VAR lexeme))]
   [whitespace (kitchen-lexer input-port)] ; Handles whitespace
   [(eof) (begin
           (displayln "Next lexeme is EOF, Next token: EOF")
           (token-EOF))])) ; End of file token



; Parser definitions
(define-struct chop-exp (exp))
(define-struct mix-exp (e1 e2))
(define-struct bake-exp (operation exp times))
(define-struct peel-exp (exp))
(define-struct whisk-exp (e1 e2))
(define-struct ingredient-exp (value))
(define-struct taste-eq-exp (exp1 exp2 do-this else-do-this))
(define-struct taste-gt-exp (exp threshold do-this else-do-this))
(define-struct taste-lt-exp (exp threshold do-this else-do-this))
(define-struct var-exp (name))
(define-struct pour-exp (var exp))

(define (lex-this lexer input)
  (lambda ()
    (lexer input)))

(define kitchen-parser
  (parser
   (start exp-list)
   (end EOF)
   (error void)
   (tokens kitchen-tokens kitchen-actions kitchen-conditional kitchen-loop)
   (grammar
    (exp-list ((exp) (list $1))
              ((exp-list exp) (append $1 (list $2))))
    (exp ((INGREDIENT) (ingredient-exp $1))
         ((VAR) (var-exp $1))
         ((POUR VAR exp) (pour-exp $2 $3))  ; Handle 'pour' followed by a variable and any expression
         ((CHOP exp) (chop-exp $2))
         ((MIX exp exp) (mix-exp $2 $3))
         ((BAKE CHOP exp INGREDIENT) (bake-exp 'chop $3 $4))
         ((BAKE MIX exp exp INGREDIENT) (bake-exp 'mix (list $3 $4) $5))
         ((BAKE PEEL exp INGREDIENT) (bake-exp 'peel $3 $4))
         ((BAKE WHISK exp exp INGREDIENT) (bake-exp 'whisk (list $3 $4) $5))
         ((PEEL exp) (peel-exp $2))
         ((WHISK exp exp) (whisk-exp $2 $3))
         ((TASTE-EQ exp exp exp exp) (taste-eq-exp $2 $3 $4 $5))
         ((TASTE-GT exp INGREDIENT exp exp) (taste-gt-exp $2 $3 $4 $5))
         ((TASTE-LT exp INGREDIENT exp exp) (taste-lt-exp $2 $3 $4 $5))))))

; Environment setup
(define env (make-hash))

; Adjust output to return the value after printing
(define (output-is result)
  (displayln (format "output is: ~a" result))
  result)  ; Ensure it returns the result

(define (eval-exp-list exp-list)
  (map eval exp-list))

; Evaluation function modified to return values only
(define (eval exp)
  (match exp
    ((ingredient-exp value) value)
    ((chop-exp e) (/ (eval e) 2))
    ((var-exp var) (hash-ref env var 0)) ; Default to 0 if not found
    ((pour-exp var exp) (let ([result (eval exp)])  ; Evaluate the expression and assign the result to the variable
                          (hash-set! env var result)
                          result))    
    ((mix-exp e1 e2) (+ (eval e1) (eval e2)))
    ((bake-exp operation exp times)
     (match operation
       ('chop (let loop ([count times] [acc (eval exp)])
                (if (= count 0)
                    acc
                    (loop (- count 1) (/ acc 2)))))
       ('mix (let loop ([count times] [acc (eval (first exp))])
                (if (= count 0)
                    acc
                    (loop (- count 1) (+ acc (eval (second exp)))))))
       ('peel (let loop ([count times] [acc (eval exp)])
                (if (= count 0)
                    acc
                    (loop (- count 1) (- acc 1)))))
       ('whisk (let loop ([count times] [acc (eval (first exp))])
                 (if (= count 0)
                     acc
                     (loop (- count 1) (* acc (eval (second exp)))))))))
    ((peel-exp e) (- (eval e) 1))
    ((whisk-exp e1 e2) (* (eval e1) (eval e2)))
    ((taste-eq-exp exp1 exp2 do-this else-do-this)
     (if (= (eval exp1) (eval exp2))
         (eval do-this)
         (eval else-do-this)))
    ((taste-gt-exp exp threshold do-this else-do-this)
     (if (> (eval exp) threshold)
         (eval do-this)
         (eval else-do-this)))
    ((taste-lt-exp exp threshold do-this else-do-this)
     (if (< (eval exp) threshold)
         (eval do-this)
         (eval else-do-this)))
    ))

; Function to evaluate input and print final result
(define (kitchen-code-evaluate input-string-or-path)
  (define input-port
    (if (file-exists? input-string-or-path)
        (open-input-file input-string-or-path)
        (open-input-string input-string-or-path)))
  (let* ((lexer-output (lex-this kitchen-lexer input-port))
         (parsed (kitchen-parser lexer-output))
         (results (map eval parsed)))
    (map output-is results)  ; Process and display each result
    (close-input-port input-port)))

     ; Return void to suppress automatic output in interactive environments
; Example tests
(kitchen-code-evaluate "chop 10")  ; Should return 5
(displayln "-----------------------------------")
(kitchen-code-evaluate "mix 5 3")  ; Should return 8
(displayln "-----------------------------------")
(kitchen-code-evaluate "peel 5")   ; Should return 4
(displayln "-----------------------------------")
(kitchen-code-evaluate "whisk 3 4") ; Should return 12
(displayln "-----------------------------------")
(kitchen-code-evaluate "mix whisk 5 3 peel 7")  ; Should return 21
(displayln "-----------------------------------")
(kitchen-code-evaluate "bake chop 10 1") ; should return 5
(displayln "-----------------------------------")
(kitchen-code-evaluate "bake peel 10 3") ; should return 7
(displayln "-----------------------------------")
(kitchen-code-evaluate "bake mix 10 3 1") ; should return 13
(displayln "-----------------------------------")
(kitchen-code-evaluate "bake whisk 10 2 1") ; should return 20
(displayln "-----------------------------------")
(kitchen-code-evaluate "taste-eq chop 10 mix 5 5 whisk 3 4 mix 5 5") ; Should execute and return the result of whisk 3 4 if true, else result of mix 5 5
(displayln "-----------------------------------")
(kitchen-code-evaluate "taste-gt chop 20 5 peel 2 mix 1 1") ; Should execute and return the result of peel 2 if true, else result of mix 1 1
(displayln "-----------------------------------")
(kitchen-code-evaluate "taste-lt chop 10 3 peel 21 mix 1 2"); Should execute and return the result of mix 1 2 if false, else result of peel 21
(displayln "-----------------------------------")
(kitchen-code-evaluate "taste-lt bake mix 10 3 1 20 peel 21 mix 1 2"); Should execute and return the result of mix 1 2 if true, else result of peel 21
(displayln "-----------------------------------")
(kitchen-code-evaluate "test.kcl"); Reads the test.kcl file in the same directory
(displayln "-----------------------------------")
(kitchen-code-evaluate "pour x 20"); assigns 20 to x
(displayln "-----------------------------------")
(kitchen-code-evaluate "mix x x"); adds x to itself. Output is 40 here
(displayln "-----------------------------------")
(kitchen-code-evaluate "pour x 5"); reassigns 5 to x
(displayln "-----------------------------------")
(kitchen-code-evaluate "mix x x"); adds x to itself. output is 10
(displayln "-----------------------------------")
(kitchen-code-evaluate "pour y mix x x"); assign the output of mix x x to y
(displayln "-----------------------------------")
(kitchen-code-evaluate "whisk y y"); multiply y by itself. Ouput is 100 here
