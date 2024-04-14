#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

; Define tokens for actions and ingredients
(define-tokens kitchen-tokens (INGREDIENT))
(define-empty-tokens kitchen-actions (CHOP MIX PEEL WHISK POUR EOF))
(define-empty-tokens kitchen-conditional (TASTE-EQ TASTE-GT TASTE-LT))
(define-empty-tokens kitchen-loop (BAKE))

; Lexer definitions
(define-lex-abbrevs
  (ingredient (re-+ (re-or (char-range "0" "9")))))


(define kitchen-lexer
  (lexer
   [(re-+ (char-range "0" "9"))
    (begin
      (displayln (format "Next lexeme is ~a, Next token: INGREDIENT" lexeme))
      (token-INGREDIENT (string->number lexeme)))]
   ["chop" (begin
            (displayln "Next lexeme is chop, Next token: KITCHEN-ACTION")
            (token-CHOP))]
   ["mix" (begin
           (displayln "Next lexeme is mix, Next token: KITCHEN-ACTION")
           (token-MIX))]
   ["bake" (begin
            (displayln "Next lexeme is bake, Next token: KITCHEN-LOOP")
            (token-BAKE))]
   ["peel" (begin
            (displayln "Next lexeme is peel, Next token: KITCHEN-ACTION")
            (token-PEEL))]
   ["whisk" (begin
             (displayln "Next lexeme is whisk, Next token: KITCHEN-ACTION")
             (token-WHISK))]
   ["taste-eq" (begin
                 (displayln "Next lexeme is taste-eq, Next token: KITCHEN-CONDITIONAL")
                 (token-TASTE-EQ))]
   ["taste-gt" (begin
                 (displayln "Next lexeme is taste-gt, Next token: KITCHEN-CONDITIONAL")
                 (token-TASTE-GT))]
   ["taste-lt" (begin
                 (displayln "Next lexeme is taste-lt, Next token: KITCHEN-CONDITIONAL")
                 (token-TASTE-LT))]
   ["pour" (begin
             (displayln "Next lexeme is pour, Next token: KITCHEN-ACTION")
             (token-POUR))]
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
(define-struct pour-exp (e1 e2))

(define (lex-this lexer input)
  (lambda ()
    (lexer input)))

(define kitchen-parser
  (parser
   (start exp)
   (end EOF)
   (error void)
   (tokens kitchen-tokens kitchen-actions kitchen-conditional kitchen-loop)
   (grammar
    (exp ((INGREDIENT) (ingredient-exp $1))
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

; Adjust output-is to return the value after printing
(define (output-is result)
  (displayln (format "output is: ~a" result))
  result)  ; Ensure it returns the result

; Evaluation function modified to return values only
(define (eval exp)
  (match exp
    ((ingredient-exp value) value)
    ((chop-exp e) (/ (eval e) 2))
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
(define (kitchen-code-evaluate input-string)
  (let* ((input (open-input-string input-string))
         (lexer-output (lex-this kitchen-lexer input))
         (parsed (kitchen-parser lexer-output))
         (result (eval parsed)))
    (output-is result)
    (void)))  ; Print final result
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