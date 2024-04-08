#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

; Define tokens for actions and ingredients
(define-tokens kitchen-tokens (INGREDIENT))
(define-empty-tokens kitchen-actions (CHOP MIX BAKE PEEL WHISK TASTE-EQ TASTE-GT EOF))

; Lexer definitions
(define-lex-abbrevs
  (ingredient (re-+ (re-or (char-range "0" "9")))))

(define kitchen-lexer
  (lexer
   [(re-+ (char-range "0" "9")) (token-INGREDIENT (string->number lexeme))]
   ["chop" (token-CHOP)]
   ["mix" (token-MIX)]
   ["bake" (token-BAKE)]
   ["peel" (token-PEEL)]
   ["whisk" (token-WHISK)]
   ["taste-eq" (token-TASTE-EQ)]
   ["taste-gt" (token-TASTE-GT)]
   [whitespace (kitchen-lexer input-port)] ; Handles whitespace
   [(eof) (token-EOF)])) ; End of file token

; Parser definitions
(define-struct chop-exp (exp))
(define-struct mix-exp (e1 e2))
(define-struct bake-exp (operation exp times))
(define-struct peel-exp (exp))
(define-struct whisk-exp (e1 e2))
(define-struct ingredient-exp (value))
(define-struct taste-eq-exp (exp1 exp2 do-this else-do-this))
(define-struct taste-gt-exp (exp threshold do-this else-do-this))

(define (lex-this lexer input)
  (lambda ()
    (lexer input)))

(define kitchen-parser
  (parser
   (start exp)
   (end EOF)
   (error void)
   (tokens kitchen-tokens kitchen-actions)
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
         ((TASTE-GT exp INGREDIENT exp exp) (taste-gt-exp $2 $3 $4 $5))))))

; Evaluation function
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
         (eval else-do-this)))))

(define (kitchen-code-evaluate input-string)
  (let* ((input (open-input-string input-string))
         (lexer-output (lex-this kitchen-lexer input))
         (parsed (kitchen-parser lexer-output)))
    (eval parsed)))

; Example tests
(kitchen-code-evaluate "chop 10")  ; Should return 5
(kitchen-code-evaluate "mix 5 3")  ; Should return 8
(kitchen-code-evaluate "peel 5")   ; Should return 4
(kitchen-code-evaluate "whisk 3 4") ; Should return 12
(kitchen-code-evaluate "bake chop 10 1") ; should return 5
(kitchen-code-evaluate "bake peel 10 3") ; should return 7
(kitchen-code-evaluate "bake mix 10 3 1") ; should return 13
(kitchen-code-evaluate "bake whisk 10 2 1") ; should return 20
(kitchen-code-evaluate "taste-eq chop 10 mix 5 5 whisk 3 4 mix 5 5") ; Should execute and return the result of whisk 3 4 if true, else result of mix 5 5
(kitchen-code-evaluate "taste-gt chop 20 5 peel 2 mix 1 1") ; Should execute and return the result of peel 2 if true, else result of mix 1 1
