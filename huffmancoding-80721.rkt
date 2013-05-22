; Валерия Браянова
; ф.н.: 80721
; 5 група


;making sorted frequency list
(define (qSortFreqList l)
  (if (null? l) l
   (append
        (qSortFreqList (filter (lambda (x) (< (cdr x) (cdar l))) l))
        (filter (lambda (x) (= (cdr x) (cdar l))) l)
        (qSortFreqList (filter (lambda (x) (> (cdr x) (cdar l))) l))
 )))

(define (listAsTreeLeaves l)
  (if (null? l) l
                (append (list (make-tree (car l) '() '())) (listAsTreeLeaves (cdr l)))
 ))
;making tree
(define (make-tree root left right)
  (list root left right)
  )
;sorted insertion into a tree
(define(insertSortedTree l x)
  (cond ((null? l) (list x))
        ((> (cdaar l) (cdar x)) (append (list x) l))
        (else (append (list (car l)) (insertSortedTree (cdr l) x)))
  ))
 
;making a frequency list
(define (Frequencies ls compare?)
  (define (helper l)
    (if (null? l) l
                  (append (list (cons (car l) (length (filter (lambda (x) (compare? x (car l))) l))))
                          (helper (filter (lambda (x) (not (compare? x (car l)))) l)))
    ))
  
  (qSortFreqList (helper ls)))

(define (Codes l compare?) 
  ;making huffman tree
  (define (helperTree ls)
    (if (>= 1 (length ls)) ls           
                         (helperTree (insertSortedTree (cddr ls) 
                                                   (make-tree (cons '() (+ (cdaar ls) (cdaadr ls))) (car ls) (cadr ls)))) 
   ))
  
  (define huffmanTree (helperTree (listAsTreeLeaves (Frequencies l compare?)))) 
   
  ;coding huffman tree
  (define (helperCoding subTree code)
    (if (not (null? (caar subTree))) (list (list (caar subTree) code))
        (append (helperCoding (cadr subTree) (append code (list 0))) ; letf-tree
                (helperCoding (caddr subTree) (append code (list 1)) ; right-tree
   ))))
  
  (cond ((null? huffmanTree) '())
        ((not (null? (caaar huffmanTree))) (list (caaar huffmanTree) 1))
        (else (helperCoding (car huffmanTree) '())))  
)

; making encode list
(define (Encode l codes compare?)

  (define (encoder ls)
    (if (null? ls) ls
        (append (cdar (filter (lambda (x) (compare? (car x) (car ls))) codes))
                (encoder (cdr ls))
   )))
  
  (define (clearList ls)
   (if (null? ls) ls
    (append (car ls) (clearList (cdr ls))))
   )
  
  (clearList (encoder l))
)

(define (encode l compare?)
  (list (Frequencies l compare?) 
        (Codes l compare?)
        (Encode l (Codes l compare?) compare?) 
 ))
 

(define L (list (cons 1 2) 3 's 45 (cons 4 7) "hey" "hey" 'f "fil" 2 (cons 1 2) 3 3 3 3 (cons 1 2) "help" (cons 4 7)))
(define L1 '((1.2) "hello world" a (1.2) 5 5 5 ))