#lang racket

(provide (all-defined-out))

; ATENȚIE - Veți avea de reimplementat mai multe funcții
; din etapa 1, însă cu un alt mod de rezolvare (folosind
; funcționale sau alte funcții solicitate în enunț).
; Enunțul acestor exerciții va debuta prin "Ca în etapa 1,
; dar este interzisă recursivitatea explicită".


; TODO 1
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți orice funcțională exceptând foldl/foldr.

(define (get-men mpref)
  (map car mpref) ;extrage primul element din aceste liste
  )


; TODO 2
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți foldl sau foldr, astfel încât să nu fie necesare
; operații de tip append sau reverse.

(define (get-women wpref)
  (foldr (lambda (L acc) (cons (car L) acc)) '() wpref) ;adauga la o lista acumulator primul element din listele date
  )

; TODO 3
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Folosiți minim o funcțională și minim o funcție anonimă.
(define (get-pref-list pref person)
  (cdar (filter (lambda (x) (equal? (car x) person)) pref)) ;selecteaza perechile ce contin 'ana, dupa care extrage partenerul
  )


; TODO 4
; Ca în etapa 1, dar este interzisă recursivitatea explicită
; și sunt permiși operatorii condiționali:
; Implementați o funcție care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Folosiți funcția member.

(define (preferable? pref-list x y)
  (> (length (member x pref-list)) (length (member y pref-list)))
  )



; TODO 5
; Implementați recursiv funcționala find-first, care primește
; un predicat și o listă și întoarce primul element al listei
; care satisface predicatul, sau false dacă un asemenea element
; nu există.
; Implementarea trebuie să fie eficientă în sensul că nu trebuie
; să continue explorarea listei odată ce s-a găsit elementul.

; implementarea este eficienta, pentru ca in momentul in care
;gasim un element care satisface predicatul, il returnam, fara a
;mai apela functia recursiv 
(define (find-first p L)
  (cond
    ((null? L) #f)
    ((p (car L)) (car L))
    (else (find-first p (cdr L)))
  )
  )



; TODO 6
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți find-first, fără să îl apelați de 2 ori (hint: define în define).

(define (get-partner engagements person)
  (define (result engagements person)
    (find-first (lambda (x) (equal? (car x) person)) engagements)
  )
  (if(result engagements person)
     (cdr (result engagements person)) ;extragere partener din pereche
     #f
     )
)


; TODO 7
; Implementați recursiv funcționala change-first care primește
; un predicat p, o listă L și o valoare val, și întoarce o nouă 
; listă în care primul element din L care satisface predicatul p
; a fost înlocuit cu valoarea val, celelalte rămânând la fel.
; Dacă niciun element din L nu satisface predicatul, lista L
; rămâne neschimbată.


(define (change-first p L val)
  (cond
    ((null? L) '())
    ((equal? (find-first p L) (car L) ) (cons val (cdr L))) ;adaug la restul listei valoarea mea si nu mai apelez recursiv pt restul listei
    (else (cons (car L) (change-first p (cdr L) val))) ; adaug el curent la rezultatul functiei recursive, deci caut mai departe val
    )
  )



; TODO 8
; Implementați funcția update-engagements care primește o listă de
; logodne engagements și două persoane p1 și p2, și întoarce lista
; actualizată de logodne, în care vechiul partener al lui p1 este
; înlocuit cu p2.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - p1 era logodită în prealabil
; - fiecare cuplu din lista engagements are pe prima poziție
;   persoanele de același gen cu p1
; Folosiți change-first.

;folosim functia change-first pentru a modifica prima pereche gasita cu conditia ca
;prima persoana din logodna este p1
(define (update-engagements engagements p1 p2)
  (change-first (lambda (x) (equal? (car x) p1)) engagements (cons p1 p2))
  )


; TODO
; Copiați implementarea funcției better-match-exists? din etapa 1.
; Funcția nu este repunctată de checker, dar este necesară pentru
; implementarea funcției stable-match? de mai jos.
; Dacă nu ați implementat better-match-exists? în etapa 1, solicitați 
; o rezolvare de la asistent, astfel încât să puteți continua.

(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (if (equal? (car p1-list) p2)
      #f
      (if(preferable? (get-pref-list pref2 (car p1-list)) p1 (get-partner engagements (car p1-list)))
         #t
         (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements)
         )
      )
  )

; TODO 9
; Implementați funcția stable-match? care primește o listă 
; completă de logodne engagements, o listă de preferințe masculine 
; mpref și o listă de preferințe feminine wpref, și întoarce true 
; dacă toate cuplurile din engagements sunt stabile.
; Un cuplu este stabil dacă pentru niciunul din membrii cuplului
; nu există un alt partener mai potrivit (conform definiției de
; la funcția better-match-exists?).
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie


(define (reverse-pairs L)
  (map (lambda (x) (cons (cdr x) (car x))) L))

(define (verify-couple p1 p2 mpref wpref engagements)
  (if (better-match-exists? p1 p2 (get-pref-list wpref p1) mpref (reverse-pairs engagements))
          #f
          (if(better-match-exists? p2 p1 (get-pref-list mpref p2) wpref engagements)
             #f
             #t
             )
      )
  )


(define (stable-match? engagements mpref wpref)
  (foldl (lambda (x acc)
            (and acc
                (verify-couple (car x) (cdr x) mpref wpref engagements)))
         #t engagements)
  )



  





