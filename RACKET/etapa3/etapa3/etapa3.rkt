#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.


(define (get-unstable-couples engagements mpref wpref)
  (filter (lambda (x)
            (letrec ((p1 (car x))
                  (p2 (cdr x))
                  (p1-pref (get-pref-list wpref p1))
                  (p2-pref (get-pref-list mpref p2))
                  (rev-engagements (map (lambda (x) (cons (cdr x) (car x))) engagements))
                  )
              (or (better-match-exists? p1 p2 p1-pref mpref rev-engagements)
                  (better-match-exists? p2 p1 p2-pref wpref engagements))
              )
            )
          engagements)        
  )      


; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.


(define (engage free-men engagements mpref wpref)
  (cond
    ((null? free-men) engagements) ; cat timp exista un barbat nelogodit
    (else
     (let try-next-woman ((m (car free-men))
              (pref-list (get-pref-list mpref (car free-men)))
              (free-men (cdr free-men))
              (engagements engagements))

          (if(null? pref-list) ;daca nu mai sunt femei pe lista lui m, continui cu restul barbatilor
               (engage free-men engagements mpref wpref)
               (let ((w (car pref-list)))
               (if (not (member w (map car engagements))) ;daca w nu e logodita, o logodim cu m
                    (engage free-men (cons (cons w m) engagements) mpref wpref)
                    (let* ((curr-engagement (assoc w engagements)) ;partener curent w
                            (m1 (cdr curr-engagement))
                            (her-pref-list (get-pref-list wpref w))
                            (new-free-men (append free-men (list m1)))
                            (new-pref-list (get-pref-list mpref (car new-free-men)))
                            (new-engagements (cons (cons w m) (remove curr-engagement engagements))))
                       (if (preferable? her-pref-list m m1) ;daca e logodita verificam sa il prefere pe m lui m1
                               (try-next-woman (car new-free-men) new-pref-list (cdr new-free-men) new-engagements) ;verificam iarasi cu noul engagement si cu noua lista de preferinte de femei
                               (try-next-woman m (cdr pref-list) free-men engagements)
                       )
                    )
                )
               )
          )
      )
     )
    )
  )
  
         




; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (let ((free-men (map car mpref)) ;toti barbatii 
        (engagements '()) ;nicio logodna stabilita
        )
    (engage free-men engagements mpref wpref)
    )
  )
                 



; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.

; parcurgem fiecare pereche cu foldl.
; adaugam in acumulator elemenetele (car x) si (cdr x)
(define (get-couple-members pair-list)
  (foldl (lambda (x acc)
           (cons (car x) (cons (cdr x) acc)))
         '()
         pair-list))


