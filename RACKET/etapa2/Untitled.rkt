#lang racket
(let (( do (lambda () (display "done") 2))
      (whatto (lambda (what) (what))))
      (whatto do) (whatto do) (whatto do))