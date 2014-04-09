(define-module (robot)
  :use-module (ice-9 rdelim))

(define (input)
  (let ((line (read-line)))
    (if (equal? line "quit") (exit 0))
    line))

(define (output s) (format #t "~a\n" s) (force-output (current-output-port)))

(define-public (laby_name_left) (output "left") (input))
(define-public (laby_name_right) (output "right") (input))
(define-public (laby_name_forward) (output "forward") (input))
(define-public (laby_name_take) (output "take") (input))
(define-public (laby_name_drop) (output "drop") (input))
(define-public (laby_name_escape) (output "escape") (input))

(define-public (laby_name_say s) (output (string-append "say " s)) (input))

(define-public laby_name_Void 0)
(define-public laby_name_Wall 1)
(define-public laby_name_Rock 2)
(define-public laby_name_Web 3)
(define-public laby_name_Exit 4)
(define-public laby_name_Unknown 5)

(define-public (laby_name_look)
  (output "look")
  (let ((answer (string->symbol (input))))
    (case answer
      ((void) laby_name_Void)
      ((wall) laby_name_Wall)
      ((rock) laby_name_Rock)
      ((web) laby_name_Web)
      ((exit) laby_name_Exit)
      (else laby_name_Unknown))))

(output "start")
(input)
