(defvar tape '(-1))
(defvar tape-length 0)
(defvar pc 0)
(defvar tape-state 0)
(defvar num 0)
(defvar blank -1)
(defvar stop 0)


(defmacro set-state (num)
  (let ((symb (intern (format nil "SET-STATE-~d" num))))
    (eval `(defun ,symb ()
	     (setq tape-state ,num)))
    symb))

(defun declare-states (lst)
  "declares the states by defining functions that change the tape state variable to those states
a bit of a waste, but because of how I defined the defstate macro it's necessary for maintaining the
definition format"
  (eval (loop for i in (loop for i in lst
			  collect `(defun ,(intern (format nil "SET-STATE-~d" i)) ()
				     (setq tape-state ,i)))
	   do (eval i))))

(defun set-state-pc ()
  (setq tape-state num))

(defun set-stop ()
  (1++ stop))

(defmacro 1++ (var)
  `(setq ,var (1+ ,var)))

(defmacro 1-- (var)
  `(setq ,var (1- ,var)))

(defun clear-tape ()
  "clears tape and related vars. sets tape to only blank"
  (setq pc 0)
  (setq tape-length 0)
  (setq tape '(-1))
  (setq num (read-pc-pos))
  (setq stop 0)
  (read-tape))


(defun append-to-tape (&rest vals)
  "appends VALS to tape"
  (if vals
      (loop
	 for i in vals
	 do (progn
	      (1++ tape-length)
	      (setq tape (append tape (cons i nil)))))))

(defun read-pos (pos)
    "reads val from tap at point POS"
  (nth pos tape))

(defun read-pc-pos ()
  "reads val from tap at point PC"
  (read-pos pc))

(defun write-to-tape (val pos)
  "writes VAL to tape at point POS"
  (setf (nth pos tape) val))

(defun write-to-tape-pc (val)
  "writes val to tape at point of PC"
  (write-to-tape val pc))

(defun move-left ()
  (if (= pc 0)
    (progn
      (setq tape (cons -1 tape)))
    (1-- pc))
  (setq num (read-pc-pos)))

(defun move-right ()
  (when (= pc tape-length)
    (append-to-tape -1))
  (1++ pc)
  (setq num (read-pc-pos)))

(defun zero ()
  (setq pc 0))

(defun write-1 ()
  (write-to-tape-pc 1))

(defun write-0 ()
  (write-to-tape-pc 0))


;;; example:
;; (defstate (0)
;;   (0
;;    move-left)
;;   (1
;;    write-1
;;    move-right)
;;   (blank
;;    set-stop)
;;   )
(defmacro defstate (initial-state &body states)
  "macro for defining the state"
  `(defun ,(intern (format nil "STATE-~d" (first initial-state)))
       ()
     (setq num (read-pc-pos))
     (case num
       ,@(loop for state in states
	    collect `(,(first state)
		       (progn ,@(loop for ops in (rest state)
				   collect `(,ops)))))
       (t (set-stop)))
     (read-tape)))



(defun execute ()
  "executes the state given by the state variable"
  (setq num (read-pc-pos))
  (unless (=  stop 1)
    (eval (list (intern (format nil "STATE-~d" tape-state))))))

(defun execute-until-done ()
  "executes until the stop bit is set"
  (loop
     if (= stop 0)
     do (progn (format t  (execute))  (sleep 1))
     else
     do (return))) 

(defun read-tape ()
  "reads the tape and various other important variables"
  (format nil "stop: ~d ~% pc: ~d ~% current num: ~d ~%  state: ~d ~% length: ~d ~% pos: ~d ~% ~a ~a ~% ~S"
	  stop
	  pc
	  num
	  tape-state
	  tape-length
	  (read-pc-pos)
	  (make-string (* 2 pc) :initial-element #\-)
	  "!"
	  (map 'list (lambda (x) (if (= -1 x) 'B x)) tape)))	;defined -1 as #\B since -1 would take two char positions


(defun add-two-numbers (num1 num2)
  
  (progn				;state definitions
    (declare-states '(5 6 10 11 20 21))
    (defstate (5)			;seek to end
      (0
       move-right
       set-stop)
      (1
       move-right
       )
      (-1
       move-left
       set-state-6))
    (defstate (6)				;initial set, 0 or one
      (0					;a1[i] = 0
       move-left
       set-state-10)
      (1					;a1[i] = 1
       move-left
       set-state-11)
      (blank
       set-stop))

    (defstate (10)				;doesn't change anything
      (0
       move-left
       set-state-6)
      (1
       move-left
       set-state-6)
      (blank
       set-stop))

    (defstate (11)				;[i] = 1
      (0					;0 + 1 = 1
       write-1
       move-left
       set-state-6)
      (1					;1 + 1 = 10, carry
       write-0
       move-left
       set-state-20)
      (blank
       set-stop))

    (defstate (20) ;carry from previous 
      (0
       move-left
       set-state-11)
      (1
       move-left
       set-state-21)
      (blank
       set-stop))
    (defstate (21)
      (0					;0 + 10 = 10 = carry + 0
       write-0
       move-left
       set-state-20)
      (1					;1 + 10 = 11 = carry + 1
       write-1
       move-left
       set-state-20
       ))
    )
  (clear-tape)				;clear the tape
  (loop				   ;initialise the numbers on the tape
     for i in num1 ;in binary, with bit [i] of each number being next to each other
     for j in num2 ;1010 + 1011 = (1 1) (0 0) (1 1) (0 1), each group represents a bit position
     do (progn (append-to-tape i)
	       (append-to-tape j)))
  
  (append-to-tape -1)			;blanks are used to delimit the work area
 
  (move-right)				;starting position
  (set-state-5)				;starting state
  ;; (read-tape)
  (execute-until-done)			;run the program
)

(add-two-numbers '(1 0 1 0) '(0 1 0 1))
