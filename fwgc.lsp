#|
        ***** fwgc.lsp *****

Solves the farmer, wolf, goat, cabbage problem implementing a state space search
approach (Depth-first search). The program outputs each state in the solution 
path. DFS explores each potential solution path as deep as possible until it 
reaches the goal state or an illegal state (where it must backtrack). The goal 
state is the farmer and all his possessions on the right bank. Illegal states 
include the wolf and goat, or the goat and cabbage alone on a bank without the
farmer, as well as states that have already been visited.

*explanation of how the state is represented and how the search is done* 

Author: Aaron Alphonsus, Dr. John M. Weiss
Class:  CSC461 Programming Languages
Date:   30 November 2016

Usage: clisp (to open Lisp prompt)
	   (load 'fwgc)
	   >  (fwgc)

Dev Timeline:
Date           Modification
-----------    --------------------------------------------
Nov 22 2016    Created main
Nov 28 2016	   Restructured program - created fwgc function
Nov 29 2016    Recursion works. Solution path list created.

TODO
 - Create move list
 - Create Print function 
 - Docstring comments
|#

; fwgc function
(defun fwgc ()
    "(main args): emulate a main function, called with command-line args"
    ;(format t "~D command line arguments: ~A" (length args) args)

    ; initial state: 0 0 0 0 (0)
    ; final state: 1 1 1 1 (15)
    ; keep list of states for printout? Nope. Print path backwards, switching
		; left and right bank
	
    ;(setf curr-state '(0 0 0 0))
    ;(setf prev-move )
	;(write-line "Hello, World!")

    ( let 
        (
            (curr-state '(0 0 0 0))
            (prev-move '-1)
            (path-list nil)
        )
	    ;(explore curr-state prev-move)
        (explore curr-state prev-move path-list)
    )    
)


; define the 'explore' recursive function (look for multi-function program)

(defun explore (curr-state prev-move path-list)
;(defun explore (curr-state prev-move)
    
    ;(write prev-move)
    ;(write (nth 0 curr-state))
    ;(nth 1 curr-state)

    ;(Print curr-state)
    ;(Print prev-move)

	; base cases
    ; if curr-state == goal_state (15) 
        ; return success
    ; if dangerous state 
        ; return failure
    (cond 
        ; goal state
        ((equal curr-state '(1 1 1 1)) 
            (setf path-list (cons curr-state path-list)) 
            (print (reverse path-list)) t
        )
        ;((equal curr-state '(1 1 1 1)) (Print curr-state) (Print prev-move) t)
        
        (
         ; dangerous states
         (or
            ; wolf and goat without farmer 
            (and 
                (equal (nth 1 curr-state) (nth 2 curr-state)) 
                (not (equal (nth 0 curr-state) (nth 1 curr-state)))
            )
            ; goat and cabbage without farmer
            (and 
                (equal (nth 2 curr-state) (nth 3 curr-state)) 
                (not (equal (nth 0 curr-state) (nth 2 curr-state)))
            )
         )  nil
        )

        ; (t transition-rules curr-state nil)
        ((transition-rules curr-state prev-move path-list) 
            (setf path-list (cons curr-state path-list)) t
        )
        #|((transition-rules curr-state prev-move) 
            (Print curr-state) (Print prev-move) t
        )|#
    )
)

; will be called instead of print
(defun print-path (path-list)




)

(defun transition-rules (curr-state prev-move path-list)
;(defun transition-rules (curr-state prev-move)

    (setf path-list (cons curr-state path-list))
    (cond 
        ; if prev-move != 0
        ; move F (switch first element: 0 to 1 or 1 to 0)
        ; if explore (curr-state, 0) == success
            ; return success

        ((and
            (not (= prev-move '0)) 
            (explore (list (toggle (nth 0 curr-state))
                           (nth 1 curr-state)
                           (nth 2 curr-state)
                           (nth 3 curr-state)
                     ) '0 path-list)
         ) t 
        )

        ; if prev-move != 1 & F and W same side
        ; move F and W (switch first & second element: 0 to 1 or 1 to 0)
        ; if explore (curr-state, 1) == success
            ; return success

        ((and 
            (not (= prev-move '1)) 
            (equal (nth 0 curr-state) (nth 1 curr-state))
            (explore (list (toggle (nth 0 curr-state))
                           (toggle (nth 1 curr-state))
                           (nth 2 curr-state)
                           (nth 3 curr-state)
                     ) '1 path-list)
         ) t
        )

        ; if prev-move != 2 & F and G same side
        ; move F and G (switch first & third element: 0 to 1 or 1 to 0)
        ; if explore (curr-state, 2) == success
            ; return success

        ((and 
            (not (= prev-move '2)) 
            (equal (nth 0 curr-state) (nth 2 curr-state))
            (explore (list (toggle (nth 0 curr-state))
                           (nth 1 curr-state)
                           (toggle (nth 2 curr-state))
                           (nth 3 curr-state)
                     ) '2 path-list)
         ) t
        )

        ; if prev-move != 3 & F and C same side
        ; move F and C (switch first & fourth element: 0 to 1 or 1 to 0)
        ; if explore (curr-state, 3) == success
            ; return success

        ((and 
            (not (= prev-move '3)) 
            (equal (nth 0 curr-state) (nth 3 curr-state))
            (explore (list (toggle (nth 0 curr-state))
                           (nth 1 curr-state)
                           (nth 2 curr-state)
                           (toggle (nth 3 curr-state))
                     ) '3 path-list)
         ) t
        )
    )
#|    
    ; moves
    ; while returning success, store moves on stack for output
    ; also figure out where things will be added for 'seen before'  
   
    return failure
|#

)

(defun toggle (bank)
   (cond ((equal bank 1) 0)
             ((equal bank 0) 1)))