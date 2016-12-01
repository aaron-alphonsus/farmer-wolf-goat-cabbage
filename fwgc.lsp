#|
        ***** fwgc.lsp *****

Solves the farmer, wolf, goat, cabbage problem implementing a state space search
approach (Depth-first search). The program outputs each state in the solution 
path. DFS explores each potential solution path as deep as possible until it 
reaches the goal state or an illegal state (where it must backtrack). The goal 
state is the farmer and all his possessions on the right bank. Illegal states 
include the wolf and goat, or the goat and cabbage alone on a bank without the
farmer, as well as states that have already been visited.

The state is represented by a list of four elements, each element either having
a value of 0 or 1. 0 represents the item on the left bank, 1 represents the item
on the right bank. The program has a recursive function to emulate the DFS which
calls another function which keeps track of the state transition rules. After 
finding the solution path, the print solution makes use of the format function 
to produce the output in the desired format.

Author:     Aaron Alphonsus
Class:      CSC461 Programming Languages
Instructor: Dr. John Weiss
Date:       30 November 2016

Usage: clisp (to open Lisp prompt)
	   (load 'fwgc)
	   > (fwgc)

Dev Timeline:
Date           Modification
-----------    --------------------------------------------
Nov 22 2016    Created main
Nov 28 2016	   Restructured program - created fwgc function
Nov 29 2016    Recursion works. Solution path list created.
Nov 30 2016    Print function created. Documentation done.
|#

(defun fwgc ()
    "(fwgc): defines initial states and calls 'explore' recursive function"

    (let 
        (
            ; keeps track of items. 0 = Left Bank, 1 = Right Bank
            (curr-state '(0 0 0 0)) 
            ; keeps track of most recent move to avoid going back
            (prev-move '-1) 
            (path-list nil) ; list of states. Stores solution path
            (move-list nil) ; list of moves made.
        )
        (explore curr-state prev-move path-list move-list)
    )   
)

(defun explore (curr-state prev-move path-list move-list)
    "
        (explore curr-state prev-move path-list move-list): 
            Explores the state space. Checks if goal state has been reached or 
            if current state is a dangerous state. Returns t or nil 
            respectively.
    "
    
    (cond 
        ; goal state
        ((equal curr-state '(1 1 1 1)) 
            ; adds the final state to the path list
            (setf path-list (cons curr-state path-list)) 
            ; adds the final move to the move list
            (setf move-list (cons prev-move move-list))
            ; calls the print function with the path and move list  
            (print-solution (reverse path-list) (reverse move-list)) 
            t
        )
        
        ; checks for dangerous states
        ((or
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

        ; calls function which applies the state transition rules
        ((transition-rules curr-state prev-move path-list move-list) 
            t
        )
    )
)

(defun transition-rules (curr-state prev-move path-list move-list)
    "
        (transition-rules curr-state prev-move path-list move-list): 
            Defines the state transition rules. Checks if moves can be made and 
            makes recursive calls
    "

    ; dangerous state check passed. Adds state to the path list and move to the
    ; move list
    (setf path-list (cons curr-state path-list))
    (setf move-list (cons prev-move move-list))
    
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
                     ) '0 path-list move-list)
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
                     ) '1 path-list move-list)
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
                     ) '2 path-list move-list)
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
                     ) '3 path-list move-list)
         ) t
        )
    )
)

(defun toggle (bank)
   "
        (toggle bank): 
            helper function to change bank state depending on current bank state
   "
   (cond ((equal bank 1) 0)
             ((equal bank 0) 1))
)

(defun print-solution (path-list move-list)
    "
        (print-solution path-list move-list): 
            Print function uses the path list and move list to print out each 
            stage in the solution path.
    "
    (let 
        (
            ; local variable to build list of items. Makes it easy to print
            (items nil) 
        )

    ; Header
    (format t "Left Bank          Right Bank          Action~%")
    (format t "_________          __________          ______~%")

    ; loops until the size of the move list
    (dotimes (i (length move-list)) 
        
        ; Left Bank
        ; Checks which positions in list are set to 0, indicating left bank. 
        ; Adds it to the items list to be printed out
        (cond
            ((equal (nth i path-list) '(1 1 1 1)) (format t "~19a" "-"))
            (t  
                (cond
                    ((equal (nth 0 (nth i path-list)) '0) 
                        (setf items (cons 'f items)))
                )
                (cond
                    ((equal (nth 1 (nth i path-list)) '0) 
                        (setf items (cons 'w items)))
                )
                (cond
                    ((equal (nth 2 (nth i path-list)) '0) 
                        (setf items (cons 'g items)))
                )
                (cond
                    ((equal (nth 3 (nth i path-list)) '0) 
                        (setf items (cons 'c items)))
                )   
                (format t "~19a" (reverse items))
                (setf items nil)
            )
        )

        ; Right Bank
        ; Checks which positions in list are set to 1, indicating right bank. 
        ; Adds it to the items list to be printed out
        (cond
            ((equal (nth i path-list) '(0 0 0 0)) (format t "~20a" "-"))
            (t 
                (cond
                    ((equal (nth 0 (nth i path-list)) '1) 
                        (setf items (cons 'f items)))
                )
                (cond
                    ((equal (nth 1 (nth i path-list)) '1) 
                        (setf items (cons 'w items)))
                )
                (cond
                    ((equal (nth 2 (nth i path-list)) '1) 
                        (setf items (cons 'g items)))
                )
                (cond
                    ((equal (nth 3 (nth i path-list)) '1) 
                        (setf items (cons 'c items)))
                )  
                (format t "~20a" (reverse items))
                (setf items nil)
            )
        )

        ; Action
        ; Checks the move, and state of the farmer to print out the correct
        ; action
        (cond
            ((equal (nth i move-list) '-1) (format t "*start state*~%"))
            
            ((and (equal(nth i move-list)'0) (equal(nth 0 (nth i path-list))'1))
                (format t "farmer goes across alone~%"))
            ((and (equal(nth i move-list)'0) (equal(nth 0 (nth i path-list))'0))
                (format t "farmer returns alone~%"))
                
            ((and (equal(nth i move-list)'1) (equal(nth 0 (nth i path-list))'1))
                (format t "farmer takes wolf across~%"))
            ((and (equal(nth i move-list)'1) (equal(nth 0 (nth i path-list))'0))
                (format t "farmer returns with wolf~%"))
                
            ((and (equal(nth i move-list)'2) (equal(nth 0 (nth i path-list))'1))
                (format t "farmer takes goat across~%"))
            ((and (equal(nth i move-list)'2) (equal(nth 0 (nth i path-list))'0))
                (format t "farmer returns with goat~%"))

            ((and (equal(nth i move-list)'3) (equal(nth 0 (nth i path-list))'1))
                (format t "farmer takes cabbage across~%"))
            ((and (equal(nth i move-list)'3) (equal(nth 0 (nth i path-list))'0))
                (format t "farmer returns with cabbage~%"))
        )
    )
    (format t "~39a" " ")
    (format t "*** problem solved! ***") 
    )
)