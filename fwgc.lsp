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

Usage: clisp fwgc.lsp <cmd-line arguments>
       (to keep program running) clisp -repl fwgc.lsp <cmd-line arguments>

Dev Timeline:
Date           Modification
-----------    ------------
Nov 22 2016    Created main

TODO
 - Define initial array and variable for state tracking
 - Design stack for 'seen before' and output
 - Create 'explore' recursive function
 - Create move functions
 - Create Output function
 - Make modular (multi-file?)
 - Docstring comments

|#

; main function
(defun main ( args )
    "(main args): emulate a main function, called with command-line args"
    (format t "~D command line arguments: ~A" (length args) args)

    ; initial state: 0 0 0 0 (0)
    ; final state: 1 1 1 1 (15)
    ; list of totals
    ; keep list of states for printout?
)

; define the 'explore' recursive function (look for multi-function program)
#|  
(defun explore (curr_state)
    ;base cases
        if curr_total == goal_total (15)
            return success
        if dangerous state or seen before
            return failure

    ; moves
    ; while returning success, store moves on stack for output
    ; also figure out where things will be added for 'seen before'
        move F
        if explore (curr_state) == success
            return success
        if F and W same side
            move F and W
            if explore (curr_state) == success
                return success
        if F and G same side
            move F and G
            if explore (curr_state) == success
                return success
        if F and C same side
            move F and C
            if explore (curr_state) == success
                return success
       
        return failure
)
|#

#|
(defun moveF (curr_state, curr_total)
    if 1st elm = 0
        change to 1, add 8 to curr_total
    else
        change to 0, subtract 8
)
|#

#|
(defun moveFW
    if 2nd elm = 0
        change to 1, add 4 to curr_total
    else
        change to 0, subtract 4
)
|#

#|
(defun moveFG
    if 3rd elm = 0
        change to 1, add 2 to curr_total
    else
        change to 0, subtract 2
)
|#

#|
(defun moveFC
    if 4th elm = 0
        change to 1, add 1 to curr_total
    else
        change to 0, subtract 1
)
|#

; call the main function, passing command-line arguments
(main *ARGS*)
