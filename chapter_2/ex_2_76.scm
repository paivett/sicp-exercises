; For the generic operations and explicit dispatch, adding a new type means rewritting the dispatch operation
; adding for every operation a new 'if' with the new type.
; Adding a new operation for all types means writing the new operation for every type, and then writing the
; the new generic dispatch operation with the if for every type.
; When using the data driven scheme, adding a new type means addint a new type, implementing all its operations and
; installing then. Neither the generic dispatch nor every other type must be touched. When adding a new operation
; all types must be edited to implement and install the new operation. Again the dispatch mechanism remains untouched.
; Finally, adding a new type for the message passing case is like the data directed case, we define the new type with its
; own dispatch for every operation, and nothing else changes. Adding a new operation requires agagin to edit every type to support it.
; For a system that constantly adds new types, using data directed or messages seems to be optimal, because changes limit to the
; new type and the rest of the system is not touched. If new operations are constantly added, there seems to be no optimal solution.
; Every typo must be edited to support this new operation.