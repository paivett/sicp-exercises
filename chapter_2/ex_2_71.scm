; For n=5
;            *
;           / \
;          /   \
;         *     \
;        / \     \
;       /   \     \
;      *     \     \
;     / \     \     \
;    /   \     \     \
;   *     \     \     \
;  / \     \     \     \
; /   \     \     \     \
;A     8     C     D     F

; Where
; freq(A) = 1
; freq(B) = 2
; freq(C) = 4
; freq(D) = 8
; freq(E) = 16


; For n=10

;                           *
;                          / \
;                         /   \
;                        *     \
;                       / \     \
;                      /   \     \
;                     *     \     \
;                    / \     \     \
;                   /   \     \     \
;                  *     \     \     \
;                 / \     \     \     \
;                /   \     \     \     \
;               *     \     \     \     \
;              / \     \     \     \     \
;             /   \     \     \     \     \
;            *     \     \     \     \     \
;           / \     \     \     \     \     \
;          /   \     \     \     \     \     \
;         *     \     \     \     \     \     \
;        / \     \     \     \     \     \     \
;       /   \     \     \     \     \     \     \
;      *     \     \     \     \     \     \     \
;     / \     \     \     \     \     \     \     \
;    /   \     \     \     \     \     \     \     \
;   *     \     \     \     \     \     \     \     \
;  / \     \     \     \     \     \     \     \     \
; /   \     \     \     \     \     \     \     \     \
;A     8     C     D     F     G     H     I     J     K

; Where
; freq(A) = 1
; freq(B) = 2
; freq(C) = 4
; freq(D) = 8
; freq(E) = 16
; freq(F) = 32
; freq(G) = 64
; freq(H) = 128
; freq(I) = 256
; freq(J) = 512

; The most frequent tree will always be encoded using 1 bit, and the least frequent with n-1 bits (the height of the tree)
