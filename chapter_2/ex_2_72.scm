; Let N be the number of symbols available, and each frequency being 1, 2, 4, ... , 2^(N-1)
; then the Huffman tree has heigth N-1. In order to encode the least frequent symbol, we must visit N-1 nodes.
; To encode the most frequent symbol, we start at the root, and potentially check both lists of symbols, one for each branch.
; One list has n-1 symbols, and the other has only 1, the most frequent one. After that check, we choost the correct branch, and upon
; After that, we reach the leaf at the next step, So we have done O(N) comparissons.
; For the least frequent symbol, we will have to go through the hole tree, so we will visit O(N-1) nodes.
; At each level of the tree, we 'loose' one symbol. For example, at the root, we must check O(N) symbols, but 
; at the inmediate next level, there are N-1 symbols, and so on. At each level i, we perform N-i comparissons.
; In total we do N + N-1 + N-2 + ... + 2, which is O(N^2) comparissons.
; To encode the nth symbol, we must perform N + N - 1 + ... + N - n comparissons.
; T(n) = N - ((n * n-1) / 2), which in the end is O(n^2)
; All this analysis is with a tree like exercise 2.71. If the tree has a different shape, this is not valid.