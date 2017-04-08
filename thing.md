# How to convert hex number to position

h -> [x, y]
0 -> [0, 0]
1 -> [1, 0]
2 -> [2, 0]
3 -> [3, 0]
4 -> [4, 0]
5 -> [5, 0]
6 -> [6, 0]
7 -> [7, 0]
8 -> [0, 1]
9 -> [1, 1]
A -> [2, 1]

h -> [h % 8, Math.floor(h / 8)]