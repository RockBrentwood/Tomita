0:
	S => 1
	x => 2
1:
	accept
2:
	[S -> x]
	T => 3
	S => 4
	x => 2
3:
	[S -> x T]
4:
	[E -> 1]
	E => 5
5:
	[T -> S E]

