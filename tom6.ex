0:
	S => 1
	NP => 2
	n => 3
	det => 4
1:
	accept
	PP => 5
	and => 6
	p => 7
2:
	VP => 8
	PP => 9
	and => 10
	v => 11
	p => 7
3:
	[NP -> n]
4:
	n => 12
5:
	[S -> S PP]
6:
	S => 13
	NP => 2
	n => 3
	det => 4
7:
	NP => 14
	n => 3
	det => 4
8:
	[S -> NP VP]
9:
	[NP -> NP PP]
10:
	NP => 15
	n => 3
	det => 4
11:
	S => 16
	NP => 17
	n => 3
	det => 4
12:
	[NP -> det n]
13:
	[S -> S and S]
	PP => 5
	and => 6
	p => 7
14:
	[PP -> p NP]
	PP => 9
	and => 10
	p => 7
15:
	[NP -> NP and NP]
	PP => 9
	and => 10
	p => 7
16:
	[VP -> v S]
	PP => 5
	and => 6
	p => 7
17:
	[VP -> v NP]
	VP => 8
	PP => 9
	and => 10
	v => 11
	p => 7

