0:
	[AP -> 1]
	NP => 1
	IP => 2
	d => 3
	n => 4
	AP => 5
1:
	accept
2:
	[NP -> IP]
3:
	[AP -> 1]
	IP => 6
	n => 4
	AP => 5
4:
	[IP -> n]
5:
	[AP -> 1]
	IP => 7
	a => 8
	n => 4
	AP => 5
6:
	[NP -> d IP]
7:
	[IP -> AP IP]
8:
	[AP -> AP a]

