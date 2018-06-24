@silent
"Type a number: " print scan 
clone _n vset
0 _i vset
_n vget ++ times {
	clear
	0 1 _n vget seq
	size times { 
		qshift _i vget *
		8 colprint
	}
	newln
	_i vget ++ _i vset	
}