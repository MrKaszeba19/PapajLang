fun{
	clone 0 = ?
	if { rem 1 } 
	else { clone -- @@factorial * }
} >factorial


0 1 10 seq
size times {
	qshift @@factorial
}