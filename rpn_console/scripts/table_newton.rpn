@silent

"Insert a natural number: " print
scan toNumber clone >n
2 ^ toString length ++ >len

0 >i
$n ++ times {
	clear
	0 1 $n seq
	size times {
		qshift $i *
		$len colprint
	}
	newln
	$i ++ >i
}