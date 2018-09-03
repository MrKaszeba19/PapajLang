@silent

// TEST 1

fun{
	5 >x
	$x println
	$y println

	$x 5 = ?
	if {
		6 >x
		"I exist" >y
		$x println
		$y println
	}

	$x println
	$y println
} >test

"Test 1" println
$x println
$y println
newln
@@test
newln
"Hello!" >y @@test
newln
$x println
$y println
newln

vclear

// TEST 2

fun{
	fun{
		6 >x
		"I exist too" >y
		$x println
		$y println
	} >help
	
	5 >x
	$x println
	$y println

	$x 5 = ?
	if @@help

	$x println
	$y println
} >test2

"Test 2" println
$x println
$y println
newln
@@test2
newln
"Hello!" >y @@test2
newln
$x println
$y println
newln