fun{ 2 31 ^ -- Math.isPrime } >expr

"TEST 1" println
@@expr clone
fun{ "yes" println } callIf
fun{ "no" println } callUnless

"TEST 2" println
fun{ "yes" println } @@expr callIf
fun{ "no" println } @@expr callUnless
