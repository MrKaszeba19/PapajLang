@silent

fun{ -1 * } >invert
fun{
	0 = ?
	if TRUE
	else FALSE
} >toboolean

scan toNumber 
clone 
@@invert println
@@toboolean println