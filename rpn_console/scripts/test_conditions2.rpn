@silent

"Type number 1: " print
scan toNumber >na
"Type number 2: " print
scan toNumber >nb

"Type what do you want to do: " println
\t "+ sum" String.concat println
\t "- difference" String.concat println
\t "* product" String.concat println
\t "/ quotient" String.concat println
scan toString >op

"+-*/" $op String.occurs 0 <= ?
if {
	"Wrong operand" println
} else {
	$op \/ = 
	$nb 0 = 
	and ?
	if {
		"You cannot divide by zero!" println
	} else {
		$na toString 
		$nb toString 
		$op
		2 times bind String.run toString >result
		"Result:" $result bind println
	}
}
