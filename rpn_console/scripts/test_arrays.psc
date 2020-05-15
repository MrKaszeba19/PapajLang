@use(Array)

1 1 20 seq all toArray >tab

15 1 7 seq size times {
	$tab 10 rand qshift setAt
}

"Array:  " print $tab println
"Length: " print $tab length println
