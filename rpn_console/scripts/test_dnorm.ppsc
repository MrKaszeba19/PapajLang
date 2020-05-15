@silent
@use(Math)
@use(String)

-5 0.25 5 seq
size times { qshift distNormStd }

statusln

"The values between -1 and 1 sigmas are " 
1 distNormStd -1 distNormStd - 100 * toString
"% of total."
2 times concat println
