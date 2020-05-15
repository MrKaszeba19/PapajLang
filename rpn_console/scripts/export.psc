@use(Math)
1 1 10 seq
all times { qshift 13 * }
clear
1 1 100 seq
all times { qshift isPrime ? if rem }
9 isPrime
all times { qshift isPrime }
1 1 100 seq
all times { qshift isPrime }
rem
qshift isPrime
clear
1 1 100 seq
all times { qshift isPrime }
clear
1 1 100 seq
all times { qshift isPrime ? unless rem }
clear
1 1 100 seq
all times { qshift clone ? unless rem }
1 1 100 seq
all times { qshift clone isPrime ? unless rem }
