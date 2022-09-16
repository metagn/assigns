when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import assigns

import options

test "naive match works":
  proc isSomeYesOrNo[T](opt: Option[T]): string =
    match opt:
    of Some(_): "yes"
    else: "no"
  
  check isSomeYesOrNo(some 3) == "yes"
  check isSomeYesOrNo(none int) == "no"

  let x = match range[1..5](4):
  of 1, 2, 3: "below or equal to three"
  of 4, 5: "above three"
  else:
    # unreachable
    ""

  check x == "above three"

  proc fizzbuzz(n: int): string =
    match (n mod 3, n mod 5):
    of (0, 0): "FizzBuzz"
    of (0, _): "Fizz"
    of (_, 0): "Buzz"
    else: $n
  
  check:
    fizzbuzz(1) == "1"
    fizzbuzz(2) == "2"
    fizzbuzz(3) == "Fizz"
    fizzbuzz(4) == "4"
    fizzbuzz(5) == "Buzz"
    fizzbuzz(15) == "FizzBuzz"
