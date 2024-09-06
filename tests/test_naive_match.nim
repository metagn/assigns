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
  
  proc flatMap[T](x: Option[Option[T]]): Option[T] =
    match x:
    of Some(Some(val)):
      result = some(val)
    else:
      result = none(T)

  when not defined(assignsMatchBreakpoint):
    proc flatMapRec[T](x: Option[T]): auto =
      match x:
      of Some(val):
        when val is Option:
          result = flatMapRec(val)
        else:
          result = some(val)
      else:
        result = none(typeof(flatMapRec(x).get))
  
  check:
    flatMap(some some 3) == some(3)
    flatMap(some none int) == none(int)
    flatMap(none(Option[int])) == none(int)
    flatMap(some some some 3) == some(some 3)
    flatMap(some some none int) == some none(int)
    flatMap(some none(Option[int])) == none(Option[int])

  when not defined(assignsMatchBreakpoint):
    check:
      flatMapRec(some some 3) == some(3)
      flatMapRec(some none int) == none(int)
      flatMapRec(none(int)) == none(int)
      flatMapRec(some some some 3) == some(3)
      flatMapRec(some some none int) == none(int)
      flatMapRec(some none(Option[int])) == none(int)
