import unittest, definesugar/tupleaccess

test "sliced tuples":
  let a = (1, 2, 3, "hi", 4, 5)
  check a[2..4] == (3, "hi", 4)
  check a[3..^1] == ("hi", 4, 5)
