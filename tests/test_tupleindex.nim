import unittest, assigns/tupleindex

test "backwards index tuples":
  let a = (1, 2, 3)
  check a[^1] == 3
  check a[^2] == 2

test "slice tuples":
  let a = (1, 2, 3, "hi", 4, 5)
  check a[3..^1] == ("hi", 4, 5)
