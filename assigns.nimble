# Package

version       = "0.8.2"
author        = "metagn"
description   = "syntax sugar for assignments"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.0.0"

when (NimMajor, NimMinor) >= (1, 4):
  when (compiles do: import nimbleutils):
    import nimbleutils
    # https://github.com/metagn/nimbleutils

task docs, "build docs for all modules":
  when declared(buildDocs):
    buildDocs(gitUrl = "https://github.com/metagn/assigns", extraOptions = "--path:src")
  else:
    echo "docs task not implemented, need nimbleutils"

task tests, "run tests for multiple backends":
  when declared(runTests):
    runTests(backends = {c, js, nims}, optionCombos = @[""])
  else:
    echo "tests task not implemented, need nimbleutils"
