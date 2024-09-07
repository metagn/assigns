# Package

version       = "0.7.1"
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
    # old
    const
      gitUrl = "https://github.com/metagn/assigns"
      gitCommit = "master"
      gitDevel = "master" 
    for f in walkDirRec("src"):
      exec "nim doc --git.url:" & gitUrl &
        " --git.commit:" & gitCommit &
        " --git.devel:" & gitDevel &
        " --outdir:docs " & f

task tests, "run tests for multiple backends":
  when declared(runTests):
    runTests(backends = {c, js, nims}, optionCombos = @[""])
  else:
    echo "tests task not implemented, need nimbleutils"
