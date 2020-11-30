# Package

version       = "0.4.4"
author        = "hlaaftana"
description   = "syntax sugar for local definitions"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.0.0"

import os

task docs, "build docs":
  const
    gitUrl = "https://github.com/hlaaftana/defines"
    gitCommit = "master"
    gitDevel = "master" 
  for f in walkDirRec("src"):
    exec "nim doc --git.url:" & gitUrl &
      " --git.commit:" & gitCommit &
      " --git.devel:" & gitDevel &
      " --outdir:docs " & f
