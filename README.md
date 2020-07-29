quickscript-test
================
Tests for quickscript

*Notice:* The tests are run with whatever version of quickscript DrRacket uses.
For example, my setup consists in disabling the default `quickscript` collection in DrRacket
and installing a `quickscript-git` collection that in sync with the development branch.
`quickscript-test` will thus test `quickscript-git`, not `quickscript`.
This is also why the non-DrRacket-based tests (such as `library.rkt`) are not in quickscript-test,
because the `(require quickscript)` line would require a different version of quickscript.

