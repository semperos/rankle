# Rankle - Rank in Clojure

**Alpha** This repository currently represents a set of code spikes and thought
experiments (see [build status](https://gitlab.com/semperos/rankle_mirror/pipelines)).

* Array programming and rank-oriented functions are inspired by [J](http://www.jsoftware.com/).
* The column map and table abstractions are inspired by [Q](http://code.kx.com/q/).

Start by reading this repo's [Rankle](Rankle.ipynb) document.

## Contributors

* [Chris O'Donnell](https://github.com/codonnell)

## License

This repository includes slightly modified versions of Clojure core functions as
well as the [fsm-clj](https://github.com/fbeline/fsm-clj) library which are
[licensed](https://github.com/fbeline/fsm-clj/blob/master/LICENSE)

> under the Eclipse Public License either version 1.0 or (at your option) any later version.

The `com.semperos.rankle.util/defalias` macro was copied from
[jackknife](https://github.com/sritchie/jackknife/blob/03bbd8584878914d28b7ad31225700c3ed306b4c/src/jackknife/def.clj#L22)
and is [licensed](https://github.com/sritchie/jackknife/blob/master/LICENSE.txt)
under the Apache License, Version 2.0.

All other original source code in this repository is distributed under the
Mozilla Public License version 2.0.
