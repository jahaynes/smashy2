# smashy2
Haskell Key-Value Store

This new version of https://github.com/jahaynes/smashy attempts to address some of the shortcomings of its predecessor,
however it is far from ready for general use.

Some features I hope to implement (above and beyond the original smashy) are:
* 'Bucketing' the data for cache-friendliness
* Deletes
* A growth model which is cleverer than just doubling when full
