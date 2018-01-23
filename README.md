# FRED
R functions to retrieve data from FRED

These functions were designed to retrieve data from [FRED](https://fred.stlouisfed.org/) in R. You'll need `rjson` and a valid API key ([see here](https://research.stlouisfed.org/useraccount/registerto)) run them.

`iFRED` returns a list with basic information about a given `sid`.

`qFRED` returns a matrix with the data itself.

More details [here](http://ordrespontane.blogspot.fr/2018/01/querying-fred-from-r.html).
