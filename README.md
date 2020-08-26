# isoclockR
Using isotopes for fun things.

# Installation

First we install the package. 

```{r}
#We need devtools installed
#install.packages(devtools)
require(devtools)
devtools::install_github("brandynlucca/isoclockR", auth_token="e89437b0ef77d567473ca6229d4a3a927703a1b0")
require(isoclockR)
```

Then we can create an `ISO` object (see: `?ISO` via command line in the R GUI/RStudio). 

```{r}
#Create ISO object
#Hypothetical animal with d15N sample
#d15N == dt == == 14.9 %o
#d0 == doi == 12.69 %o
#df == dfi == 17.59 %o
#lambda == lambda == 0.0172

random_animal <- ISOgen(data.names="d15N", doi=12.69, dfi=17.59, lambda=0.0172)
```

So we can look at how this object is generated with some metadata (currently still being fleshed out, of course).

```{r}
#Show object
random_animal
```

So we can see all of the variables necessary for the `isoclock(...)` from Klaasen *et al*. (2010). We can then put these into the equation:

```{r}
#Isoclock equation
#We can either set the object against itself
random_animal <- isoclock(random_animal)

#Or we can let it update itself.
isoclock(random_animal)
```

So we have now calculated the residence times, which are updated within the object. While parsing functions are being developed for a `summary(...)` function, these data can be viewed via:

```{r}
#Parse out data
random_animal@data
```

where values represent the isotope value, and residence represents the residence time in days. We can also put in arbitrary values, as well, without creating an ISO object at the start:

```{r}
#Isoclock without premade object
new_animal <- isoclock(doi=12.69, dfi=17.59, dt=14.9, lambda=0.0172, data.names="d15N")
new_animal
```
