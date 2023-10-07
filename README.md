# viticolaR
A mechanistic compartment model to assist in fungicide decision support for 
downy mildew (_Plasmodia viticola_) infections in grapevines.  

## Getting started  
### Installing the package
Install the R package and dependency (`epiphytoolR`) from github using the following 
code

```r
install_github("https://github.com/PaulMelloy/viticolaR")
install_github("https://github.com/PaulMelloy/epiphytoolR")
```

### Format weather data  
The model only requires clean weather data formatted with `epiphytoolR::format_weather()`
function.
The package contains a clean dataset `nt_weather` which is already formatted using
this method.
See [example code](https://github.com/PaulMelloy/viticolaR/blob/main/data-raw/DATASET.R)
for how to format weather data from BOM sources, including m=imputing missing 
weather data.  

### Running the model  
```r
NT_DMod <- estimate_DM_PI(nt_weather)
```

Get a formatted output  

```
get_PI_dates(NT_DMod)
```
