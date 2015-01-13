# GeneticDesignsCode
Code for North Carolina II

### Description

Function to compute the North Carolina II Design.

### Usage

```{r eval=F}
carolinaII(set,male,female,replication,y,name.y)
```
### Arguments

```
set        : column of set values
male       : column of the male values
female     : column of the female values
replication: column of replication values 
y          : column of response variable
name.y     : response variable name
```

### Details

The North Carolina Genetic Design (bibliography) is a weight free index.

### Value
Measure the heredability of the progenie
The output is a list of data frames and values
```
cuadro : Df ,Mean Sq,Expect Mean Sq, F value, Pr(>F) 
var.Am : 
var.Af : 
var.D  : 
```

### Example

```{r eval=F}
# Run the function.
#yield :response variable
carolinaII(set,male,female,replication,yield,"yield")
```
