# GeneticDesignsCode
Code for genetic designs LineByTester, North Carolina I and North Carolina II

### Description

Function to compute the Elston index.

### Usage

```{r eval=F}
carolinaII(set,male,female,replication,y,name.y)
```
### Arguments

```
set        : column with the set values
male       :
female     : column with the female values
replication: column with replication values 
y          : response variable
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
carolinaII(set,male,female,replication,yield,"yield")
```
