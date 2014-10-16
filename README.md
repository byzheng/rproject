rproject
========

This is a simple package to manage multiple projects in R. The folder structure looks like
* Root folder
 * project.ini
 * Project1
    * config.ini
    * Rcode
    * Resources
    * Results
 * Project2
    * config.ini
    * Rcode
    * Resources
    * Results

In the project.ini, all project will be listed
```
p1 = Project1
p2 = Project2
```
-----
To install:

the latest development version: 

```{r}
library(devtools)
install_github("byzheng/rproject")
```
