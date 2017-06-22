# RGMQL

This package works with java 1.8 and scala 2.11.8

## Settings OSX

edit .bash_profile, add environment variable

export SCALA_HOME="<scala_path>"
export JAVA_HOME="<java_path>"
export PATH=$PATH

edit /etc/paths add these

$SCALA_HOME/bin
$JAVA_HOME/bin


## Settings Windows

create environment variable JAVA_HOME and SCALA_HOME



this settings prevents warning in RStudio when creating scala interpreter object

cat: /no such file or directory

in Rstudio after create interpreter and call info function in R we got this warning

> rscala::scalaInfo(verbose = T)

Searching for a suitable Scala installation.
* FAILURE: 'scala.home' argument is NULL
* FAILURE: SCALA_HOME () environment variable
* ATTEMPT: Found a candidate (/usr/local/scala-2.11.8/bin/scala)
* SUCCESS: 'scala' in the shell's search path

don-t worry is ok!

