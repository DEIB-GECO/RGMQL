# RGMQL

## Settings

put scala in folder OSX /usr/local/
create symbolic links as instruction below

sudo ln -s /usr/local/scala-2.11.8 /usr/local/scala
sudo ln -s /usr/local/scala-2.11.8 /usr/local/share/scala

edit .bash_profile, add these

export SCALA_HOME=/usr/local/share/scala
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_79.jdk/Contents/Home
export PATH=$PATH

edit /etc/paths add these

/usr/local/scala/bin
$SCALA_HOME/bin
$JAVA_HOME/bin

this setting prevents warning in RStudio when creating scala interpreter object

cat: /no such file or directory

in Rstudio after create interpreter and call info function in R we got this warning

> rscala::scalaInfo(verbose = T)

Searching for a suitable Scala installation.
* FAILURE: 'scala.home' argument is NULL
* FAILURE: SCALA_HOME () environment variable
* ATTEMPT: Found a candidate (/usr/local/scala-2.11.8/bin/scala)
* SUCCESS: 'scala' in the shell's search path

don-t worry is ok!

(I don't know what happen with other R "editor" or using R through command line)

## for local "compiling"

I don't put the jar file in repository cause are ~ 100MB
after cloned or downloaded the git create a folder "inst" inside GMQL then create folder "java" inside it
we should have this structure:

GMQL
 |__ inst
       |__ java

then copy the jar file inside "java" or create a symbolic link
