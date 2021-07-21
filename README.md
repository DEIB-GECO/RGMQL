# RGMQL
API for calling interactively the GMQL Engine from R-Bioconductor

[RGMQL on Bioconductor](https://www.bioconductor.org/packages/release/bioc/html/RGMQL.html)

## Requirements

The library requires the following:
* R version 3.4.2 or higher
* Java version 1.8 or higher
* The JAVA_HOME enviroment variable set

It is recommended the use ot the latest version of RStudio.

## Structure
```
RGMQL/
|-- Example of workflows/
|-- R/
|-- inst/
|   |-- example/
|   |-- NEWS
|-- man/
|-- vignettes/
|   |-- RGMQL-vignette.R
|   |-- RGMQL-vignette.Rmd
|   |-- RGMQL-vignette.html
|   |-- american-medical-association-no-et-al.csl
|   |-- bibliography.bib
|   |-- ....
|-- DESCRIPTION
|-- NAMESPACE
|-- README.md
```

- [Example of workflows](Example%20of%20workflows)/ folder containig various use cases
- [R](R)/ folder containing all the script files
- [inst](inst)/ folder containing all files that should be copied into the installed R package folder.
- [inst/example](inst/example) folder containing the datasets and other files in order to run correctly all the test.
- [inst/NEWS](inst/NEWS) file report the feature/changes/bugfix for each version. 
- [man](man)/ folder containing R documentation files
- [vignettes](vignettes)/ folder containing all the files used to generate correctly the vignette
- [vignettes/RGMQL-vignette.R](vignettes/RGMQL-vignette.R) long-form guide to your package
- [vignettes/RGMQL-vignette.Rmd](vignettes/RGMQL-vignette.Rmd) the vignettes in markup language
- [vignettes/RGMQL-vignette.html](vignettes/RGMQL-vignette.html) the vignettes in html
- [vignettes/bibliography.bib](vignettes/bibliography.bib) the bibliographic or list of reference file used in vignette
- [vignettes/american-medical-association-no-et-al.csl](vignettes/american-medical-association-no-et-al.csl) the citation style language used in vignette
- [DESCRIPTION](DESCRIPTION) this file store important/mandatory metadata about RGMQL package.
- [NAMESPACE](NAMESPACE) this file make your packages self-contained, ensuring that other packages won’t interfere with your code, that your code won’t interfere with other packages, and that your package works regardless of the environment in which it’s run.


## OSX Settings

#### before Catalina

Edit the `.bash_profile` and add the `JAVA_HOME` environment variable:

`export JAVA_HOME = <java_path>`

`export PATH=$PATH`

#### after Catalina

Since on macOS Catalina the default shell is Zsh we need to edit or create the .zsh file:

Edit the `.zsh` and add the `JAVA_HOME` environment variable:

`export JAVA_HOME = <java_path>`

`export PATH=$PATH`

At the end, in both cases, edit the `/etc/paths` and add:

`$JAVA_HOME/bin`


## Windows Settings

Create environment variable `JAVA_HOME`:

* Right click on _This PC_.
* click on _Advanced system settings_
* go to _Advanced_ tab an click on _evnironment variables_
* create a `JAVA_HOME` variable the jdk path

### Errors

Be aware that during a local-processing execution an error message
```{
Error in .jcall("RJavaTools", "Ljava/lang/Object;", "invokeMethod", cl, 
: org.apache.spark.SparkException: Job aborted due to stage failure: 
Task 0 in stage 7.0 failed 1 times, most recent failure: 
Lost task 0.0 in stage 7.0 (TID 59, localhost, executor driver): 
java.io.IOException: (null) entry in command string: null chmod 0644
```
may arise.

This happens because some Hadoop binary files are missing in Windows 64Bits.
In this case you need to:

* Open [DownGit](https://minhaskamal.github.io/DownGit/#/home)
* Paste the url `https://github.com/steveloughran/winutils/tree/master/hadoop-2.8.1` and download the **winutil-hadoop2.8.1**
* Create a directory (for example at ```C:\Program Files\hadoop\bin```. Use a path you wish
* Copy the files from the repository folder **hadoop-2.8.1** into the folder earlier created.
* Create environment variable HADOOP_HOME with value equal to the folder path you copied the binaries.

or

* Go to [https://github.com/steveloughran/winutils](https://github.com/steveloughran/winutils), download the repository
* Create a directory (for example at ```C:\Program Files\hadoop\bin```. Use a path you wish
* Copy the files from the repository folder **hadoop-2.8.1** into the folder earlier created.
* Create environment variable HADOOP_HOME with value equal to the folder path you copied the binaries.

