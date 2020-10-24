# RGMQL
API for calling interactively the GMQL Engine from R-Bioconductor

[RGMQL on Bioconductor](https://www.bioconductor.org/packages/release/bioc/html/RGMQL.html)

## Requirements

The library requires the following:
* R version 3.4.2 or higher
* Java version 1.8 or higher
* The JAVA_HOME enviroment variable set

It is recommended the use ot the latest version of RStudio.

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

be aware that during execution a message  

`Error in .jcall("RJavaTools", "Ljava/lang/Object;", "invokeMethod", cl, : org.apache.spark.SparkException: Job aborted due to stage failure: Task 0 in stage 7.0 failed 1 times, most recent failure: Lost task 0.0 in stage 7.0 (TID 59, localhost, executor driver): java.io.IOException: (null) entry in command string: null chmod 0644`

may arise in some circustances .

This happens because some binary files are missing for Windows, in this case we need to:

* Open [DownGit](https://minhaskamal.github.io/DownGit/#/home)
* Paste the url `https://github.com/steveloughran/winutils/tree/master/hadoop-2.8.1` in above website, and download the **winutil-hadoop2.8.1**

or

* Go to https://github.com/steveloughran/winutils and download the repo and use **winutil-hadoop2.8.1**


* Create a directory (example `C:\Program Files\hadoop`) with a folder inside named `bin`
* Get those files and copy them inside the bin subfolder, (copy on `C:\Program Files\hadoop\bin`)
* Create environment variable `HADOOP_HOME` with the path containing those files.


