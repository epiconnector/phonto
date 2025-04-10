<img src="vignettes/images/logo.png" alt="drawing" width="300"/>

This repository is a fork of the original [ccb-hms/phonto](https://github.com/ccb-hms/phonto), which is licensed under the CC BY 4.0 license. We continue to develop and enhance the project here.

## PHONTO - PHenome ONTOlogy for NHANES

This package is designed to work with the Docker container available from <https://github.com/deepayan/nhanes-postgres>. That container can be obtained by running the code below. Once installed and started users can log in via a web browser to analyze NHANES data using any tools they would like. The data live in a SQL database and can be accessed by a variety of tools. We provide an interface via RStudio and this package works together with the [nhanesA package](https://github.com/cjendres1/nhanes) to support a wide variety of analyses. phonto provides a few vignettes and users can familiarize themselves with the [Quick Start vignette](https://ccb-hms.github.io/phonto/vignettes/cobalt_paper.html) in order to find out how to interact with the DB. More docs can be found [Phonto page](https://ccb-hms.github.io/phonto/)

### Start Docker

**1. Start Docker**

Start Docker on Mac or Linux

``` dockerfile
docker run --rm \
       --name nhanes-pg \
        -d \
	    -v <LOCAL_DIRECTORY>:/HostData \
        -p 8787:8787 \
        -p 2200:22 \
        -p 5432:5432 \
        -e 'CONTAINER_USER_USERNAME=test' \
        -e 'CONTAINER_USER_PASSWORD=test' \
         deepayansarkar/nhanes-postgresql:0.10.1
```

Start Docker on Windows

``` dockerfile
docker run --rm ^
       --name nhanes-pg ^
        -d ^
	    -v <LOCAL_DIRECTORY>:/HostData ^
        -p 8787:8787 ^
        -p 2200:22 ^
        -p 5432:5432 ^
        -e 'CONTAINER_USER_USERNAME=test' ^
        -e 'CONTAINER_USER_PASSWORD=test' ^
         deepayansarkar/nhanes-postgresql:0.10.1
```

**2. Log into Rstudio**

Log into RStudio via: <http://localhost:8787> and using the username set in the command above. In the above command, the username and password are set as `USER` and `PASSWORD`, respectively, but you can modify them if you prefer.

More details about the [NHANES Docker](https://github.com/deepayan/nhanes-postgres).

<br/>

### Installation

You can install the development version of `phonto` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")

devtools::install_github("ainilaha/phonto")
```

### Examples

This is a basic example which shows you how to solve a common problem: Testing to see if I can push to this directory (Teresa)
