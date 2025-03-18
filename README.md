# SLOPE-app

<!-- badges: start -->
<!-- badges: end -->

SLOPE is a shiny application that serves as the frontend to the slopeR package.  
It provides an interface to the machine learning capabilities of slopeR, including 
expert mentor, which recommends machine learning models to use based on user 
goals and data.  Users can upload data, perform various preprocessing steps such 
as filtering, normalization, and imputation, before running desired machine learning 
methods.  Results, including performance metrics and visualizations, as well as a 
report are available for download at the end of the workflow.

The MAP project version is available at `https://map.emsl.pnnl.gov/app/slope`.  
The Multiprobe project version is available at `https://multiprobe.us/slope/`. 

# Running Locally

## 1.  Using R/Rstudio/Shiny
Install the required packages, this can be done with `renv`.  To install package 
with `renv`, first `install.packages("renv")`.  Then call `renv::restore()`.  
This will install all packages contained in the renv.lock file.  
See the [renv website](https://rstudio.github.io/renv/articles/renv.html) for 
more details.  Manual installation is possible by inspecting `global.R` in the 
Main_app folder and Predict_app folder.  

You may also need a [Python virtual environment](https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/#creating-a-virtual-environment) 
with the packages from requirements.txt installed if you plan on using/testing 
the cloud (minio) capabilities.  After setting up a virtual environment, you 
will need to point the `MINIO_CONFIG_PATH` to a yaml file that specifies the 
path to a python environment as well as minio connection information.  Examples 
of such files can be found in `cfg/`.  

Once all dependencies are installed, make sure calling `.libPaths()` displays 
the renv environment, then call `shiny::runApp()`.

## 2.  Using Docker
Either build the container as described in the development section, or 
pull it from gitlab (access to EMSL systems are required to pull from gitlab):

```bash
# docker pull
docker pull code-registry.emsl.pnl.gov/multiomics-analyses/slope:<version>`

# docker compose
TAG=<version> docker compose pull
```

Then run the docker container (examples are the map version):

```bash
# manually
docker run -v /absolute/path/to/cfg/minio_config.yml:/srv/shiny-server/slope/cfg/minio_config.yml -p 8300:8300 code-registry.emsl.pnl.gov/multiomics-analyses/slope-app:<version>

# docker compose
TAG=<version> docker compose up

# make
make up TAG=<version>
```

and navigate to https://127.0.0.1:8300/slope


# Development

We use [renv](https://rstudio.github.io/renv/articles/renv.html) to track 
dependencies.  The renv.lock file contains a list of dependencies and various 
details about them.  To install the dependencies, call `renv::restore()`.  This 
will install all the packages listed in renv.lock, which tracks all dependencies.  
To update the lockfile dependencies use `renv::snapshot()`.  See the `renv` 
documentation for more details.

## Docker Images

We maintain a 'base' container with all dependencies, defined in `Dockerfile-base` 
and a 'top' container defined in `Dockerfile`.  See those files for build 
arguments and the Makefile for entrypoints to build processes.

For general advice on developing with docker, see [this guide](./developer_guide.md).

### MAP Portal and AWS Version

The app has two versions that are able to pull data from cloud storage, one 
that pulls data from a running minio instance, and another that pulls data from AWS.  

The base Dockerfile takes a 'cloud_version' build argument that can be set to 
'map' or 'aws'.  The 'map' version is the default and pulls data from a running 
minio instance.  The 'aws' version pulls data from AWS.  The Makefile has 
various commands to build and push images to the appropriate repos for both versions.
For AWS versions, access to the AWS instance must be exported into the environment
if accessing data from the cloud database (fields for AWS_ACCESS_KEY_ID, 
AWS_SECRET_ACCESS_KEY, and AWS_DEFAULT_REGION)





