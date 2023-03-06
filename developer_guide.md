## Developing Shiny Apps in Docker Container

During development, it is good to edit your code locally but run it inside the
docker environment.  Besides the obvious reason of making sure it runs in the 
environment it will be deployed in, this has a couple other advantages:

- You can install new packages and test them in the dockerized environment without having to rebuild the dependency image.
- You can update the lockfile in the deployment environment, which may require different package versions (hashes) than your local environment.

To develop inside the docker environment, we want to run the container and mount our project directory into the appropriate location inside the container.  For an example, I'll use the SLOPE base container*.  The command is:

```
docker run --rm -d \  
-v <absolute path to your project dir>:/srv/shiny-server \  
-p 3801:3801 \  
--name SLOPE \  
code-registry.emsl.pnl.gov/multiomics-analyses/SLOPE-app/base:0.1.2  
```

The --rm and -d are optional.  The -v flag is used to mount a local directory (in this case, all the app code in our project directory) into the docker container.  In this example, we are making sure the code in our project directory is available inside the container at /srv/shiny-server.  Any changes we make locally in our project directory will be reflected inside the container.  

(Optional) The -p flag specifies the port mapping, with the syntax <port on our machine>:<port inside the container>.  Basically, this is telling docker, we want to be able to access traffic being served at some location (port) inside the container, from some location on our machine.
Choose any ports that are not being used by <your machine port #>:<container port #>.  In the example I've chosen 3801:3801.  Alternatively, you can also use 3838 as the container port: <your machine port #>:3838, and it may allow you to skip spinning up the app in the container (more on this later).

We make a name for our running container so we can easily reference it `--name SLOPE`.  Finally, we specify which image we want to spin up `code-registry.emsl.pnl.gov/multiomics-analyses/SLOPE-app/base:0.1.2 `.

In a terminal, we should be able to see our running container:

```
docker ps | grep SLOPE

>>> 8dfeeeeb90f2   code-registry.emsl.pnl.gov/multiomics-analyses/SLOPE-app/base:0.1.2   "/init"                  6 minutes ago   Up 6 minutes   0.0.0.0:3801->3801/tcp             SLOPE
```

#### Running the app from within the container.

**If you used 3838 as the container port in the -p docker run argument**, you should be able to navigate to localhost:<your machine port #> in your browser and see the app.  The container that SLOPE is built off of (rocker/shiny) serves the app at port 3838 by default, this may not be the case if you are using a different image to build yours on top of.  

**If you specified the internal port**, we need to enter the container and tell it to run the app on that port.

First, enter the container in a shell using:

`docker exec -it SLOPE /bin/bash`  

You will be enter a bash shell inside the container (if your image has bash installed).  Then, navigate to the app directory (the place where you mounted everything with -v), in the case of SLOPE we should already be there (/srv/shiny-server).  Then, enter an R session and run the app with shiny::runApp():

`>>> R`

```
R version 4.1.1 (2021-08-10) -- "Kick Things"
Copyright (C) 2021 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin17.0 (64-bit)

> shiny::runApp("<relative path to your app>", port=<container port # specified in docker run>, host=0.0.0.0, launch.browser = F)
```

We specify port as the port we chose on the container side of -p in the docker run command.  The `host=0.0.0.0` argument is due to some funky stuff with docker which I do not fully understand.  Specifying '0.0.0.0' in host however tells shiny to make the app available at all connections.  Finally, we don't want to `launch.browser` because we are in a container that doesn't have one.

Now, in your local browser, navigate to localhost:<your machine port #> to see the app.

### Modifying the App

At this point, with the container running, modify your code as normal.  When you want to test changes, either refresh the app if you are running on the default 3838, or enter the container and re-run the shiny::runApp(...) command.

### Updating renv within the container.

Updating a lockfile so it is consistent across systems is a pain, since it depends on the R-version and system version (especially BioConductor packages).  We can update it in the container to make it consistent across developers:

1. Enter the container with docker exec -it \<container-name> /bin/bash
2. Enter an R session and make sure your .libPaths() is pointing to the renv library.  In SLOPE it is installed in the default library path.
3. Modify dependences however you wish and include references to them in the code or DESCRIPTION if you are using 'implicit' or 'explicit' renv snapshot types.
4. Test the changes by running the app within the container.
5. Snapshot the changes.

Changes to the lockfile will appear locally.  I have noticed changes to the hashes of the packages but not the remoteSHA, so the version appears to be the exact same.  This may be due to lockfile entries installed on a different OS.

```
>>> R

> .libPaths()
[1] "/usr/local/lib/R/site-library" "/usr/local/lib/R/library"
> install.packages("prompter")

... include library(prompter) in the code and test the app ...

> renv::snapshot(type="implicit")
The following package(s) will be updated in the lockfile:

# CRAN ===============================
- prompter     [* -> 1.1.0]

Do you want to proceed? [y/N]: y
```

----
*Note we are using the *base container*, since all we want is the dependencies to test out local code, without any extra code included in a full container.  You could do the container with code in it, just make sure you are running the app from the code you mounted from your local machine, not what already exists in the container.
