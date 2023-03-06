## SLOPER top image which simply copies the source and exposes the correct port.
## App dependencies are all installed in the base package.

# Change this when we bump versions, or if you have some test version of the
# base container you can specify --build-arg base_tag=<yourtag> in docker run.
ARG base_tag=0.0.1

FROM code-registry.emsl.pnl.gov/multiomics-analyses/SLOPE-app/base:$base_tag

# copy app source
COPY . .

# Make this shiny app available at port 8300
EXPOSE 8301

# Launch App
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 8301, launch.browser = FALSE)"]
