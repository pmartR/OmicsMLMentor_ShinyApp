## SLOPER top image which simply copies the source and exposes the correct port.
## App dependencies are all installed in the base package.

# Change this when we bump versions, or if you have some test version of the
# base container you can specify --build-arg base_tag=<yourtag> in docker run.

#ARG BASE_IMAGE=slope-base
#ARG BASE_TAG=latest

# Note ARGs are "forgotten" after FROM statement
#FROM ${BASE_IMAGE}:${BASE_TAG}

# Degnan - March 2026 - I'm updating the files so no need to rebuild the container
FROM code-registry.emsl.pnl.gov/multiomics-analyses/slope-app:0.0.5 

WORKDIR /srv/shiny-server/slope

COPY . .

ARG PORT=2800
EXPOSE ${PORT}
ENV PORT=${PORT}

# Add $PORT to shiny config file
RUN mkdir -p /etc/shiny-server && \
    envsubst < shiny-server.conf.tmpl > /etc/shiny-server/shiny-server.conf

WORKDIR /srv/shiny-server