VERBOSE := false
ifeq (${VERBOSE},false)
.SILENT:
endif

.ONESHELL:
SHELL := /bin/bash

MAKEFLAGS += --always-make

unexport AWS_DEFAULT_REGION AWS_SECRET_ACCESS_KEY AWS_ACCESS_KEY_ID

ifneq ($(wildcard .env),)
include .env
export $(shell sed 's/=.*//' .env)
endif

ENV ?= dev
IMAGE := slope
TAG := latest
BASE_TAG := latest
TAG_LATEST := 1
REPO := $(shell echo ${ENV}-${IMAGE} | tr [:upper:] [:lower:])

define aws
export AWS_ACCESS_KEY_ID=${AWS_ACCESS_KEY_ID}
export AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY}
export AWS_DEFAULT_REGION=${AWS_DEFAULT_REGION}
aws
endef

AWS_ACCOUNT = $(shell $(aws) sts get-caller-identity --query Account --output text)
ECR_SERVER = ${AWS_ACCOUNT}.dkr.ecr.${AWS_DEFAULT_REGION}.amazonaws.com
ECR_REGISTRY = ${ECS_SERVER}

BAMBOO := false
ARTIFACT_DIR := $(realpath ..)
CLOUD_VERSION := aws

PORT := 2800
export DOCKER_BUILDKIT := 1
export BUILDKIT_PROGRESS := plain

ifeq (${INTERACTIVE},true)
DOCKER_FLAGS += -it --entrypoint bash
endif

# Operations for Multiprobe/AWS version
run:
	docker run --rm \
		-p ${PORT}:${PORT} \
		${DOCKER_FLAGS} \
		${IMAGE}:${TAG} 

deploy:
	if ${BAMBOO}; then
		docker load < ${ARTIFACT_DIR}/${IMAGE}.tar.gz
	fi
	$(MAKE) push-image

build:
	docker build \
		-f Dockerfile-base \
		-t ${IMAGE}-base:${TAG} \
		--build-arg cloud_version=${CLOUD_VERSION} \
		.
		
	docker build \
		-t ${IMAGE}:${TAG} \
		--build-arg BASE_IMAGE=${IMAGE}-base \
		--build-arg BASE_TAG=${TAG} \
		--build-arg PORT=${PORT} \
		.
	if ${BAMBOO}; then
		docker save ${IMAGE}:${TAG} | gzip > ${ARTIFACT_DIR}/${IMAGE}.tar.gz
	fi 

create-ecr-repo:
	if ! aws ecr describe-repositories --repository-names=${REPO}; then \
		echo "Creating repository ${REPO}"; \
		aws ecr create-repository --repository-name=${REPO}
	fi

push-image: create-ecr-repo
	aws ecr get-login-password --region ${AWS_DEFAULT_REGION} \
		| docker login --username AWS --password-stdin ${ECR_SERVER}
	docker tag ${IMAGE}:${TAG} ${ECR_SERVER}/${REPO}:${TAG}
	docker push ${ECR_SERVER}/${REPO}:${TAG}
	docker rmi ${ECR_SERVER}/${REPO}:${TAG}

pull-image:
	aws ecr get-login-password --region ${AWS_DEFAULT_REGION} \
		| docker login --username AWS --password-stdin ${ECR_SERVER}
	docker pull ${ECR_SERVER}/${REPO}:${TAG} 

# Operations for MAP version
build_base_map:
	docker build \
		-f Dockerfile-base \
		-t code-registry.emsl.pnl.gov/multiomics-analyses/slope/slope-app/base:${BASE_TAG} \
		--build-arg cloud_version=map \
		.
		
build_top_map:
	docker build \
		-t code-registry.emsl.pnl.gov/multiomics-analyses/slope-app:${TAG} \
		--build-arg BASE_IMAGE=code-registry.emsl.pnl.gov/multiomics-analyses/slope/base \
		--build-arg BASE_TAG=${BASE_TAG} \
		--build-arg PORT=${PORT} \
		.

login_map:
	docker login code-registry.emsl.pnl.gov

push_base_map: login_map
	docker push "code-registry.emsl.pnl.gov/multiomics-analyses/slope-app/base:${BASE_TAG}"

	@if [ ${TAG_LATEST} = 1 ]; then\
		docker tag "code-registry.emsl.pnl.gov/multiomics-analyses/slope-app/base:${BASE_TAG}" "code-registry.emsl.pnl.gov/multiomics-analyses/slope-app/base:latest";\
		docker push "code-registry.emsl.pnl.gov/multiomics-analyses/slope-app/base:latest";\
  fi

push_top_map: login_map
	docker push "code-registry.emsl.pnl.gov/multiomics-analyses/slope-app:${TAG}"

	@if [ ${TAG_LATEST} = 1 ]; then\
		docker tag "code-registry.emsl.pnl.gov/multiomics-analyses/slope-app:${TAG}" "code-registry.emsl.pnl.gov/multiomics-analyses/slope-app:latest";\
		docker push "code-registry.emsl.pnl.gov/multiomics-analyses/slope-app:latest";\
	fi

push_map: push_base_map push_top_map

.PHONY: up
up:
	TAG=${TAG} docker-compose up --profile ${PROFILE}
test: 
	echo "This is a test!!!"

print-%: ; @echo $* = '$($*)' from $(origin $*)