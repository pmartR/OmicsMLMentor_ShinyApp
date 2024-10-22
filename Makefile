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

build_base:
	docker build \
		-f Dockerfile-base \
		-t ${IMAGE}-base:${TAG} \
		--build-arg CLOUD_VERSION=${CLOUD_VERSION} \
		.
		
build_top:
	docker build \
		-t ${IMAGE}:${TAG} \
		--build-arg BASE_IMAGE=${IMAGE}-base \
		--build-arg BASE_TAG=${TAG} \
		--build-arg PORT=${PORT} \
		.

build:
	docker build \
		-f Dockerfile-base \
		-t ${IMAGE}-base:${TAG} \
		--build-arg CLOUD_VERSION=${CLOUD_VERSION} \
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

test: 
	echo "This is a test!!!"

print-%: ; @echo $* = '$($*)' from $(origin $*)