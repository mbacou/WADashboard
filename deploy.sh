#!/bin/bash

# description: - tag, commit and deploy 'wadashboard' Docker image to Amazon ECR (IWMI)

cd "$(dirname "$(realpath "$0")")";

VERSION=`git describe --abbrev=0 --tags`
VERSION=(${VERSION//./ })
VNUM1=${VERSION[0]}
VNUM2=${VERSION[1]}
VNUM2=$((VNUM2+1))
TAG="$VNUM1.$VNUM2.0"

git tag -a "$TAG" -m "Release v$TAG"
git commit -am "Release v$TAG" && git push

aws ecr get-login-password --region af-south-1 | docker login --username AWS --password-stdin 643578423538.dkr.ecr.af-south-1.amazonaws.com
docker build -t wadashboard .
docker tag wadashboard:latest 643578423538.dkr.ecr.af-south-1.amazonaws.com/wadashboard:latest
docker push 643578423538.dkr.ecr.af-south-1.amazonaws.com/wadashboard:latest
