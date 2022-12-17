#!/bin/bash

# description: - tag, commit and deploy 'wadashboard' Docker image to Amazon ECR (IWMI)

cd "$(dirname "$(realpath "$0")")";

if [ "$1" == "" ]
  then
    echo "A version tag is required to run this script"
    exit 1
fi

git tag "$1" -m "Release $1"
git commit -am "$1" && git push --tags

docker build -t wadashboard .
docker tag wadashboard:latest 643578423538.dkr.ecr.af-south-1.amazonaws.com/wadashboard:latest
aws ecr get-login-password --region af-south-1 | docker login --username AWS --password-stdin 643578423538.dkr.ecr.af-south-1.amazonaws.com
docker push 643578423538.dkr.ecr.af-south-1.amazonaws.com/wadashboard:latest
