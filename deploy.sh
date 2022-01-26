#!/bin/bash

# description: - build and deploy 'wadashboard' Docker image to Amazon ECR (IWMI)

cd "$(dirname "$(realpath "$0")")";

aws ecr get-login-password --region af-south-1 | docker login --username AWS --password-stdin 643578423538.dkr.ecr.af-south-1.amazonaws.com
docker build -t wadashboard .
docker tag wadashboard:latest 643578423538.dkr.ecr.af-south-1.amazonaws.com/wadashboard:latest
docker push 643578423538.dkr.ecr.af-south-1.amazonaws.com/wadashboard:latest
