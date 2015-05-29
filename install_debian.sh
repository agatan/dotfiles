#!/bin/bash

for i in ./etc/init/debian/*
do
    /bin/bash $i
done
