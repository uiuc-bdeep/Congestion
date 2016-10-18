#!/bin/bash

pushd /data/Congestion
tar -czf /tmp/stream.tar.gz stream
tar -czf /tmp/stores.tar.gz stores

/usr/local/bin/aws s3 cp /tmp/stream.tar.gz s3://bdeep-concourse/VOT/
/usr/local/bin/aws s3 cp /tmp/stores.tar.gz s3://bdeep-concourse/VOT/

popd
