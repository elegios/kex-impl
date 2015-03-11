#!/bin/bash
ts=1000
echo "$ts $ts $(seq 1 $(echo "$ts*$ts" | bc)) $ts $ts $(seq 1 $(echo "$ts*$ts" | bc))"