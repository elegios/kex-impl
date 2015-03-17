#!/bin/bash
ts=999
echo "$ts $ts $(seq 1 $(echo "$ts*$ts" | bc)) $ts $ts $(seq 1 $(echo "$ts*$ts" | bc))"