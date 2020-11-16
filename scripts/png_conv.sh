#!/bin/bash

mogrify -path ./imgs -resize 8x8! -type GrayScale ./origins/*.png
mogrify -path ./dhash-imgs -resize 9x8! -type GrayScale ./origins/*.png
