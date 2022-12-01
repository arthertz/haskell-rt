![image](https://user-images.githubusercontent.com/58437698/204404240-50338519-3f80-4627-ae6c-265eb5b8b99a.png)

# haskell-rt
A raytracer for my advanced functional programming class in fall 2022

## Features (WIP)

# Installation
Make sure you have cabal installed (tested on 3.8.1.0) and a recent version of GHC (tested on 8.10.2)
Navigate to the project directory and enter `cabal run`
The project should build and output a series of .ppm files, which can be viewed with image viewers or online.

# PPM viewer
This is a PPM viewer that I know will work with the output of this program:
http://paulcuth.me.uk/netpbm-viewer/

# Benchmarking
You can use `cabal bench` to perform benchmarks on just the raytracer without the overhead from writing to PPM.
It is a good idea to make sure profiling is off first in the .cabal file.
The benchmark setup is found in `app/bench/Main.hs`
