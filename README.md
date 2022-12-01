![image](https://user-images.githubusercontent.com/58437698/204949907-a5dd6fb4-44a8-4c00-bb91-aaac10ec60f6.png)

# haskell-rt
A raytracer for my advanced functional programming class in fall 2022

## Features (WIP)

# Installation
![image](https://user-images.githubusercontent.com/58437698/204949921-10e05c76-ae6d-4816-9b40-66c627568932.png)

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
