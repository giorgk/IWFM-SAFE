RoadMap
===
In this example there are 3 main [matlab live scripts](https://www.mathworks.com/help/matlab/matlab_prog/create-live-scripts.html)

1. __ModelConfiguration.mlx__ <br/>
This is used to setup the finite element grid and other geometry data and input files of this example.

2. __RunSimulation.mlx__ <br/>
This is used to run the the model and process the results so that one can iterate the run faster inside Matlab
3. __SW_GW_Calc.mlx__ <br/>
This is the main file that calculates the Streamwater - groundwater interaction using IWFM and SAFE

Model info
---
The input files paths assume that the executables are launched from this folder.<br/>
e.g.
```
PreProcessor2015_D.exe Preprocessor\Ex1_Preprocessor.in
Simulation2015_D.exe Simulation\Ex1_Simulation.in
``` 
