# IWFM-SAFE

In this code the origianl IWFM code is modified to implement SAFE.
The starting version of IWFM the code is the 2015.0.1113. 

# Code modifications to implement SAFE

The SAFE method controls how the stream interacts with groundwater, therefore SAFE is a type of stream - groundwater connector.
The streamGWconnectors they all inherit from the base class `BaseStrmGWConnectorType`. Therefore in SAFE we added an extra component connector which extends the base class. This new connector was named as
 `StrmGWConnector_v411_Type`.

 The stream groundwater connector is called from the stream class therefore to avoid intervening the existing stream code we generated an additional AppSteam class 411 as well as a streamnode class 411. which both extend the basic classes.

 ## Modifications of the project files
 To make the code aware of the new classes, several changes needed in the project files. 
 ### Update the *.vfproj files.
 Each of the subprograms folders PreProcessor, Simulation, etc contain the a configuration file with the extension *.vfproj. Under the files section we add the relative paths of the new source files for the ProPorcessor, Simulation and Simulation_Parallel. The budget do not need to include the stream classes.

 ### Update existing IWFM classes
 The new classes have to be included into few core classes of IWFM.
 These are the following:    
- `Package_AppStream.f90`</br>
Here we included the new stream_v411 and added one function which returns the version number in integer form (`GetINTVersion`). This file is responsible to select the appropriate version. There are multiple SELECT CASE statements where we add the option 4.11. 
- `Class_BaseStrmGWConnector.f90` </br>
The Safe calculations require the horizontal and vertical hydraulic conductivity of the groundwater nodes that the stream nodes are connected to. In this file we added the interface of the abstract method (`Set_KH_KV`) to the base class that with the purpose of passing the conductivities from the AppGrid to the GWconnectors.
- `Class_StrmGWConnector_v40.f90, Class_StrmGWConnector_v41.f90, Class_StrmGWConnector_v42.f90, Class_StrmGWConnector_v421.f90, Class_StrmGWConnector_v50.f90` </br>
Because the `Set_KH_KV` is an abstract method of the base class where all connectors inherit all versions are required to overload this although in practice it does nothing. (This is something that may need to find better way to implement) 
- `Class_StrmGWConnector.f90` </br>
We have to include the new version of the component and register the abstact method (`Set_KH_KV`). This is the method that selects the approprate version so we included the 4.11 version under the SELECT CASE statements.
 - `Package_Model.f90` </br>
 The Package Model class is responsible to initialize all the groundwater components. Safe requires the hydraulic conductivity which is set when the Groundwater component is setup. However the Stream groundwater connector component is initialized before the groundwater component. Therefore an extra step is added which supplies the conductiivties to the stream nodes. 
 - `Class_AppGW.f90` </br>
 Added a method to read the horizontal and vertical conductivity for a specific node and layer, so that we dont have to extract all values.

 *If the programs has trouble compiling it may require a clean rebuild*

## Implementation of SAFE in 4.11
The stream component 4.11 inherits all the functionality of the base class.
However the starting point of the 4.11 version is the 4.1 version. Therefore the code was built around the 4.1 implementation of `Simulate` and `CompileConductance`. </br>
In addition we included a number of member variables that are used in SAFE.These are:
- Wsafe : The width calculated from safe
- Gsafe 
- LayerBottomElevation : This is the elevation of the layer that is going to be treated as bottom for the SAFE. (Currently was are using 1 layer aquifer)
- Kh : Horizontal hydraulic conductivity
- Kv : Vertical hydraulic conductivity
- e_cl : Thickness of the streambed
- K_cl : COnductivity of streambed material
- L : Representative length for each node
- CondTemp : This is used to implicitly get the time conversion factor for the conductivity.




