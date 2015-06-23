# Glasgow Many-Core Framework

To use GMCF for multithreading, you need to do the following:

- change program to subroutine + use gmcf, automatic with script
- change static to dynamic allocation,  automatic with script with some constraints: 
	- you need to do this per file in your code
	- if a subroutine takes arguments, then the allocation should stay as it was for the arguments. Only local arrays must be dynamically allocated. 
NOTE: We should do this with rf4a to make sure it is robust.

- Once this is done, your code is ready for GMCF, you can use the GMCF API to exchange data between the threads, perform synchronisation etc.
- To compile your code: 
	- first you need to build the GMCF framework code with `build_gmcf`
	- then you can build your own code as you like, but is must be compiled into a library not an executable
	- then you can build the toplevel executable using the provided SConstruct.gmcf file
	



