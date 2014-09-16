# Glasgow Model Coupling Framework

In time, this will become a radically new way to couple simulation models ...

## Aim

Model Coupling is increasingly important in field such as climate science: individual models are already hugely complicated, so a conventional integration by merging the code bases would require a very large effort by a highly specialised team. Instead, exchanging information between running models is a more practical approach.

The aim of GMCF is to make Model Coupling easier and more suited to modern heterogeneous manycore architectures. Our approach is to use modern language, compiler and runtime technologies so that in the end the user only has to write a scenario to realise the coupling. This is a long-term goal (many years).

# Current Prototype

The code in this repository is a Proof-of-Concept: I created the Glasgow Model Coupling Framework (GMCF), and API and runtime, and used it to couple the Weather Research and Forecasting Model with the Large Eddy Simulator developed at Kyoto University's Disaster Prevention Research Institute. The current coupling code works on a single multicore node with single-threaded WRF and LES. It should work with multithreaded code (WRF+OpenMP) and even e.g. OpenCL-based GPGPU code, but I have not tested that.

The GMCF is already very generic and can be used for coupling other models as well, but it is not yet easy to use. 

## Architecture

GMCF consists of a run-time thread pool with FIFO communication, based on my GPRM framework (formerly called Gannet, https://github.com/wimvanderbauwhede/GannetCode). On top of this, I added a Fortran-C++ integration code generator and the actual model coupling API. This API has three levels and ultimately accesses the GPRM API. 

### Model-specific API

The user-level API is specialised per model. Currently this is done manually, eventually it will be generated. The API has very few calls, the naming convention is gmcf<call name><model name>in camelcase. For example for WRF the API calls would be gmcfInitWrf, gmcfSyncWrf, gmcfPreWrf, gmcfPostWrf. the Init call initialises GMCF for the model. The Sync call synchronises the model with the other models it's communicating with. The Pre and Post calls are taking care of the actual data exchange, Pre is before computation, Post is after computation.  

### Fortran API

The model-specific API calls underlying gmcf API which has many more fine-grained calls for synchronisation and data exchange. This API is entirely accessible to the user. (Although our ultimate aim is that the user will never have to write a line of code).

### C++ API

The Fortran API is just a wrapper around the C++ API. This API is not really intended for actual use, its purpose is to build the next-level API. So if your model is in C or C++ rather than Fortran, you'd want to create a C/C++ equivalent of `gmcfAPI.f95`.

## Requirements

- This code works on Mac OS X (anything from Snow Leopard on) and on Linux.
- You need a C++11- capable C++ compiler, I use g++ 4.8 or 4.9
- You need a moder Fortran compiler, I use gfortran 4.8 or 4.9 but recent Intel Fortran or Portland Group Fortran compilers will work too; you may need to change the options in the build scripts.
- The build scripts use Perl, Ruby and Python, and in particular SCons v2.3 so it's best to use Python 2.7 although 2.6.6 works fine.

## Installation

- After pulling the repository, you need to set the environment GANNET_DIR to the path of the gmcf folder, e.g. `$HOME/gmcf`.
- In the gmcf folder, you need to source the etc/gannetrc file: `. etc/gannetrc`
- You need to set the $CC, $FC and $CXX environment variables for the C, Fortran and C++ compilers.

## Making it work with other models

I will write this out more in full, but in short: 

- Put your models in `t/ModelCoupling/src/<your src folder>`
- Change your toplevel program into a subroutine, have a look at `t/ModelCoupling/src/GMCF/Models/model1.f95` as an example. NOTE: at the moment you  _must_ call the subroutine `program_<your_name>`, and <your_name> will be the name of your model.
- Create the API module for your model, again have a look at `t/ModelCoupling/src/GMCF/Models/gmcfAPImodel1.f95`.
- Use the API in your model.
- Change your build script so that, instead of building a program, it builds a static library containing the toplevel subroutine.
- Change `t/ModelCoupling/Sconstruct` for your models, mainly adding the libraries required by your models.
- Finally, copy the file `t/ModelCoupling/src/GMCF/Task/GMCF.yml or just edit it: under 'Models:', change the name to the name of your model (<your_name>).

Now, if you want to create more complex coupled models, not just producer-consumer, then more work is needed, but I think the code is to immature for this.



