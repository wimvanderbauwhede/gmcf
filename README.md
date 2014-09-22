# Glasgow Model Coupling Framework

## Background

Model Coupling is increasingly important in fields such as climate science: individual models are already hugely complicated, so a conventional integration by merging the code bases would require a very large effort by a highly specialised team. Instead, exchanging information between running models is a more practical approach.

## Aim

The aim of GMCF is to make Model Coupling easier and more suited to modern heterogeneous manycore architectures. Our approach is to use modern language, compiler and runtime technologies so that in the end the user only has to write a scenario to realise the coupling. This is a long-term goal (many years).

## Current Prototype

The code in this repository is a Proof-of-Concept, in a very early stage: I created the Glasgow Model Coupling Framework (GMCF), an API and runtime, and used it to couple the [Weather Research and Forecasting Model](http://www2.mmm.ucar.edu/wrf/users/) with the [Large Eddy Simulator](https://github.com/wimvanderbauwhede/LES) developed at Kyoto University's Disaster Prevention Research Institute. The current coupling code works on a multicore/GPU node with OpenMP WRF and our OpenCL LES. I have tested the integration on a 12-core Intel Xeon E5-2640 host running CentOS 6.4, with an NVIDIA GeForce GTX 480.

The GMCF is already very generic and can be used for coupling other models as well, but it is not yet easy to use.

## Architecture

GMCF consists of a run-time thread pool with FIFO communication, based on my GPRM framework (formerly called [Gannet](https://github.com/wimvanderbauwhede/GannetCode)). On top of this, I added a Fortran-C++ integration code generator and the actual model coupling API. This API has three levels and ultimately accesses the GPRM API.

### Model-specific API

The user-level API is specialised per model. Currently this is done manually, eventually it will be generated. The API has very few calls, the naming convention is `gmcf`_Call_nameModel_name_ in camelcase. For example for WRF the API calls would be `gmcfInitWrf`, `gmcfSyncWrf`, `gmcfPreWrf`, `gmcfPostWrf`.

- The _Init_ call initialises GMCF for the model.
- The _Sync_ call synchronises the model with the other models it's communicating with.
- The _Pre_ and _Post_ calls are taking care of the actual data exchange, _Pre_ is before computation, _Post_ is after computation.  

### Fortran API

The model-specific API calls the underlying GMCF API which has many more fine-grained calls for synchronisation and data exchange. This API is entirely accessible to the user. (Although our ultimate aim is that the user will never have to write a line of code).

### C++ API

The Fortran API is just a wrapper around the C++ API. This API is not really intended for actual use, its purpose is to build the next-level API. So if your model is in C or C++ rather than Fortran, you'd want to create a C/C++ equivalent of `gmcfAPI.f95`.

## Requirements

- This code works on Mac OS X (anything from Snow Leopard on) and on Linux.
- You need a C++11- capable C++ compiler, I use g++ 4.8 or 4.9
- You need a moder Fortran compiler, I use gfortran 4.8 or 4.9 but recent Intel Fortran or Portland Group Fortran compilers will work too; you may need to change the options in the build scripts.
- The build scripts use Perl, Ruby and Python, and in particular SCons v2.3 so it's best to use Python 2.7 although 2.6.6 works fine.
- You need a compiler with OpenMP support and OpenCL for the device you want to use. I have used gcc's OpenMP, NVIDIA's OpenCL for GPU and Intel's OpenCL for the CPU.

## Installation

- You need to set the environment variable `$GANNET_DIR` to the path of the gmcf folder, e.g. `$HOME/gmcf`.
- In the gmcf folder, you need to source the etc/gannetrc file: `. etc/gannetrc` (or put it in your `.bashrc` or equivalent)
- You need to set the $CC, $FC and $CXX environment variables for the C, Fortran and C++ compilers.
- If you use OpenCL, you need to install (my OclWrapper library)[https://github.com/wimvanderbauwhede/OpenCLIntegration].

- To build the model1/model2 producer-consumer coupling example:

  $ cd $GANNET_DIR/t/ModelCoupling
  $ ./build_gmcf

- And to clean:

  $ ./clean_gmcf  

**NOTE: The WRF/LES sources are not included, and need to be patched.**

- Get WRFV3.4 from http://www2.mmm.ucar.edu/wrf/users/wrfv3.4/updates-3.4.html. Put it anywhere but _not_ in `$GANNET_DIR/t/ModelCoupling/src/WRFV3.4_gmcf`.
- Replace the original files with the modified ones from the GMCF repository, and move the folder to `$GANNET_DIR/t/ModelCoupling/src/WRFV3.4_gmcf`.
- Do the same for LES: get it from https://github.com/wimvanderbauwhede/LES, replace the original files with the modified ones from the GMCF repository and put it under `$GANNET_DIR/t/ModelCoupling/src/LES_F95` or  `$GANNET_DIR/t/ModelCoupling/src/LES_OCL` for the OpenCL version.
- For the OpenCL version you also need to install my [OpenCL wrapper library](https://github.com/wimvanderbauwhede/OpenCLIntegration), and you need to symlink the `OpenCL` folder in `LES_OCL` into `ModelCoupling`:

    $ pwd
    $GANNET_DIR/t/ModelCoupling

    $ ln -s src/LES_OCL/OpenCL .

- To build the WRF/LES producer-consumer coupling example:

    $ cd $GANNET_DIR/t/ModelCoupling
    $ ./build_gmcf_wrf_les

- And to clean:

    $ ./clean_gmcf_wrf_les

- And similar for the OpenCL version, with ./build_gmcf_wrf_les_ocl

- To run the WRF/LES integration:

    $ cd run
    $ ../gmcfCoupler

- You might need to set `$OMP_STACKSIZE` and `$OMP_NUM_THREADS` to run WRF with OpenMP. 


## Making it work with other models

I will write this out more in full, but in short:

- Put your models in `t/ModelCoupling/src/`_your_src_folder_.
- Change your toplevel program into a subroutine, have a look at `t/ModelCoupling/src/GMCF/Models/model1.f95` as an example. NOTE: at the moment you  _must_ call the subroutine `program_`_your_name_, and _your_name_ will be the name of your model.
- Create the API module for your model, again have a look at `t/ModelCoupling/src/GMCF/Models/gmcfAPImodel1.f95`.
- Use the API in your model.
- Change your build script so that, instead of building a program, it builds a static library containing the toplevel subroutine, and installs it in `$GANNET_DIR/t/ModelCoupling/src/GMCF/Models`.
- Change `t/ModelCoupling/Sconstruct` for your models, mainly adding the libraries required by your models.
- Finally, copy the file `t/ModelCoupling/src/GMCF/Task/GMCF.yml or just edit it: under 'Models:', change the name to the name of your model (_your_name_).

Now, if you want to create more complex coupled models, not just producer-consumer, then more work is needed, but I think the code is to immature for this.
