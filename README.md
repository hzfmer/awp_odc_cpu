### awp-odc-cpu
CPU Version of the AWP-ODC 4th order staggered-grid finite difference anelastic wave propagation code..

This might be updated at some future date with a nice little explanation of the code.  Currently just used as a change-log.

Features Implemented:
Q(f)
MPI-IO for reading mesh
MPI-IO for writing seismograms

Change log
- implemented options for homogenous material for mediarestart = 0
- mpi-io for source reading

To-Do
- finish testing and replace mpi-io with ifault=2 option.
- add option for q0 value in attenuation model
