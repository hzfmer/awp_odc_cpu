echo 'building awp...'
make clean
make
mkdir $1
pushd $1
mkdir output_sfc
mkdir output_ckp
mkdir -p input_rst/srcpart/split_faults
mkdir -p input_rst/srcpart/part_faults
mkdir -p input_rst/srcpart/tpsrc
ln -s ~/projects/qf_cpu/pmcl3d .
cp ../IN3D .
popd 
