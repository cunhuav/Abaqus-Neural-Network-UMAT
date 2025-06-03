# Abaqus Neural Network UMAT

This repository demonstrates how to link a neural network constitutive model with Abaqus through a Fortran `UMAT`. The UMAT implemented in [`ro_nn_umat.f90`](ro_nn_umat.f90) reads pre-trained network weights from data files and uses them to predict stress and stiffness during the analysis.

## Project purpose

The goal is to show a minimal example of coupling Abaqus with a neural network based material model. The Fortran code defines two networks: one predicting the stress tensor and another predicting the consistent tangent matrix. These networks are loaded at the first call to the UMAT and then evaluated at each material integration point.

## Compiling the UMAT

Abaqus can compile the source automatically when launching a job:

```bash
abaqus job=UnixialTensionNN user=ro_nn_umat.f90
```

Alternatively, compile the Fortran file into a shared library:

```bash
gfortran -fPIC -shared -O2 ro_nn_umat.f90 -o ro_nn_umat.so
```

Then run the job using the compiled library:

```bash
abaqus job=UnixialTensionNN user=ro_nn_umat.so
```

Replace `gfortran` with your compiler of choice (for example `ifort`) if needed.

## Data files

- `RO_stress.dat` – parameters of the neural network used to predict stress.
- `RO_Ct.dat` – parameters of the neural network used to predict the tangent stiffness matrix.
- `UnixialTensionNN.inp` – Abaqus input deck that exercises the UMAT in a uniaxial tension test.
Both `.dat` files start with the input/output sizes and normalization ranges followed by the layer definitions (number of layers, weights, and biases). They are read automatically inside `ro_nn_umat.f90` when the UMAT is initialized.

## Known Issues and Future Work
At present, I have observed during actual use that the neural network (NN) accumulates error, i.e., exhibits exposure error.

## License

This project is distributed under the [MIT License](LICENSE). Please keep the original copyright notice:

```
Copyright (c) 2025 cunhuav
```
