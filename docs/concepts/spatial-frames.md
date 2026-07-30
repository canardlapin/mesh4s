# Spatial coordinate frames

`spatial4s.Frame` is a typed coordinate frame. It owns the meaning of points and
vectors used by mesh4s realizations. It is unrelated to
[`frame4s`](https://github.com/canardlapin/frame4s), which is a typed dataframe
library.

The shared spatial contract contains:

- dimensions `D2` and `D3`;
- process-local and caller-supplied persistent frame identity;
- an immutable threaded frame registry;
- explicit alignment of independently restored frames;
- `Point` and `Vec` values owned by one frame;
- coordinate units that support standard length units, dimensionless
  coordinates, and validated custom identifiers;
- extensible coordinate conventions with standard `RAS`, `LPS`, and
  `Unspecified` values; and
- validated affine coefficients without registration policy.

`spatial4s` owns coordinate meaning. `reframe4s` owns typed transformations and
registration policy. `image4s` retains grid identity, lattice indices, shape,
and index-to-frame sampling geometry.
