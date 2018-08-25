# Kastri Free Build Projects

The build projects provided here serve two purposes:

1. A means of ensuring that all the source compiles without errors, hints or warnings
2. For "precompiling" source into binaries, to reduce compile time and memory usage when compiling the units into an application

You may note that:

The output directory for the project is configured as:

>`$(BlackHole)`

This is a user-defined variable that you can add via the IDE options in the Environment Variables section, under User Overrides. Add a variable called BlackHole, and assign it a path which is not normally part of a backup, since the files can be discarded.

The Unit output directory is configured as:

>`$(DCUPath)\$(ProdVer)\$(Platform)\$(Config)`

As per BlackHole, you can define DCUPath in the User Overrides, however this time define it as a path to where the compiler can find the binaries when it compiles an application that requires them. The ProdVer variable should be defined as a unique value that relates to the version of Delphi that the DCUs are required for, e.g. I use a value of D250 for Delphi 10.2 Tokyo. Platform and Config are variables that are replaced by the compiler when compiling for the selected platform (e.g. Win32, iOS 64 etc) and config (e.g. Debug or Release)

You may choose to configure the compiler paths in the IDE so that it includes:

>`$(DCUPath)\$(ProdVer)\$(Platform)\$(Config)`

Unfortunately, this needs to be done for each platform, as there is no way to make this path available for all platforms in the IDE options using a single entry. Alternatively, add this path to the project compiler search path on a project-by-project basis.

