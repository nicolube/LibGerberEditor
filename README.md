# A Lib for manipulating gerber rs274x and excellon dill files

This lib extends upon `MakerPnP/gerber-parser` and adds basic functionality
to manipulate gerber 

## Features
- Moving Layers
- Merging Layers
- Vector Text generation
- Copy Layers
- Step and Repeating Layers
- Unit conversion (need to be verified for mixed [in + mm] files)

## Uses cases
- Writing panelizers
- Makeing and UI for Gerber manipulation
- Any other thing you might think of


Any length provided via the API is in mm so we do not have to guess what is what...

This library is currently functional and mostly tested with KiCad and Autodesk Eagle, 
but still very much WIP so feel free to contribute and help me make something that may last and help others.

### 

### Sample Layouts:

#### test/mobo: https://github.com/opulo-inc/lumenpnp/
license: `CERN-OHL-W v2 license. Full text is available at https://cern.ch/cern-ohl`