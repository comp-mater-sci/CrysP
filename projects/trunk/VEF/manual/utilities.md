

Utilities                                                        {#page_utilities}
=========
[TOC]


cur2c                                                            {#cur2c}
=====


Convert discrete ODF produced by the VEF into continuous ODF


    usage: cur2c [-h] --input INPUT [--block BLOCK] --output OUTPUT
                 [--symmetry {orthorhombic,triclinic}]
    
    Calculate continuous C-coefficient ODF from discrete CUR data
    
    optional arguments:
      -h, --help            show this help message and exit
      --input INPUT, -i INPUT
                            Input CUR file
      --block BLOCK         Index of the block inside CUR file (indexed from 1).
                            If omitted, all blocks of the CUR file will be
                            processed.
      --output OUTPUT, -o OUTPUT
                            output C-coefficient file
      --symmetry {orthorhombic,triclinic}
                            Symmetry class to be imposed in the output
                            C-coefficient ODF



odf2odf                                                          {#odf2odf}
=======


Covert between various ODF representations.


    usage: odf2odf [-h] --input INPUT --output OUTPUT
    
    Convert between different file representations of ODF. Currently C-coefficient
    (.C), AODF (.aodf) and MTEX (.odf) file formats are supported.
    
    optional arguments:
      -h, --help            show this help message and exit
      --input INPUT, -i INPUT
                            Input ODF file
      --output OUTPUT, -o OUTPUT
                            Output ODF file



odf2smt                                                          {#odf2smt}
=======


Discretize ODF into a form suitable for the VEF


    usage: odf2smt [-h] --input INPUT [--output OUTPUT] [--ncrystals NCRYSTALS]
    
    Calculate discrete representative crystal orientations from ODF file
    
    optional arguments:
      -h, --help            show this help message and exit
      --input INPUT, -i INPUT
                            ODF file, which can be either a C-coefficient file,
                            AODF file, or MTEX odf file.
      --output OUTPUT, -o OUTPUT
                            output SMT file. If omitted, the name of input file
                            with extension .smt will be used.
      --ncrystals NCRYSTALS
                            Number of crystals in the SMT file



odfcompare                                                       {#odfcompare}
==========


Quantitatively compare ODFs


    usage: odfcompare [-h] --input INPUTS [INPUTS ...] --output OUTPUT
    
    compare several ODFs given as C-coefficient files
    
    optional arguments:
      -h, --help            show this help message and exit
      --input INPUTS [INPUTS ...], -i INPUTS [INPUTS ...]
                            C-coefficient ODF files
      --output OUTPUT, -o OUTPUT
                            output file



odfcompose                                                       {#odfcompose}
==========


Create artificial ODF, either from scratch or by modifying an existing ODF


    usage: odfcompose [-h] --input INPUT --output OUTPUT
                      [--adjust COMPONENT_NAME CORRECTION]
                      [--set COMPONENT_NAME VALUE] [--keep COMPONENT_NAME]
    
    Compose synthetic textures
    
    optional arguments:
      -h, --help            show this help message and exit
      --input INPUT, -i INPUT
                            C-coefficient ODF files
      --output OUTPUT, -o OUTPUT
                            output file
      --adjust COMPONENT_NAME CORRECTION
                            Modify texture component (strengthen/diminish): Two
                            parameters are needed: COMPONENT_NAME VALUE ' VALUE
                            can be positive or negative, either a number or
                            percentage'
      --set COMPONENT_NAME VALUE
                            Set volume of texture component COMPONENT_NAME to
                            VALUE
      --keep COMPONENT_NAME
                            Try to retain texture component COMPONENT_NAME



odfideal                                                         {#odfideal}
========


List ODF texture components available


    usage: odfideal [-h] --list
    
    Ideal texture components
    
    optional arguments:
      -h, --help  show this help message and exit
      --list, -l  list available texture components



odfmerge                                                         {#odfmerge}
========


Merge ODFs


    usage: odfmerge [-h] --input {ODF_FILE,COMPONENT_NAME} WEIGHT --output OUTPUT
                    --assume {grid,continuous} --title TITLE
    
    Merge several ODFs
    
    optional arguments:
      -h, --help            show this help message and exit
      --input {ODF_FILE,COMPONENT_NAME} WEIGHT, -i {ODF_FILE,COMPONENT_NAME} WEIGHT
                            Input ODF to be merged. The WEIGHT specifies its
                            relative contribution to the output. The WEIGHT is
                            automatically normalized. The ODF is either path to
                            ODF file or texture component name.
      --output OUTPUT, -o OUTPUT
                            output ODF file
      --assume {grid,continuous}
      --title TITLE         Title to be included in the output ODF



odfmodify                                                        {#odfmodify}
=========


Make basic modifications to ODF


    usage: odfmodify [-h] --input INPUT --output OUTPUT [--title TITLE]
                     [--symmetry {orthorhombic,triclinic}]
                     [--rotation phi_1 Phi phi_2]
    
    Modify the ODF. Currently only C-coefficient files are supported.
    
    optional arguments:
      -h, --help            show this help message and exit
      --input INPUT, -i INPUT
                            input C-coefficient file
      --output OUTPUT, -o OUTPUT
                            output C-coefficient file
      --title TITLE         Title to be included in the output file
      --symmetry {orthorhombic,triclinic}
                            Title to be included in the output ODF
      --rotation phi_1 Phi phi_2
                            rotation by three Euler angles in Bunge convention:
                            phi_1 Phi phi_2 given in degrees



odfplot                                                          {#odfplot}
=======


Plot Orientation Distribution Function (ODF)


    usage: odfplot [-h] --input INPUT [INPUT ...] [--output OUTPUT [OUTPUT ...]]
                   [--format {pdf,png,svg,ps,eps} [{pdf,png,svg,ps,eps} ...]]
                   [--bw]
                   [--levelset {auto,autolocal,mtm6.4,mtm44,mtm12,mtm16,mtm32,mtm22} | --levels LEVELSET [LEVELSET ...]]
                   [--odfmin ODFMIN] [--odfmax ODFMAX] [--nlevels NLEVELS]
                   [--no-labels] [--sections SECTIONS]
                   [--style {contour,filled,MTMcontour}] [--no-colorbar] [--title]
    
    Print ODF sections
    
    optional arguments:
      -h, --help            show this help message and exit
      --input INPUT [INPUT ...], -i INPUT [INPUT ...]
                            ODF file, which is one of: C-coefficient file, aodf
                            file or mtex file.
      --output OUTPUT [OUTPUT ...], -o OUTPUT [OUTPUT ...]
                            output graphic file
      --format {pdf,png,svg,ps,eps} [{pdf,png,svg,ps,eps} ...]
                            format of the output file
      --bw                  request black-and-white plot
      --levelset {auto,autolocal,mtm6.4,mtm44,mtm12,mtm16,mtm32,mtm22}
                            Selection of level sets
      --levels LEVELSET [LEVELSET ...]
                            ODF levels at which isolines are drawn
      --no-labels
      --sections SECTIONS
      --style {contour,filled,MTMcontour}
                            Style of ODF plot: contour or filled
      --no-colorbar         Switch off the colorbar in filled plots
      --title               Switch on title on the plot



odfvolfrac                                                       {#odfvolfrac}
==========


Calculate volume fractions of texture components


    usage: odfvolfrac [-h] --input INPUTS [INPUTS ...] --output OUTPUT
                      --components COMPONENTS [COMPONENTS ...]
    
    Compute volume fractions of texture components
    
    optional arguments:
      -h, --help            show this help message and exit
      --input INPUTS [INPUTS ...], -i INPUTS [INPUTS ...]
                            input C-coefficient files
      --output OUTPUT, -o OUTPUT
                            output file
      --components COMPONENTS [COMPONENTS ...]
                            List of texture components or "ALL" (see odfideal
                            --list for the names of the available texture
                            components)

