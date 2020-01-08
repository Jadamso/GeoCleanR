#!/bin/bash
## chmod +x $HOME/Desktop/R_Code/GeoCleanR/HHIsub.sh

while getopts 'm:w:q:c:n:g:f:y:o:' opt
do
    case "${opt}" in
        ## Resource Specification
        m) mem="${OPTARG}";;
        w) walltime="${OPTARG}";;
        q) que="-q ${OPTARG}";;
        c) cores="${OPTARG}";;
        n) nodes="${OPTARG}";;
        ## HHI Parameters
        g) gdir="${OPTARG}";;
        f) fact="${OPTARG}";;
        y) years="${OPTARG}";;
        ## Code Records
        o) outdir="${OPTARG}";;
    esac
done


GDIR="gdir=$gdir"
FACT="fact=$fact"
YEARS="years=$years"

SCRIPT=FACTOR_"$fact"_YEAR_"$years"
BASHSCRIPT="$outdir$SCRIPT".sh

RSCRIPTout="$outdir$SCRIPT".Rout    
BASHSCRIPTout="$outdir$SCRIPT".shout

#------------------------------------------------------------------
##################
# Submit Job 
##################

echo -e "R CMD BATCH --no-save '--args $FACT $YEARS $GDIR' HHIbash.R" | qsub $que \
    -l select=$nodes:ncpus=$cores:mpiprocs=$cores:mem=$mem,walltime=$walltime \
    -e $BASHSCRIPTout \
    -o $BASHSCRIPTout \
    -j oe \
    -N $SCRIPT




