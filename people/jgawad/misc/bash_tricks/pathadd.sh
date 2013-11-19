#/bin/bash
# $Rev$

# Add path to a colon-separated list of values in VARIABLE, either by appending or prepending it.
#
# $1 : path 
# $2 : name of the variable (e.g. PATH, MANPATH, LD_LIBRARY_PATH, PYTHONPATH etc.)
# $3 : empty or "after". Unless "after" is used, the path will be prepended to the list.
#
# The function is roughly based on "pathmunge"
pathadd () {
    local dir=$1
    local var=$2
    local pos=$3
    # Use a sort of "indirect reference" 
    case ":${!var}:" in
        *:"$dir":*)
            ;;
        *)
            if [ "$pos" = "after" ] ; then
                eval ${var}=${!var}:$dir
            else
                eval ${var}=$dir:${!var}
            fi
    esac
}

