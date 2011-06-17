#!/bin/bash

# Usage: ./generate.sh plop
#  ...where plop is a file containing the signature of all required methods.
#  Creates stub files for all classes with methods in plop. WILL OVERRIDE
#  EXISTING FILES.

plopFile=${1:?Error: plop file expected as first arg}

# We first collect all classes for which we want a definition.
classListFile="./plop-classes"
cat ${plopFile} | sed -e "s/\.[^\.]*(.*$//g" | sort | uniq > ${classListFile}

for line in $(cat ${classListFile}); do
    dots=${line}
    nodots=`echo ${dots} | sed -e "s/\.//g"`
    of="${nodots}.scala"

    rm -f ${of}
    echo "package insane" >> ${of}
    echo "package predefined" >> ${of}
    echo "" >> ${of}
    echo "import annotations._" >> ${of}
    echo "" >> ${of}
    echo "@AbstractsClass(\"${dots}\")" >> ${of}
    echo "class ${nodots} {" >> ${of}

    for methFullSig in $(cat ${plopFile} | sed -e "s/ //g" | grep "^${dots}"); do
        echo -n "  ${methFullSig}";

        methName=`echo ${methFullSig} | sed -e "s/[^(]*\.\([^\.(]*\)(.*$/\1/"`
        retType=`echo ${methFullSig} | sed -e "s/.*)\(.*\))$/\1/"`
        formalBlock=`echo ${methFullSig} | sed -e "s/[^(]*((\(.*\))[^()]*)$/\1/" | sed -e "s/,/, /g" | sed -e "s/:/ : /g" | sed -e "s/\\\\\$//g"`

        if [ "<init>" == ${methName} ]; then
            cleanMethName="PLOPINIT"
        else
            cleanMethName=${methName}
        fi

        etienneFriendlySig=`echo ${methFullSig} | sed -e "s/,/, /g"`

        case ${retType} in
            Boolean)
                retVal="true"
                ;;
            Char)
                retVal="'0'"
                ;;
            Int)
                retVal="0"
                ;;
            Byte)
                retVal="0"
                ;;
            Short)
                retVal="0"
                ;;
            Long)
                retVal="0L"
                ;;
            Double)
                retVal="0.0d"
                ;;
            Float)
                retVal="0.0f"
                ;;
            Unit)
                retVal="()"
                ;;
            java.lang.String)
                retVal="\"\""
                ;;
            *)
                retVal="new ${retType}()"
                ;;
        esac

        echo "  @AbstractsMethod(\"${etienneFriendlySig}\")" >> ${of}
        echo "  def ${cleanMethName}(${formalBlock}) : ${retType} = { ${retVal} }" >> ${of}
        echo "" >> ${of}

        echo " --> $retVal"
    done

    echo "}" >> ${of}
done

rm -f ${classListFile}
echo "Done."
