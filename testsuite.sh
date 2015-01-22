#/bin/bash

printLine() {
    printf -- '---------------> '
    printf "$1"
    echo " <---------------"
}

# build compiler itself
build_compiler=false
# compile tests with java compiler
build_tests=false
# test haskell compiler
test_compiler=false

# parse arguments
for arg in "$@"; do
    if [ "$arg" == "make" ]; then
        build_compiler=true
    elif [ "$arg" == "javac" ]; then
        build_tests=true
    elif [ "$arg" == "test" ]; then
        test_compiler=true
    fi
done

if [ $build_compiler = true ] && [ ! -f "makefile" ]; then
    echo "-> Makefile not found. Abort"
    exit 1
fi

if [ ! -f "testsuite/order.txt" ]; then
    echo "-> order.txt not found"
    exit 1
fi

printLine "TESTSUITE RUNNING"
echo "-> Paramters Overview:"
echo "Building Compiler = $build_compiler"
echo "Building Tests With Java Compiler = $build_tests"
echo "Building Tests With Haskell Compiler = $test_compiler"
echo ""

if [ $build_compiler = true ]; then
    printLine "BUILDING COMPILER"
    echo "-> building compiler..."
    make
    if [ $? -eq 0 ]; then
        echo "-> compilation finished"
        echo ""
    else
        echo "-> could not build compiler. Abort."
        exit 1
    fi
fi

if [ $build_tests = true ]; then
    printLine "BUILDING TESTS WITH JAVA COMPILER"
    while read line; do
        filepath="testsuite/${line}.java"
        javac $filepath
        if [ ! $? -eq 0 ]; then
            echo "-> failed to compile ${line}.java"
            exit 1
        fi
    done < "testsuite/order.txt"
    echo "-> compilation of all files successful"
    echo ""
fi

if [ $test_compiler = true ]; then
    printLine "TESTING HASKELL COMPILER"
    while read line; do
        if [[ "$line" == //* ]]; then
            echo "!!-> ignoring ${line}.java"
            continue
        fi
        echo "-> testing ${line}.java"
        ./Main "testsuite/${line}.java" > /dev/null
        if [ $? -eq 0 ]; then
            echo "-> successful"
        else
            echo "-> an error occurred"
            exit 1
        fi
    done < "testsuite/order.txt"
    echo "All tests successful"
    exit 0
fi
