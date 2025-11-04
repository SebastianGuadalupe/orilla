cmake_minimum_required(VERSION 3.22)

set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded")

project(orilla-compiler LANGUAGES CXX)

add_executable (orilla-compiler "orilla-compiler.cpp" "orilla-compiler.h" "tokenizer/tokenizer.cpp" "tokenizer/tokenizer.h" "types/types.h" "parser/parser.cpp" "parser/parser.h" "generator/generator.cpp" "generator/generator.h")

find_package(LLVM CONFIG REQUIRED)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

message(STATUS "LLVM include dirs: ${LLVM_INCLUDE_DIRS}")

llvm_map_components_to_libnames(LLVM_LIBS
        core
        support
        X86
        X86AsmParser
        X86CodeGen
)

target_link_libraries(orilla-compiler PRIVATE ${LLVM_LIBS})

target_compile_definitions(orilla-compiler PRIVATE
    $<$<CONFIG:Debug>:_ITERATOR_DEBUG_LEVEL=0>
)

set_property(TARGET orilla-compiler PROPERTY CXX_STANDARD 20)
