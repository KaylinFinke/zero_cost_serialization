cmake_minimum_required(VERSION 3.16 FATAL_ERROR)
project(zero_cost_serialization)

option(ZERO_COST_SERIALIZATION_BUILD_EXAMPLES "Build example usages of zero_cost_serialization" ON)
option(ZERO_COST_SERIALIZATION_BUILD_TOOLS "Build zero_cost_serialization tools" ON)
option(ZERO_COST_SERIALIZATION_BUILD_TESTS "Build tests for zero_cost_serialization" ON)

if(PROJECT_IS_TOP_LEVEL)
    set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
    set(CMAKE_CXX_CLANG_TIDY clang-tidy; -header-filter=.; -checks=*;)
endif(PROJECT_IS_TOP_LEVEL)

set(ZERO_COST_SERIALIZATION_ROOT_DIR ${CMAKE_CURRENT_LIST_DIR})

set(ZERO_COST_SERIALIZATION_INCLUDE_DIR ${ZERO_COST_SERIALIZATION_ROOT_DIR}/include/zero_cost_serialization)
set(ZERO_COST_SERIALIZATION_SOURCE_DIR ${ZERO_COST_SERIALIZATION_ROOT_DIR}/src)
set(ZERO_COST_SERIALIZATION_LIBRARY_SOURCE
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/map.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/list.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/apply.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/bitfield.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/generated.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/interchange_float.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/invoke.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/reinterpret_memory.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/strict_alias.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/serializable.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/detail/link.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/detail/error.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/nullable_resource.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/scope_guard.h
    ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/constraint.h
    )

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    set(ZERO_COST_SERIALIZATION_LIBRARY_SOURCE ${ZERO_COST_SERIALIZATION_LIBRARY_SOURCE} ${ZERO_COST_SERIALIZATION_ROOT_DIR}/clang/zero_cost_serialization/detail/warning.h)
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(ZERO_COST_SERIALIZATION_LIBRARY_SOURCE ${ZERO_COST_SERIALIZATION_LIBRARY_SOURCE} ${ZERO_COST_SERIALIZATION_ROOT_DIR}/msvc/zero_cost_serialization/detail/warning.h)
else()
    set(ZERO_COST_SERIALIZATION_LIBRARY_SOURCE ${ZERO_COST_SERIALIZATION_LIBRARY_SOURCE} ${ZERO_COST_SERIALIZATION_ROOT_DIR}/compiler/zero_cost_serialization/detail/warning.h)
endif()

if(CMAKE_CXX_COMPILER_FRONTEND_VARIANT STREQUAL "MSVC")
    set(ZERO_COST_SERIALIZATION_LIBRARY_SOURCE ${ZERO_COST_SERIALIZATION_LIBRARY_SOURCE} ${ZERO_COST_SERIALIZATION_ROOT_DIR}/windows/zero_cost_serialization/detail/platform.h)
else()
    set(ZERO_COST_SERIALIZATION_LIBRARY_SOURCE ${ZERO_COST_SERIALIZATION_LIBRARY_SOURCE} ${ZERO_COST_SERIALIZATION_ROOT_DIR}/platform/zero_cost_serialization/detail/platform.h)
endif()

add_library(zero_cost_serialization INTERFACE ${ZERO_COST_SERIALIZATION_LIBRARY_SOURCE})
target_include_directories(zero_cost_serialization INTERFACE ${ZERO_COST_SERIALIZATION_INCLUDE_DIR}/..)
target_compile_features(zero_cost_serialization INTERFACE cxx_std_20)

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_include_directories(zero_cost_serialization INTERFACE ${ZERO_COST_SERIALIZATION_ROOT_DIR}/clang)
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    target_include_directories(zero_cost_serialization INTERFACE ${ZERO_COST_SERIALIZATION_ROOT_DIR}/msvc)
else()
    target_include_directories(zero_cost_serialization INTERFACE ${ZERO_COST_SERIALIZATION_ROOT_DIR}/compiler)
endif()

if(CMAKE_CXX_COMPILER_FRONTEND_VARIANT STREQUAL "MSVC")
    target_include_directories(zero_cost_serialization INTERFACE ${ZERO_COST_SERIALIZATION_ROOT_DIR}/windows)
else()
    target_include_directories(zero_cost_serialization INTERFACE ${ZERO_COST_SERIALIZATION_ROOT_DIR}/platform)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    add_compile_options(-Wall
                        -Wextra
                        -pedantic)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_compile_options(-Weverything
                        -Wno-c++98-compat           #-Wc++98-compat library requires C++20.
                        -Wno-c++98-compat-pedantic  #-Wc++98-compat-pedantic library requires C++20.
                        -Wno-c++20-compat           #-Wc++20-compat library requires C++20.
                        -Wno-padded                 #-Wpadded emits a warning even when the structure cannot be reordered to eliminate internal padding.
                        -Wno-switch-default         #-Wswitch-default emits a warning for nod default even when the default case is unreachable
    )
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    add_compile_options(/Wall
                        /wd4820                     # 'bytes' bytes padding added after construct 'member_name'
                        /wd5027                     # 'type': move assignment operator was implicitly defined as deleted
                        /wd4625                     # 'type' : copy constructor was implicitly defined as deleted
                        /wd4626                     # 'derived class' : assignment operator was implicitly defined as deleted because a base class assignment operator is inaccessible or deleted
                        /wd5045                     # Compiler will insert Spectre mitigation for memory load if /Qspectre switch specified
                        /wd4711                     # function 'function' selected for inline expansion
                        /wd4710                     # 'function' : function not inlined
                        /wd4868                     # compiler may not enforce left-to-right evaluation order in braced initializer list
    )
endif()

#############################
#Build Tools
#############################
if(${ZERO_COST_SERIALIZATION_BUILD_TOOLS})
    set(ZERO_COST_SERIALIZATION_TOOLS_DIR ${ZERO_COST_SERIALIZATION_ROOT_DIR}/tools)
    add_executable(zero_cost_serialization_codegen ${ZERO_COST_SERIALIZATION_TOOLS_DIR}/codegen.cpp)
    target_compile_features(zero_cost_serialization_codegen PUBLIC cxx_std_20)
endif(${ZERO_COST_SERIALIZATION_BUILD_TOOLS})

#############################
#Build Examples
#############################
if(${ZERO_COST_SERIALIZATION_BUILD_EXAMPLES})
    set(ZERO_COST_SERIALIZATION_EXAMPLES_DIR ${ZERO_COST_SERIALIZATION_ROOT_DIR}/examples)
    add_executable(zero_cost_serialization_simple_rpc ${ZERO_COST_SERIALIZATION_EXAMPLES_DIR}/simple_rpc.cpp)
    target_link_libraries(zero_cost_serialization_simple_rpc PUBLIC zero_cost_serialization)
    target_compile_features(zero_cost_serialization_simple_rpc PUBLIC cxx_std_20)
endif(${ZERO_COST_SERIALIZATION_BUILD_EXAMPLES})

#############################
#Build Tests
#############################
if(${ZERO_COST_SERIALIZATION_BUILD_EXAMPLES})
    set(ZERO_COST_SERIALIZATION_TESTS_DIR ${ZERO_COST_SERIALIZATION_ROOT_DIR}/tests)
    add_executable(zero_cost_serialization_tests ${ZERO_COST_SERIALIZATION_TESTS_DIR}/tests.cpp)
    target_link_libraries(zero_cost_serialization_tests PUBLIC zero_cost_serialization)
    target_compile_features(zero_cost_serialization_tests PUBLIC cxx_std_20)
    if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
        target_compile_options(zero_cost_serialization_tests PUBLIC -Wno-ignored-qualifiers)
    elseif(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
        target_compile_options(zero_cost_serialization_tests PUBLIC /wd5266 /wd4514)
    endif()
endif(${ZERO_COST_SERIALIZATION_BUILD_EXAMPLES})
